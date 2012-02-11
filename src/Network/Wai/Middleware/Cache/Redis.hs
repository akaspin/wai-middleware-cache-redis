{-# LANGUAGE OverloadedStrings #-} 

-- | Redis backend for "Network.Wai.Middleware.Cache".
--
--   This backend uses "Database.Redis.Pile" for low-lewel operations.
--
-- > cache
-- >     (redisBackend 
-- >         -- use defaults, DB 0 and "myprefix" 
-- >         R.defaultConnectInfo 0 "myprefix"
-- >         (const Nothing)    -- no expiration
-- >         (const ["mytag"])  -- simply one tag "mytag"
-- >         (rawPathInfo)      -- URL path as key 
-- >         lookupETag         -- And find "If-None-Match"
-- >     ) app -- our app

module Network.Wai.Middleware.Cache.Redis (
    -- * Cache backend
    redisBackend,
    -- * Helpers
    lookupETag
) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Arrow as A (first)

import Data.Maybe (fromJust)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (fromChunks)
import Data.CaseInsensitive (original, mk)
import qualified Data.Serialize as S

import Data.Conduit (ResourceT, runResourceT, ($$), ($=), Flush(..))
import qualified Data.Conduit.List as CL
import Data.Conduit.Blaze (builderToByteStringFlush)

import qualified Crypto.Hash.SHA1 as SHA
import Data.Hex (hex)

import Network.Wai (Request(..), Response(..), responseSource, responseLBS)
import Network.Wai.Middleware.Cache (CacheBackend)
import Network.HTTP.Types (Status(..))

import qualified Database.Redis as R
import Database.Redis.Pile (pile)

-- | Redis backend for "Network.Wai.Middleware.Cache". 
--
--   Except caching, this backend always adds @ETag@ to 'Response' headers 
--   with hexed @SHA1@ as value.
redisBackend ::
       R.ConnectInfo
            -- ^ Redis connection info.
    -> Integer  
            -- ^ Redis DB.
    -> B.ByteString
            -- ^ Cache prefix for key and tags.  
            --   See "Database.Redis.Pile" for details.
    -> (Request -> Maybe Integer)
            -- ^ TTL extraction. Use 'Nothing' for no expiration.
    -> (Request -> [B.ByteString])
            -- ^ Tags extraction. 
            --   See "Database.Redis.Pile" for details.
    -> (Request -> B.ByteString)
            -- ^ Key extraction.
    -> (Request -> Maybe B.ByteString)
            -- ^ @ETag@ value extraction. To extract @If-None-Match@ header
            --   use 'lookupETag'. Use @(const Nothing)@ for block 
            --   @304@-responses.
    -> CacheBackend
redisBackend cInfo db cachePrefix ttlFn tagsFn keyFn eTagFn app req = do
    rawRes <- liftIO $ do
        conn <- R.connect cInfo
        R.runRedis conn $ do
            void $ R.select db
            pile cachePrefix key eTag $ runResourceT $ do
                res <- app req
                case res of
                    ResponseFile{} -> undefined
                    _ -> do 
                        d <- parseResponse res
                        return (d, ttl, tags)
    return $ buildResponse rawRes 
  where
    (ttl, tags, key) = (ttlFn req, tagsFn req, keyFn req)
    eTag = case eTagFn req of
        Nothing -> Nothing
        Just v -> Just ("header:ETag", v)

-- | Helper for extract @If-None-Match@ header from 'Request'.
lookupETag :: Request -> Maybe B.ByteString
lookupETag = lookup "If-None-Match" . requestHeaders

----------------------------------------------------------------------------
-- Internal
----------------------------------------------------------------------------

buildResponse :: Maybe [(B.ByteString, B.ByteString)] -> Maybe Response
buildResponse Nothing = Nothing
buildResponse (Just raw) = 
    Just $ responseLBS (Status sc sm) hs body
  where
    rawResp = fromJust . lookup "response" $ raw
    (sc, sm, hs, body) = case S.decode rawResp of
        Left sm' -> (500, BS8.pack sm', [], "")
        Right (sc', sm', hs', bodyChunks) ->
            (sc', sm', map (A.first mk) hs', fromChunks bodyChunks)

        
parseResponse :: Response -> ResourceT IO [(B.ByteString, B.ByteString)]
parseResponse res = do
    bodyChunks <- b $= builderToByteStringFlush 
                    $= CL.map fromChunk $$ CL.consume
    let bodyHash = hex . SHA.finalize . foldl SHA.update SHA.init $ bodyChunks
    return [("response", 
                S.encode (sc, sm, map (A.first original) hs, bodyChunks)),
            ("header:ETag", bodyHash)]  
  where
    (Status sc sm, hs, b) = responseSource res
    fromChunk (Chunk a) = a
    fromChunk Flush = ""
    
    