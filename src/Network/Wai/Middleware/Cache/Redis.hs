{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} 

-- | Redis backend for "Network.Wai.Middleware.Cache".
--
--   This backend uses "Database.Redis.Pile" for low-lewel operations. Main 
--   drawback of this backend is the fact that whole response 
--   will be consumed in memory.
--
-- > cache
-- >     (redisBackend 
-- >         -- use defaults, DB 0 and "myprefix" 
-- >         R.defaultConnectInfo 0 
-- >         "myprefix"         -- prefix for caching
-- >         Nothing            -- no expiration
-- >         ["mytag"]          -- one cache tag
-- >         (rawPathInfo)      -- URL path as key 
-- >         lookupETag         -- And find "If-None-Match"
-- >     ) app -- our app

module Network.Wai.Middleware.Cache.Redis (
    -- * Cache backend
    redisBackend,
) where

import Control.Monad.IO.Class (liftIO)
import qualified Control.Arrow as A (first)

import Data.Maybe (fromMaybe)

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromChunks)
import Data.CaseInsensitive (original, mk, CI(..))

import Data.Conduit (ResourceT, runResourceT, ($$), ($=), Flush(..))
import qualified Data.Conduit.List as CL
import Data.Conduit.Blaze (builderToByteStringFlush)

import Network.Wai (Request(..), Response(..), responseSource, responseLBS)
import Network.Wai.Middleware.Cache (CacheBackend)
import Network.HTTP.Types (Status(..))

import qualified Database.Redis as R
import Database.Redis.Pile (pile)

-- | Redis backend for "Network.Wai.Middleware.Cache". 
redisBackend ::
       R.ConnectInfo
            -- ^ Redis connection info.
    -> Integer  
            -- ^ Redis DB Index.
    -> ByteString
            -- ^ Cache prefix for key and tags.  
            --   See "Database.Redis.Pile" for details.
    -> Integer
            -- ^ Cache TTL. Use @Zero (0)@  for no expiration.
    -> [ByteString]
            -- ^ Cache Tags. See "Database.Redis.Pile" for details.
    -> (Request -> ByteString)
            -- ^ Cache key extraction function.
    -> (Request -> Maybe ByteString)
            -- ^ @ETag@ value extraction. To extract @If-None-Match@ header
            --   use 'lookupETag'. Use @(const Nothing)@ for block 
            --   @304@-responses.
    -> CacheBackend
redisBackend cInfo db cachePrefix ttl tags keyFn eTagFn app req = do
    cachedResponse <- liftIO $ do
        conn <- R.connect cInfo
        R.runRedis conn $ do
            _ <- R.select db
            pile cachePrefix key eTag $ liftIO . runResourceT $ do
                res <- app req
                (h, d) <- buildCachedResponse res
                return (d, h, tags, ttl)
    parseCachedResponse cachedResponse
  where
    (key, eTag) = (keyFn req, eTagFn req)

----------------------------------------------------------------------------
-- Internal
----------------------------------------------------------------------------

-- | Cached response
type CachedResponse = (Int, ByteString, 
    [(ByteString, ByteString)], [ByteString])

buildCachedResponse :: Response -> ResourceT IO (ByteString, CachedResponse)
buildCachedResponse res = do
    bodyChunks <- bsrc $= builderToByteStringFlush 
                       $= CL.map fromChunk $$ CL.consume
    let bodyHash = fromMaybe "" $ lookup "etag" hs 
    let stripHeaders = map (A.first original) hs
    return (bodyHash, (sc, sm, stripHeaders, bodyChunks))
  where
    (Status sc sm, hs, bsrc) = responseSource res
    fromChunk (Chunk a) = a
    fromChunk Flush = ""

-- | Parse cached response
parseCachedResponse :: Maybe CachedResponse -> ResourceT IO (Maybe Response)
parseCachedResponse Nothing = return Nothing
parseCachedResponse (Just (sc, sm, hs, bodyChunks)) = do
    let wrappedHeaders = map (A.first mk) hs
    return $ Just $ responseLBS (Status sc sm) 
            wrappedHeaders $ fromChunks bodyChunks



    