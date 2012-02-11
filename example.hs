{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)

import Data.ByteString.Lazy.Char8 (pack) -- Just for an orphan instance
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Network.Wai

import Network.Wai.Middleware.Cache (cache)
import Network.Wai.Middleware.Cache.Redis (redisBackend, lookupETag)

import qualified Database.Redis as R


main :: IO ()
main = do
    putStrLn $ "http://localhost:8888/"
    run 8888 $ cache 
                    (redisBackend R.defaultConnectInfo 0 "myprefix"
                        (const Nothing) (const ["ex"]) (rawPathInfo) 
                        lookupETag) 
             $ route 
               app

route :: Middleware
route app' req = case rawPathInfo req of
    "/cached" -> app' req
    _ -> return $ responseLBS status200 [] ""

app :: Application
app _ = do
    liftIO $ print "I'm working"
    return $ responseLBS 
        status200 [("Content-Type", "text/plain")] "OK"