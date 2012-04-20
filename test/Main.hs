{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Test.Framework (defaultMain, testGroup, mutuallyExclusive)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Network.Wai (Application, responseLBS)
import Network.Wai.Test
import qualified Network.HTTP.Types as H

import qualified Data.ByteString as B
import Data.Binary
import Data.CaseInsensitive (original, mk, CI(..), FoldCase(..))

import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = defaultMain [
        mutuallyExclusive $ testGroup "Flow" [
            testCase "Binary Case-Insensitive" caseBinaryCI,
            testCase "Just test" caseTestWai
        ]
    ]

instance (FoldCase a, Binary a) => Binary (CI a) where
    get = do
        t <- get
        return $ mk t
    put d = put $ original d
    
caseBinaryCI :: Assertion
caseBinaryCI = do
    let d = mk ("test" :: B.ByteString)
    let db = encode d
    liftIO $ print (decode db :: CI B.ByteString)
    liftIO $ d @=? decode db

caseTestWai :: Assertion
caseTestWai = flip runSession shortApp $ do
    r <- request defaultRequest
    liftIO $ print r

shortApp :: Application
shortApp _ = return $ responseLBS H.ok200 [] "short"