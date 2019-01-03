{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Network.AWS
import System.Process.Typed
import System.IO
import Network.AWS.Route53.ChangeResourceRecordSets
import Network.AWS.Route53.Types
import Data.List.NonEmpty
import qualified Data.ByteString.Lazy as B
import Data.Text.Encoding as E
import Data.Semigroup ((<>))
import Options.Generic

b :: Text -> Text -> Text -> ChangeResourceRecordSets
b rid domain ip = changeResourceRecordSets (ResourceId rid) (changeBatch $ (change Upsert $ resourceRecordSet domain A & rrsTTL .~ Just 60 & rrsResourceRecords .~ (Just (pure (resourceRecord ip)))) :| [])

f s = do
  lgr <- newLogger Debug stdout
  env <- newEnv Discover
  runResourceT $ runAWS (env & envLogger .~ lgr) $ within Tokyo $ send (b (rid s) (domain s) (ip s))
        
main :: IO ()
main = do
  s <- getRecord ""
  a <- f s
  print a

data Sample = Sample
  { rid      :: Text
  , domain :: Text
  , ip :: Text } deriving Generic

instance ParseRecord Sample
