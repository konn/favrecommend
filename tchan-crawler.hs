{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Conduit
import qualified Data.Conduit.List as LC
import qualified Data.Text as T
import qualified Data.Attoparsec as A
import Data.Aeson
import Network.HTTP.Conduit hiding (host)
import Web.Authenticate.OAuth.IO hiding (insert, delete)
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import Instances
import Database.MongoDB hiding (Value)
import Prelude hiding (lookup)
import Control.Applicative
import qualified Data.Bson as Bson
import qualified Data.UString as US
import MyConduits

import Tokens

main :: IO ()
main = do
  req <- signOAuth twitter konn =<< parseUrl "https://userstream.twitter.com/2/user.json"
  ch <- liftIO newTChanIO
  (withManager $ http req >=> myOp ch) `finally` do
         putStrLn "connetion closed. waiting for 10 secs..."
         threadDelay (10*10^6)
         putStrLn "reconnecting..."
  runResourceT $ sourceTChan ch $$ sinkMongo (host "127.0.0.1")

sinkMongo :: Host -> Sink Value IO ()
sinkMongo hst = sinkIO (runIOE $ connect $ host "127.0.0.1") close action (const $ return ())
  where
    action pipe ans = do
      let eventTag = (,) <$> lookup' "event" ans <*> lookupDeep ["source", "screen_name"] ans :: Maybe (T.Text, T.Text)
      case eventTag of
        Just ("favorite", "mr_konn") -> do
          let Just obj = lookup' "target_object" ans
              mid = lookup' "id" obj :: Maybe Integer
              Just js  = fromJSON' (maybe obj (\v -> insertJS "_id" v obj) mid)
          access pipe master "twitter" $ save "timeline" $ merge ["favorited" =: True] $ renderCreatedAtDoc js
          return IOProcessing
        Just ("unfavorite", _) -> do
          let Just obj = lookup' "target_object" ans
              mid = lookup' "id" obj :: Maybe Integer
              Just js  = fromJSON' $ maybe obj (\v -> insertJS "_id" v obj) mid
          access pipe master "twitter" $ save "timeline" $ merge ["favorited" =: False] $ renderCreatedAtDoc js
          return IOProcessing
        _ -> case lookupDeep ["delete", "status", "id"] ans :: Maybe Integer of
               Just id -> access pipe master "twitter" (delete (select ["_id" =: id] "timeline")) >> return IOProcessing
               Nothing -> do
                 let mid = lookup' "id" ans :: Maybe Integer
                     Just js = fromJSON' $ maybe ans (\v -> insertJS "_id" v ans) mid
                     isStatus = isJust (lookup' "text" ans :: Maybe T.Text)
                 when isStatus $ do
                   access pipe master "twitter" $ insert "timeline" (renderCreatedAtDoc js)
                   return ()
                 return IOProcessing
    
myOp :: TChan Value -> Response (Source IO BS.ByteString) -> ResourceT IO ()
myOp ch rsp =
  responseBody rsp $= splitWhen 13 $= LC.concatMap (maybeToList . A.maybeResult . A.parse json)
                   $$ sinkTChan ch

renderCreatedAtDoc :: Document -> Document
renderCreatedAtDoc = map transWithKey
  where
    transWithKey ("created_at" := Bson.String ustr) = "created_at" =: fromTwitterTime (read $ US.unpack ustr)
    transWithKey (k := v)                           = k := renderCreatedAtValue v

renderCreatedAtValue :: Bson.Value -> Bson.Value
renderCreatedAtValue (Doc doc)       = Doc $ renderCreatedAtDoc doc
renderCreatedAtValue (Bson.Array bs) = Bson.Array $ map renderCreatedAtValue bs
renderCreatedAtValue dat             = dat
