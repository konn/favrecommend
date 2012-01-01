{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Main where
import Data.Enumerator hiding (map)
import qualified Data.Enumerator.Binary as BE
import qualified Data.Enumerator.List as LE
import qualified Data.Text as T
import qualified Data.Attoparsec as A
import Data.Aeson
import Network.HTTP.Enumerator hiding (host)
import Web.Authenticate.OAuth hiding (insert, delete)
import Control.Monad.Trans
import Network.HTTP.Types
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Control.Exception
import Control.Concurrent
import Data.Maybe
import Instances
import Database.MongoDB hiding (Value)
import Prelude hiding (lookup)
import Control.Applicative
import qualified Data.Bson as Bson
import qualified Data.UString as US

import Tokens

main :: IO ()
main = do
  req <- signOAuth twitter konn =<< parseUrl "https://userstream.twitter.com/2/user.json"
  (withManager $ run_ . http req myIter) `finally` do
         putStrLn "connetion closed. waiting for 10 secs..."
         threadDelay (10*10^6)
         putStrLn "reconnecting..."
         main

renderCreatedAtDoc :: Document -> Document
renderCreatedAtDoc = map transWithKey
  where
    transWithKey ("created_at" := Bson.String ustr) = "created_at" =: fromTwitterTime (read $ US.unpack ustr)
    transWithKey (k := v)                           = k := renderCreatedAtValue v

renderCreatedAtValue :: Bson.Value -> Bson.Value
renderCreatedAtValue (Doc doc)       = Doc $ renderCreatedAtDoc doc
renderCreatedAtValue (Bson.Array bs) = Bson.Array $ map renderCreatedAtValue bs
renderCreatedAtValue dat             = dat

myIter :: MonadIO m => Status -> ResponseHeaders -> Iteratee BS.ByteString m ()
myIter s h = BE.splitWhen (== 13) =$ mapMaybeEnum (A.maybeResult . A.parse json) =$ LE.drop 1 >> iter
  where
    iter = liftIO (runIOE $ connect $ host "127.0.0.1") >>= loop
    loop pipe = do
      eof <- isEOF
      unless eof $ do
        Just ans <- LE.head
        let eventTag = (,) <$> lookup' "event" ans <*> lookupDeep ["source", "screen_name"] ans :: Maybe (T.Text, T.Text)
        case eventTag of
          Just ("favorite", "mr_konn") -> do
            let Just obj = lookup' "target_object" ans
                mid = lookup' "id" obj :: Maybe Integer
                Just js  = fromJSON' (maybe obj (\v -> insertJS "_id" v obj) mid)
            access pipe master "twitter" $ save "timeline" $ merge ["favorited" =: True] $ renderCreatedAtDoc js
          Just ("unfavorite", _) -> do
            let Just obj = lookup' "target_object" ans
                mid = lookup' "id" obj :: Maybe Integer
                Just js  = fromJSON' $ maybe obj (\v -> insertJS "_id" v obj) mid
            access pipe master "twitter" $ save "timeline" $ merge ["favorited" =: False] $ renderCreatedAtDoc js
          _ -> case lookupDeep ["delete", "status", "id"] ans :: Maybe Integer of
                 Just id -> access pipe master "twitter" $ delete (select ["_id" =: id] "timeline")
                 Nothing -> do
                   let mid = lookup' "id" ans :: Maybe Integer
                       Just js = fromJSON' $ maybe ans (\v -> insertJS "_id" v ans) mid
                   access pipe master "twitter" $ insert "timeline" (renderCreatedAtDoc js) >> return ()
        loop pipe
      liftIO $ close pipe

mapMaybeEnum :: Monad m => (a -> Maybe b) -> Enumeratee a b m c
mapMaybeEnum f = LE.concatMap (maybeToList . f)
