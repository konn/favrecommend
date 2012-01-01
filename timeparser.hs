{-# LANGUAGE OverloadedStrings #-}
module Main where
import Database.MongoDB
import qualified Data.Bson as Bson
import qualified Data.UString as US
import Instances
import Control.Exception
import Control.Monad.Loops

renderCreatedAtDoc :: Document -> Document
renderCreatedAtDoc = map transWithKey
  where
    transWithKey ("created_at" := Bson.String ustr) = "created_at" =: fromTwitterTime (read $ US.unpack ustr)
    transWithKey (k := v)                           = k := renderCreatedAtValue v

renderCreatedAtValue :: Bson.Value -> Bson.Value
renderCreatedAtValue (Doc doc)       = Doc $ renderCreatedAtDoc doc
renderCreatedAtValue (Bson.Array bs) = Bson.Array $ map renderCreatedAtValue bs
renderCreatedAtValue dat             = dat

main :: IO ()
main = bracket (runIOE $ connect $ host "127.0.0.1") close $ \pipe -> do
  ans <- access pipe master "twitter" $ do
    docs <- rest =<< find (select [] "timeline")
    mapM_ (save "timeline" . renderCreatedAtDoc) docs
  print ans
  return ()


  