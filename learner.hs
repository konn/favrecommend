{-# LANGUAGE OverloadedStrings #-}
module Main where
import SVM
import Text.MeCab hiding (id)
import qualified Text.MeCab as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.MongoDB hiding (group, sort, lookup, Document)
import qualified Data.Bson as B
import Data.Time
import qualified Data.UString as US
import Control.Monad.Trans
import qualified Data.Vector as V
import Data.List hiding (find)
import Control.Arrow
import Data.Maybe
import Data.Either
import Control.Applicative
import Control.Monad.Loops
import Control.Exception

crawl :: Action IO [B.Document]
crawl = do
  time <- liftIO $ getCurrentTime
  let limit = (-3*3600) `addUTCTime` time
  rest =<< find (select ["created_at" =: ["$lt" =: limit]] "timeline")

parseTweet :: B.Document -> IO [Node]
parseTweet doc = do
  let txt = maybe "" T.pack $ B.lookup "text" doc
  mec <- new ["mecab", "-l1"]
  parseToNode mec txt

features :: [Node] -> V.Vector Double
features nodes = let seeds = map (fromIntegral . head &&& fromIntegral . length) $ group $ sort $ map M.id nodes
                     maxID = maximum $ map fst seeds
                 in V.generate (maxID+1) $ fromMaybe 0 . flip lookup seeds

compileDoc :: B.Document -> IO Document
compileDoc doc = Document <$> (features <$> parseTweet doc) <*> pure ("favorited" `B.at` doc)

main :: IO ()
main = do
  pipe <- runIOE $ connect $ host "127.0.0.1"
  tws <- either (fail . show) return  =<< access pipe master "twitter" crawl
  close pipe
  docs <- rights <$> forkMapM compileDoc tws
  st <- runSVMT $ do
    mapM_ learn docs
  print st
  
