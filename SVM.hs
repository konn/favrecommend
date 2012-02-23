{-# LANGUAGE TypeSynonymInstances #-}
module SVM ( SVMT, SVM, SVMState(..), (<.>)
           , runSVMT, runSVM, learn, main
           , judge, Document(..)) where
import Prelude hiding (sum)
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.IntMap as M
import Control.Concurrent
import Control.Applicative
import Data.Foldable (sum)
import Data.Maybe

type Vector = M.IntMap

(<.>) :: Num a => Vector a -> Vector a -> a
v1 <.> v2 = sum $ M.intersectionWith (*) v1 v2

(.+.), (.-.) :: Num a => Vector a -> Vector a -> Vector a
(.+.) = M.unionWith (+)
(.-.) = M.unionWith (-)

(!) :: Num a => Vector a -> Int -> a
v ! i = fromMaybe 0 $ M.lookup i v

(*.) :: Num a => a -> Vector a -> Vector a
k *. v = M.map (k *) v

zero :: Num a => Vector a
zero = M.empty


data SVMState = SVMS { planeVector :: Vector Double
                     , step :: Integer
                     , lastUpdated :: Vector Integer 
                     , eta :: Double
                     } deriving (Show, Eq, Ord)

data Document = Document { feature :: Vector Double
                         , label :: Double
                         } deriving (Show, Eq, Read)

type SVMT m = StateT SVMState m 
type SVM = SVMT Identity

runSVMT :: Monad m => SVMT m a -> m SVMState
runSVMT act = execStateT act SVMS { planeVector = zero
                                  , lastUpdated = zero
                                  , step = 0
                                  , eta = 0.1
                                  }

runSVM :: SVM a -> SVMState
runSVM = runIdentity . runSVMT

absolute :: (Floating a) => Vector a -> a
absolute v = sqrt (v <.> v)

learn :: MonadIO m => Document -> SVMT m ()
learn doc = do
  s@SVMS{planeVector = w,eta=eta, lastUpdated=upd, step=step} <- get
  let y  = label doc
      x  = feature doc
      cs = M.mapWithKey (\k _ -> 0.01 * fromIntegral (step - (upd ! k))) x
      w' = M.unionWith (\c w_k -> signum w_k * max (absolute w - c) 0) cs w
      -- t1 = if y * (w' <.> x) <= 1 then y *. x else zero
      -- w'' = w' .+. (eta *. t1)
      w'' = w' .-. (eta *. if y*(w' <.> x) <= 1 then (-y) *. x else zero)
  liftIO $ do
    putStrLn $ "\n===== Step " ++ show step ++ " ====="
    -- putStr "upd = \t" >> print upd
    putStr "x = \t" >> print x
    -- putStr "eta = \t" >> print eta
    -- putStr "t1 = \t" >> print t1
    putStr "w' = \t" >> print w'
    putStr "w'' = \t" >> print w''
  put s{planeVector = w'', step = step + 1, lastUpdated = foldr (`M.insert` step) upd $ M.keys x}

judge :: Monad m => Vector Double -> SVMT m Bool
judge doc = do
  SVMS {planeVector = w} <- get
  return $ (doc <.> w) >= 0

mkDoc :: [String] -> Document
mkDoc (x:xs) = Document (M.fromList $ map split xs)
                        (if x == "-1" then -1 else 1)      

split :: String -> (Int, Double)
split str = let (s1, ':':s2) = span (/=':') str in (read s1, read s2)

main = do
  src <- readFile "a1a.dat"
  let docs = map (mkDoc . words) $ lines src
  src' <- readFile "a1a.dat"
  let tests = map (mkDoc . words) $ lines src'
  runSVMT $ do
    replicateM_ 10 $ mapM_ learn docs
    hoges <- forM (zip [1..] tests) $ \(i, doc) -> do
      ans <- judge (feature doc)
      return (label doc, if ans then 1.0 else -1.0)
    let trues = filter (uncurry (==)) hoges
    liftIO $ putStrLn "===================="
    liftIO $ do
      let len  = fromIntegral $ length tests
          tp   = fromIntegral $ length $ filter (\(a,b) -> a == b && a == 1) hoges
          tn   = fromIntegral $ length $ filter (\(a,b) -> a == b && a == -1) hoges
          fp   = fromIntegral $ length $ filter (\(a,b) -> a /= b && b == 1) hoges
          fn   = fromIntegral $ length $ filter (\(a,b) -> a /= b && b == -1) hoges
          prec = tp / (tp + fp)
          recl = tp / (tp + fn)
      putStrLn $ "accuracy:\t"  ++ show (100 * (tp+tn) / len)       ++ "%"
      putStrLn $ "precision:\t" ++ show (100*prec)                  ++ "%"
      putStrLn $ "recall  :\t"    ++ show (100*recl)                ++ "%"
      putStrLn $ "F1-score:\t"  ++ show (200*prec*recl/(prec+recl)) ++ "%"
