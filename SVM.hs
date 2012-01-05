module SVM ( SVMT, SVM, SVMState(..), (<.>)
           , runSVMT, runSVM, learn
           , judge, Document(..)) where
import Data.Vector hiding (mapM_, zip, forM)
import Prelude hiding (zipWith, sum, replicate, length, (++), map)
import qualified Prelude as P
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Vector.Mutable as MV
import Control.Concurrent
import Control.Applicative

(<.>) :: Num a => Vector a -> Vector a -> a
v1 <.> v2 = sum $ zipWith' 0 0 (*) v1 v2

zipWith' :: a -> b -> (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith' d1 d2 f v1 v2 = zipWith f (v1 ++ replicate (length v2 - length v1) d1)
                                   (v2 ++ replicate (length v1 - length v2) d2)

(.+.), (.-.) :: Num a => Vector a -> Vector a -> Vector a
(.+.) = zipWith' 0 0 (+)
(.-.) = zipWith' 0 0 (-)

(*.) :: Num a => a -> Vector a -> Vector a
k *. v = map (k *) v

zero :: Num a => Int -> Vector a
zero len = replicate len 0


data SVMState = SVMS { planeVector :: Vector Double
                     , eta :: Double
                     } deriving (Show, Eq, Ord)

data Document = Document { feature :: Vector Double
                         , label :: Double
                         } deriving (Show, Eq, Read)

type SVMT m = StateT SVMState m 
type SVM = SVMT Identity

runSVMT :: Monad m => SVMT m a -> m SVMState
runSVMT act = execStateT act SVMS { planeVector = zero 0
                                  , eta = 0.01
                                  }

runSVM :: SVM a -> SVMState
runSVM = runIdentity . runSVMT

absolute :: Num a => Vector a -> a
absolute v = v <.> v

learn :: Monad m => Document -> SVMT m ()
learn doc = do
  s@SVMS{planeVector = w,eta=eta} <- get
  let y  = label doc
      x  = feature doc
      t1 = if y * (w <.> x) <= 1 then negate y *. x else zero (length w)
      t2 = 0.01 *. map signum w
      w' = w .-. (eta *. (t1 .+. t2))
  put s{planeVector = w'}
  when (sqrt (absolute (w' .-. w)) > 1e-2) $ do
    learn doc

judge :: Monad m => Vector Double -> SVMT m Bool
judge doc = do
  SVMS {planeVector = w} <- get
  return $ (doc <.> w) >= 0

mkDoc :: [String] -> Document
mkDoc (x:xs) = Document (create $ sub $ P.map split xs)
                        (if x == "-1" then -1 else 1)
  where
    sub xs = do
      v <- MV.replicate 124 0
      MV.write v 0 1
      P.mapM (uncurry $ MV.write v) xs
      return v
      

split :: String -> (Int, Double)
split str = let (s1, ':':s2) = P.span (/=':') str in (read s1, read s2)

