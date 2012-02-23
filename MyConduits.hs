module MyConduits ( splitWhen, sinkTChan, sinkChan, sourceTChan
                  , getCurrentContents, conduitCount ) where
{- parIter, parEnum, printChunkLength, iterQC -}

import Data.Conduit
import qualified Data.Conduit.List as LC
import Control.Concurrent hiding (yield)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Control.Concurrent.STM
import Control.Applicative
import Data.Word

splitWhen :: Resource m => Word8 -> Conduit BS.ByteString m BS.ByteString
splitWhen p = conduitState id push close
  where
    push front bs' = return $ StateProducing leftover ls
      where
        bs = front bs'
        (leftover, ls) = getLines id bs

    getLines front bs
        | BS.null bs = (id, front [])
        | BS.null y = (BS.append x, front [])
        | otherwise = getLines (front . (x:)) (BS.drop 1 y)
      where
        (x, y) = BS.breakByte p bs

    close front
        | BS.null bs = return []
        | otherwise = return [bs]
      where
        bs = front BS.empty

sinkTChan :: ResourceIO m => TChan a -> Sink a m ()
sinkTChan ch = SinkData
               { sinkPush  = push
               , sinkClose = close
               }
  where
    push x = do
      liftIO (atomically $ writeTChan ch x)
      return $ Processing push close
    close  = return ()

sinkChan :: ResourceIO m => Chan a -> Sink a m ()
sinkChan ch = SinkData
              { sinkPush  = push
              , sinkClose = close
              }
  where
    push x = do
      liftIO (writeChan ch x)
      return $ Processing push close
    close  = return ()

getCurrentContents :: Int -> TChan a -> STM [a]
getCurrentContents len ch = step len
  where
    step 0 = pure []
    step n = (:) <$> readTChan ch <*> step (n - 1)
         <|> pure []


conduitCount :: ResourceIO m => Int -> MVar Double -> Conduit a m a
conduitCount wait var = conduitIO allocator
                                  (uncurry $ const killThread)
                                  push
                                  closer
  where
    allocator = do
      mVar <- newMVar (0 :: Integer)
      tid <- forkIO $ counter mVar
      return (mVar, tid)
    counter c = forever $ do
      old <- readMVar c
      threadDelay (wait * 10^6)
      new <- swapMVar c 0
      swapMVar var $ fromIntegral (new - old) / fromIntegral wait
      return ()
    push (count, _) input = do
      liftIO $ modifyMVar_ count (return . (+1))
      return $ IOProducing [input]
    closer (c, tid) = do
      liftIO $ swapMVar var 0
      return []
      

sourceTChan :: ResourceIO m => TChan a -> Source m a
sourceTChan ch = Source 
  { sourcePull  = Open (sourceTChan ch) <$> liftIO (atomically $ readTChan ch)
  , sourceClose = return ()
  } 
