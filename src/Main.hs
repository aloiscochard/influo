import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Primitive
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Criterion.Main
import Criterion.Measurement
import Data.Map (Map)
import Data.Monoid
import Data.Monoid.Statistics.Numeric hiding (Min, Max)
import Data.Semigroup
import Network.HTTP
import System.Random.MWC

import qualified Data.Map as Map

numberOfThreads           = 4
numberOfRequests          = 100 * 1000
numberOfRequestsPerThread = div numberOfRequests numberOfThreads

data Event = Event Int ResponseCode | Kill deriving Show

type Counts = Map ResponseCode Integer

main :: IO ()
main = withSystemRandom $ \gen -> do
  shared  <- atomically $ (newTQueue :: STM (TQueue Event))
  runners <- replicateM numberOfThreads $ async (replicateM numberOfRequestsPerThread . nfIO $ action shared gen)
  worker  <- async (stats shared (\x -> (Min x, Max x, Mean 1 (fromIntegral x))))
  _       <- waitAll runners
  _       <- atomically $ writeTQueue shared Kill
  count   <- wait worker
  _       <- putStrLn . show $ count
  return () where
    action :: TQueue Event -> Gen (PrimState IO) -> IO ()
    action shared gen = do
      path              <- genString gen
      (duration, code)  <- time $ simpleHTTP (getRequest ("http://localhost/" ++ path)) >>= getResponseCode
      _                 <- atomically . writeTQueue shared $ Event (round (duration * 1000)) code
      return ()
    stats :: Monoid a => TQueue Event -> (Int -> a) -> IO (a, Counts)
    stats shared f = stats_ shared f mempty Map.empty
    stats_ :: Monoid a => TQueue Event -> (Int -> a) -> a -> Counts -> IO (a, Counts)
    stats_  shared f m counts = do
      event <- atomically $ readTQueue shared
      case event of
        Event duration code -> stats_ shared f (mappend m (f duration)) (Map.insert code ((Map.findWithDefault 0 code counts) + 1) counts)
        Kill                -> return (m, counts)


{-- TODO Move to async --}
waitAll :: [Async a] -> IO [(Async a, a)]
waitAll asyncs =
  atomically . sequence $ map (\a -> do r <- waitSTM a; return (a, r)) asyncs

{-- TODO Move to commons --}
genString :: Gen (PrimState IO) -> IO String
genString gen = do
    xs <- replicateM 4 $ uniformR (start, end) gen
    return $ map toEnum xs where
      start = fromEnum 'a'
      end = fromEnum 'z'

