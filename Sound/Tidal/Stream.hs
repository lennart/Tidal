{-# LANGUAGE OverloadedStrings, FlexibleInstances, RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable #-}

module Sound.Tidal.Stream where

import Data.Maybe
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception as E
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Ratio
import Data.Typeable
import Sound.Tidal.Pattern
import qualified Sound.Tidal.Parse as P
import Sound.Tidal.Tempo (Tempo, logicalTime, clocked,clockedTick,cps)
import Sound.Tidal.Utils
import qualified Sound.Tidal.Time as T

import qualified Data.Map.Strict as Map

type ToMessageFunc = Shape -> Tempo -> Int -> (Double, Double, ParamMap) -> Maybe (IO ())

data Backend = Backend {
  toMessage :: ToMessageFunc,
  flush :: Shape -> Tempo -> Int -> IO ()
  }

data Stream a = Stream {
  shp :: Maybe Shape,
  identity :: a,
  set :: Maybe (a -> IO ()),
  time :: Maybe (IO T.Time),
  patM :: Maybe (MVar (a, [a])),
  ticker :: Maybe (Backend -> Shape -> MVar (a, [a]) -> Tempo -> Int -> IO ()),
  transport :: Backend
  }

instance (Show a) => Show (Stream a) where
  show s@Stream{identity=id'} = show (id')

infixr 0 $=

($=) :: Stream a -> a -> IO()
($=) Stream{set=Just f} = f
($=) s@Stream{set=Nothing} = defaultSetter s


infixr 1 |~

(|~) :: (T.Time -> [a] -> a) -> Stream a -> Stream a
(|~) = transset

--defaultSetter :: Stream a -> a -> IO ()
defaultSetter s p =  streamset s p head

transset :: (T.Time -> [a] -> a) -> Stream a -> Stream a
transset trans s = s { set = Just trans' }
  where
    trans' = transSetter trans s

--transSetter :: (T.Time -> [a] -> a) -> Stream a -> a -> IO ()
transSetter trans s@Stream{time=Just getNow} p = do
  now <- getNow
  streamset s p $ trans now
transSetter _ Stream{time=Nothing} _ = putStrLn "Transition without `getNow` attempted, aborting"

-- streamset ::  Stream a -> a -> ([a] -> a) -> IO ()
streamset s p f = maybe (putStrLn "No pattern state present") swap dsMb
  where
    swap ds = do ps <- takeMVar ds
                 let p' = f (p:snd ps)
                 putMVar ds (p', p:snd ps)
                 return ()
    dsMb = patM s

-- streamstate :: Stream a -> IO (Stream a)
streamstate s@Stream{shp=Just shp', ticker=Just tk} = do
  patternsM <- newMVar (identity s, [])
  let ot = (tk backend shp' patternsM) :: Tempo -> Int -> IO ()
  _ <- forkIO $ clockedTick ticksPerCycle ot
  return $ s { patM = Just patternsM }
    where
      backend = transport s
streamstate s = return s


--stream' :: Shape -> IO T.Time -> Backend -> IO (Stream ParamPattern)
stream' s t b = streamstate $ Stream {
  identity=silence,
  shp=Just s,
  time=Just t,
  transport=b,
  set=Nothing,
  ticker=Just onTick',
  patM=Nothing
  }

data Param = S {name :: String, sDefault :: Maybe String}
           | F {name :: String, fDefault :: Maybe Double}
           | I {name :: String, iDefault :: Maybe Int}
  deriving Typeable

instance Eq Param where
  a == b = name a == name b

instance Ord Param where
  compare a b = compare (name a) (name b)
instance Show Param where
  show p = name p

data Shape = Shape {params :: [Param],
                    latency :: Double,
                    cpsStamp :: Bool}


data Value = VS { svalue :: String } | VF { fvalue :: Double } | VI { ivalue :: Int }
           deriving (Show,Eq,Ord,Typeable)

type ParamMap = Map.Map Param (Maybe Value)

type ParamPattern = Pattern ParamMap
           
ticksPerCycle = 8

defaultValue :: Param -> Maybe Value
defaultValue (S _ (Just x)) = Just $ VS x
defaultValue (I _ (Just x)) = Just $ VI x
defaultValue (F _ (Just x)) = Just $ VF x
defaultValue _ = Nothing

hasDefault :: Param -> Bool
hasDefault (S _ Nothing) = False
hasDefault (I _ Nothing) = False
hasDefault (F _ Nothing) = False
hasDefault _ = True

defaulted :: Shape -> [Param]
defaulted = filter hasDefault . params

defaultMap :: Shape -> ParamMap
defaultMap s
  = Map.fromList $ map (\x -> (x, defaultValue x)) (defaulted s)

required :: Shape -> [Param]
required = filter (not . hasDefault) . params

hasRequired :: Shape -> ParamMap -> Bool
hasRequired s m = isSubset (required s) (Map.keys (Map.filter (\x -> x /= Nothing) m))

isSubset :: (Eq a) => [a] -> [a] -> Bool
isSubset xs ys = all (\x -> elem x ys) xs


doAt t action = do forkIO $ do now <- getCurrentTime
                               let nowf = realToFrac $ utcTimeToPOSIXSeconds now
                               threadDelay $ floor $ (t - nowf) * 1000000
                               action
                   return ()

logicalOnset' change tick o offset = logicalNow + (logicalPeriod * o) + offset
  where
    tpc = fromIntegral ticksPerCycle
    cycleD = ((fromIntegral tick) / tpc) :: Double
    logicalNow = logicalTime change cycleD
    logicalPeriod = (logicalTime change (cycleD + (1/tpc))) - logicalNow


applyShape' :: Shape -> ParamMap -> Maybe ParamMap
applyShape' s m | hasRequired s m = Just $ Map.union m (defaultMap s)
                | otherwise = Nothing

start :: Backend -> Shape -> IO (MVar (ParamPattern))
start backend shape
  = do patternM <- newMVar silence
       let ot = (onTick backend shape patternM) :: Tempo -> Int -> IO ()
       forkIO $ clockedTick ticksPerCycle ot
       return patternM

-- variant of start where history of patterns is available
state :: Backend -> Shape -> IO (MVar (ParamPattern, [ParamPattern]))
state backend shape
  = do patternsM <- newMVar (silence, [])
       let ot = (onTick' backend shape patternsM) :: Tempo -> Int -> IO ()
       forkIO $ clockedTick ticksPerCycle ot
       return patternsM

stream :: Backend -> Shape -> IO (ParamPattern -> IO ())
stream backend shape
  = do patternM <- start backend shape
       return $ \p -> do swapMVar patternM p
                         return ()

streamcallback :: (ParamPattern -> IO ()) -> Backend -> Shape -> IO (ParamPattern -> IO ())
streamcallback callback backend shape
  = do f <- stream backend shape
       let f' p = do callback p
                     f p
       return f'

onTick :: Backend -> Shape -> MVar (ParamPattern) -> Tempo -> Int -> IO ()
onTick backend shape patternM change ticks
  = do p <- readMVar patternM
       let ticks' = (fromIntegral ticks) :: Integer
           a = ticks' % ticksPerCycle
           b = (ticks' + 1) % ticksPerCycle
           messages = mapMaybe
                      (toMessage backend shape change ticks)
                      (seqToRelOnsetDeltas (a, b) p)
       E.catch (sequence_ messages) (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
       flush backend shape change ticks
       return ()

-- Variant where mutable variable contains list as history of the patterns
onTick' :: Backend -> Shape -> MVar (ParamPattern, [ParamPattern]) -> Tempo -> Int -> IO ()
onTick' backend shape patternsM change ticks
  = do ps <- readMVar patternsM
       let ticks' = (fromIntegral ticks) :: Integer
           toM = (toMessage backend)
           a = ticks' % ticksPerCycle
           b = (ticks' + 1) % ticksPerCycle
           messages = mapMaybe
                      (toM shape change ticks)
                      (seqToRelOnsetDeltas (a, b) $ fst ps)
       E.catch (sequence_ messages) (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
       flush backend shape change ticks
       return ()

make :: (a -> Value) -> Shape -> String -> Pattern a -> ParamPattern
make toValue s nm p = fmap (\x -> Map.singleton nParam (defaultV x)) p
  where nParam = param s nm
        defaultV a = Just $ toValue a
        --defaultV Nothing = defaultValue nParam

makeS = make VS

makeF :: Shape -> String -> Pattern Double -> ParamPattern
makeF = make VF

makeI :: Shape -> String -> Pattern Int -> ParamPattern
makeI = make VI

param :: Shape -> String -> Param
param shape n = head $ filter (\x -> name x == n) (params shape)

merge :: ParamPattern -> ParamPattern -> ParamPattern
merge x y = (flip Map.union) <$> x <*> y

infixl 1 |=|
(|=|) :: ParamPattern -> ParamPattern -> ParamPattern
(|=|) = merge

(#) = (|=|)

mergeWith op x y = (Map.unionWithKey op) <$> x <*> y

mergeWith
  :: (Ord k, Applicative f) =>
     (k -> a -> a -> a)
     -> f (Map.Map k a) -> f (Map.Map k a) -> f (Map.Map k a)

mergeNumWith intOp floatOp = mergeWith f
  where f (F _ _) (Just (VF a)) (Just (VF b)) = Just (VF $ floatOp a b)
        f (I _ _) (Just (VI a)) (Just (VI b)) = Just (VI $ intOp a b)
        f _ _ b = b

mergePlus = mergeWith f
  where f (F _ _) (Just (VF a)) (Just (VF b)) = Just (VF $ a + b)
        f (I _ _) (Just (VI a)) (Just (VI b)) = Just (VI $ a + b)
        f (S _ _) (Just (VS a)) (Just (VS b)) = Just (VS $ a ++ b)
        f _ _ b = b


infixl 1 |*|
(|*|) :: ParamPattern -> ParamPattern -> ParamPattern
(|*|) = mergeNumWith (*) (*)

infixl 1 |+|
(|+|) :: ParamPattern -> ParamPattern -> ParamPattern
(|+|) = mergePlus

infixl 1 |-|
(|-|) :: ParamPattern -> ParamPattern -> ParamPattern
(|-|) = mergeNumWith (-) (-)

infixl 1 |/|
(|/|) :: ParamPattern -> ParamPattern -> ParamPattern
(|/|) = mergeNumWith (div) (/)

setter :: MVar (a, [a]) -> a -> IO ()
setter ds p = do ps <- takeMVar ds
                 putMVar ds $ (p, p:snd ps)
                 return ()

