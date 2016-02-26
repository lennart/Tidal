module Sound.Tidal.Mux where

import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map

import Sound.Tidal.Utils
import Sound.Tidal.Pattern
import Sound.Tidal.Stream
import Sound.Tidal.Params
import Sound.Tidal.Time

setters :: IO Time -> [IO Time -> IO Stream] -> IO (Setter, Transitioner)
setters = genericSetters s_p

genericSetters prm getNow streams = do
  streams' <- mapM (\x -> x getNow) streams
  return $ (genericSetter prm streams',genericTransition prm streams')

genericSetter :: Param -> [Stream] -> Setter
genericSetter prm streams p = do
  mapM_ (\(d, p') -> (manipulator d) p') $ filtered prm streams p

genericTransition :: Param -> [Stream] -> Transitioner
genericTransition prm streams f p = do
  mapM_ (\(d, p') -> (transitioner d) f p') $ filtered prm streams p



filtered prm os p = concat [ zip named mapped, zip unnamed [unmapped] ]
    where
      (mapped, unmapped) = splitPatternByName prm p $ map (fromJust.samplename) named
      (named, unnamed) = partition (isJust.samplename) os

splitPatternByName :: Param -> ParamPattern -> [String] -> ([ParamPattern], ParamPattern)
splitPatternByName prm p = foldl (filterPatternByName prm p) ([], p)

filterPatternByName prm p (res, p') val = (res ++ [scope], unmapped)
  where
    scope = filterValues f p
    unmapped = filterValues (not.f) p'
    f = (/= Map.empty) . (Map.filterWithKey $ (\k v -> and [k == prm, v == (Just $ VS val)]))
