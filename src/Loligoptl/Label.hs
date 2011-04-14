{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, TypeSynonymInstances #-}
module Loligoptl.Label
  ( Label
  , allLabels -- to be used only by the Fuel monad
  , LabelMap
  , FactBase
  , LabelSet
  , IsMap (..)
  , IsSet (..)
  , mapDeleteList, mapMember
  )
where

import qualified Data.IntMap as M
import qualified Data.IntSet as S

newtype Label = Label { unLabel :: Int }
  deriving (Eq, Ord)

instance Show Label where
  show (Label n) = "L" ++ show n

allLabels :: [Label]
allLabels = map Label [1..]

newtype LabelMap a = LM (M.IntMap a)
type FactBase a = LabelMap a
newtype LabelSet = LS (S.IntSet)

class IsMap map where
  type KeyOf map

  mapEmpty :: map a
  mapLookup :: KeyOf map -> map a -> Maybe a
  mapInsert :: KeyOf map -> a -> map a -> map a
  mapDelete :: KeyOf map -> map a -> map a
  mapUnionWithKey :: ((KeyOf map) -> a -> a -> a) -> map a -> map a -> map a
  mapFold :: (a -> b -> b) -> b -> map a -> b
  mapFoldWithKey :: (KeyOf map -> a -> b -> b) -> b -> map a -> b
  mapToList :: map a -> [(KeyOf map, a)]
  mapSingleton :: KeyOf map -> a -> map a
  mapMap :: (a->b) -> map a -> map b
  mapUnion :: map a -> map a -> map a

-- utility functions defined on maps
mapDeleteList :: (IsMap map) => [KeyOf map] -> map a -> map a
mapDeleteList keys map = foldl (flip mapDelete) map keys -- fold the deletes together

mapMember :: (IsMap map) => KeyOf map -> map a -> Bool
mapMember key map = case mapLookup key map of
                      Just _ -> True
                      Nothing -> False

class IsSet set where
  type ElemOf set

  setEmpty :: set
  setUnion :: set -> set -> set
  setFromList :: [ElemOf set] -> set
  setMember :: ElemOf set -> set -> Bool

instance IsMap LabelMap where
  type KeyOf LabelMap = Label
  
  mapEmpty = LM (M.empty)
  mapLookup label (LM m) = M.lookup (unLabel label) m
  mapInsert key val (LM m) = LM (M.insert (unLabel key) val m)
  mapDelete key (LM m) = LM (M.delete (unLabel key) m)
  mapUnionWithKey f (LM m1) (LM m2) = LM (M.unionWithKey (f . Label) m1 m2)
  mapFold f b (LM m) = M.fold f b m
  mapFoldWithKey f b (LM m) = M.foldWithKey (f . Label) b m
  mapToList (LM m) = map (\(k,a) -> (Label k, a)) $ M.toList m
  mapSingleton key a = LM (M.singleton (unLabel key) a)
  mapMap f (LM m) = LM (M.map f m)
  mapUnion (LM m1) (LM m2) = LM (M.union m1 m2)

instance IsSet LabelSet where
  type ElemOf LabelSet = Label
  
  setUnion (LS s1) (LS s2) = LS (S.union s1 s2)
  setEmpty = LS S.empty
  setFromList list = LS (S.fromList (map unLabel list))
  setMember a (LS s) = S.member (unLabel a) s
