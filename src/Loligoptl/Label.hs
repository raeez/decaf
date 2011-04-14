{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, TypeSynonymInstances #-}
module Loligoptl.Label
  ( Label(..)
--  , allLabels -- to be used only by the Fuel monad
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

import Decaf.IR.LIR

{-data Label = Label { readable :: String
                   , unLabel :: Int }
  deriving (Eq, Ord)-}
type Label = LIRLabel
readable (LIRLabel s _) = s
unLabel (LIRLabel _ i) = i

--instance Show Label where
--d  show (Label r n) = r ++ show n

--allLabels :: [Label]
--allLabels = map Label [1..]

data LabelMap a = LM (M.IntMap a) (M.IntMap String) 
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
  mapFromList :: [(KeyOf map, a)] -> map a

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
  type KeyOf LabelMap = LIRLabel
  
  mapEmpty = LM (M.empty) (M.empty)
  mapLookup label (LM m s) = M.lookup (unLabel label) m
  mapInsert key val (LM m s) = LM (M.insert (unLabel key) val m) 
                                  (M.insert (unLabel key) (readable key) s)
  mapDelete key (LM m s) = LM (M.delete (unLabel key) m) (M.delete (unLabel key) s)
  -- strings don't matter here I think
  mapUnionWithKey f (LM m1 s1) (LM m2 s2) = LM (M.unionWithKey (f . LIRLabel "") m1 m2) 
                                               (M.union s1 s2)
  mapFold f b (LM m s) = M.fold f b m
  mapFoldWithKey f b (LM m s) = M.foldWithKey (f . LIRLabel "") b m
  mapToList (LM m s) = map (\((k,a),(k',st)) -> (LIRLabel st k, a)) $ zip (M.toList m) (M.toList s)
  mapSingleton key a = LM (M.singleton (unLabel key) a)
                          (M.singleton (unLabel key) (readable key))
  mapMap f (LM m s) = LM (M.map f m) s
  mapUnion (LM m1 s1) (LM m2 s2) = LM (M.union m1 m2) (M.union s1 s2)

  mapFromList pairs = 
    let keys = map (unLabel . fst) pairs
        strs = map (readable . fst) pairs
        vals = map snd pairs
    in LM (M.fromList $ zip keys vals)
          (M.fromList $ zip keys strs)

instance IsSet LabelSet where
  type ElemOf LabelSet = Label
  
  setUnion (LS s1) (LS s2) = LS (S.union s1 s2)
  setEmpty = LS S.empty
  setFromList list = LS (S.fromList (map unLabel list))
  setMember a (LS s) = S.member (unLabel a) s
