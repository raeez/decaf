{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, TypeSynonymInstances #-}
module Loligoptl.Label
  ( Label
  , LabelSet, LabelMap
  , FactBase, noFacts, lookupFact
  --, freshLabel
  --, uniqueToLbl -- MkGraph and GHC use only
  )

where
import Loligoptl.Collections
import Loligoptl.Unique

import qualified Data.IntMap as M
import qualified Data.IntSet as S

import Decaf.IR.LIR

-----------------------------------------------------------------------------
--		Label
-----------------------------------------------------------------------------
type Label = LIRLabel
readable (LIRLabel s _) = s
unLabel (LIRLabel _ i) = i

{-uniqueToLbl :: Unique -> Label
uniqueToLbl = (LIRLabel "") . uniqueToInt

freshLabel :: UniqueMonad m => m Label
freshLabel = freshUnique >>= return . uniqueToLbl-}

-----------------------------------------------------------------------------
-- LabelSet

-- newtype LabelSet = LS UniqueSet deriving (Eq, Ord, Show)
newtype LabelSet = LS (S.IntSet)

instance IsSet LabelSet where
  type ElemOf LabelSet = Label

  setUnion (LS s1) (LS s2) = LS (S.union s1 s2)
  setEmpty = LS S.empty
  setFromList list = LS (S.fromList (map unLabel list))
  setMember k (LS s) = S.member (unLabel k) s
  setInsert k (LS s) = LS (S.insert (unLabel k) s)
  {-
  setNull (LS s) = setNull s
  setSize (LS s) = setSize s
  -- setMember (Label k) (LS s) = setMember k s

  -- setEmpty = LS setEmpty
  setSingleton (Label k) = LS (setSingleton k)
  setDelete (Label k) (LS s) = LS (setDelete k s)

  -- setUnion (LS x) (LS y) = LS (setUnion x y)
  setDifference (LS x) (LS y) = LS (setDifference x y)
  setIntersection (LS x) (LS y) = LS (setIntersection x y)
  setIsSubsetOf (LS x) (LS y) = setIsSubsetOf x y

  setFold k z (LS s) = setFold (k . uniqueToLbl) z s

  setElems (LS s) = map uniqueToLbl (setElems s)
  -- setFromList ks = LS (setFromList (map lblToUnique ks))
  -}

-----------------------------------------------------------------------------
-- LabelMap

-- utility functions defined on maps
mapDeleteList :: (IsMap map) => [KeyOf map] -> map a -> map a
mapDeleteList keys map = foldl (flip mapDelete) map keys -- fold the deletes together

-- mapMember :: (IsMap map) => KeyOf map -> map a -> Bool
-- newtype LabelMap v = LM (UniqueMap v) deriving (Eq, Ord, Show)
data LabelMap a = LM (M.IntMap a) (M.IntMap String) 

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

  mapMember key map = case mapLookup key map of
                        Just _ -> True
                        Nothing -> False

  {-mapNull (LM m) = mapNull m
  mapSize (LM m) = mapSize m
  mapMember (Label k) (LM m) = mapMember k m
  mapLookup (Label k) (LM m) = mapLookup k m
  mapFindWithDefault def (Label k) (LM m) = mapFindWithDefault def k m

  mapEmpty = LM mapEmpty
  mapSingleton (Label k) v = LM (mapSingleton k v)
  mapInsert (Label k) v (LM m) = LM (mapInsert k v m)
  mapDelete (Label k) (LM m) = LM (mapDelete k m)

  mapUnion (LM x) (LM y) = LM (mapUnion x y)
  mapUnionWithKey f (LM x) (LM y) = LM (mapUnionWithKey (f . uniqueToLbl) x y)
  mapDifference (LM x) (LM y) = LM (mapDifference x y)
  mapIntersection (LM x) (LM y) = LM (mapIntersection x y)
  mapIsSubmapOf (LM x) (LM y) = mapIsSubmapOf x y

  mapMap f (LM m) = LM (mapMap f m)
  mapMapWithKey f (LM m) = LM (mapMapWithKey (f . uniqueToLbl) m)
  mapFold k z (LM m) = mapFold k z m
  mapFoldWithKey k z (LM m) = mapFoldWithKey (k . uniqueToLbl) z m

  mapElems (LM m) = mapElems m
  mapKeys (LM m) = map uniqueToLbl (mapKeys m)
  mapToList (LM m) = [(uniqueToLbl k, v) | (k, v) <- mapToList m]
  mapFromList assocs = LM (mapFromList [(lblToUnique k, v) | (k, v) <- assocs]) -}

-----------------------------------------------------------------------------
-- FactBase

type FactBase f = LabelMap f
instance Show a => Show (LabelMap a) where
  show = show.mapToList

noFacts :: FactBase f
noFacts = mapEmpty

lookupFact :: Label -> FactBase f -> Maybe f
lookupFact = mapLookup
