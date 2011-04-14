{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, Rank2Types, ScopedTypeVariables #-}

module Loligoptl.Graph
    ( O, C, Block(..)
    , Graph, MaybeO(..), MaybeC(..)
    , Graph'(..)
    , Body, emptyBody
    , NonLocal(..)
    , addBlock --, bodyList
    , DG, DBlock(..)
    , gUnitOO, gUnitCO, gUnitOC, gUnitCC
    , dgSplice, gSplice , cat
    , dgNil, dgNilC
    )
where

import Loligoptl.Label
import qualified Data.Map as Map


data O
data C

data MaybeO ex t where
  JustO    :: t -> MaybeO O t
  NothingO :: MaybeO C t

data MaybeC ex t where
  JustC    :: t -> MaybeC C t
  NothingC :: MaybeC O t

instance Functor (MaybeO ex) where
  fmap f (JustO a) = JustO (f a)
  fmap f NothingO = NothingO

instance Functor (MaybeC ex) where
  fmap f (JustC a) = JustC (f a)
  fmap f NothingC = NothingC


data Block n e x where
  BFirst  :: n C O -> Block n C O
  BMiddle :: n O O -> Block n O O
  BLast   :: n O C -> Block n O C

  BCat    :: Block n e O -> Block n O x -> Block n e x

  BHead   :: Block n C O -> n O O       -> Block n C O -- the zipper
  BTail   :: n O O       -> Block n O C -> Block n O C  
  BClosed :: Block n C O -> Block n O C -> Block n C C 



type Body n = LabelMap (Block n C C)
emptyBody :: LabelMap (thing C C)
emptyBody = mapEmpty


data Graph' block n e x where
  GNil  :: Graph' block n O O
  GUnit :: block n O O -> Graph' block n O O 
  GMany :: MaybeO e (block n O C)
        -> LabelMap (block n C C)
        -> MaybeO x (block n C O)
        -> Graph' block n e x

type Graph = Graph' Block


class NonLocal thing where
  entryLabel :: thing C x -> Label
  successors :: thing e C -> [Label]

instance NonLocal n => NonLocal (Block n) where
  entryLabel (BFirst n)    = entryLabel n
  entryLabel (BHead h _)   = entryLabel h
  entryLabel (BClosed h _) = entryLabel h
  successors (BLast n)     = successors n
  successors (BTail _ t)   = successors t
  successors (BClosed _ t) = successors t



addBlock :: NonLocal thing => thing C C -> LabelMap (thing C C) -> LabelMap (thing C C)
addBlock block body = mapInsert (entryLabel block) block body
--bodyList = id
{-  GNil  :: Graph' block n O O
  GUnit :: block n O O -> Graph' block n O O 
  GMany :: MaybeO e (block n O C)
        -> LabelMap (block n C C)
        -> MaybeO x (block n C O)
        -> Graph' block n e x
-}
--addBlock GNil b = GUnit 


-- | Decorated graphs
type DG f = Graph' (DBlock f)
data DBlock f n e x = DBlock f (Block n e x)

instance NonLocal n => NonLocal (DBlock f n) where
  entryLabel (DBlock _ b) = entryLabel b
  successors (DBlock _ b) = successors b

dgNil  = GNil
dgNilC :: DG f n C C
dgNilC = GMany NothingO emptyBody NothingO

gUnitOO b = GUnit b
gUnitOC b = GMany (JustO b) emptyBody NothingO
gUnitCO b = GMany NothingO emptyBody (JustO b)
gUnitCC b = GMany NothingO (addBlock b emptyBody) NothingO

dgSplice :: NonLocal n => DG f n e a -> DG f n a x -> DG f n e x
dgSplice = splice fzCat
  where fzCat :: DBlock f n e O -> DBlock t n O x -> DBlock f n e x
        fzCat (DBlock f b1) (DBlock _ b2) = DBlock f (b1 `cat` b2)


-- boilerplate from hoopl follows
-- necessary for all the good static type checking
splice :: forall block n e a x . NonLocal (block n) =>
          (forall e x . block n e O -> block n O x -> block n e x)
       -> (Graph' block n e a -> Graph' block n a x -> Graph' block n e x)
splice bcat = sp
  where sp :: forall e a x .
              Graph' block n e a -> Graph' block n a x -> Graph' block n e x

        sp GNil g2 = g2
        sp g1 GNil = g1

        sp (GUnit b1) (GUnit b2) = GUnit (b1 `bcat` b2)

        sp (GUnit b) (GMany (JustO e) bs x) = GMany (JustO (b `bcat` e)) bs x

        sp (GMany e bs (JustO x)) (GUnit b2) = GMany e bs (JustO (x `bcat` b2))

        sp (GMany e1 bs1 (JustO x1)) (GMany (JustO e2) b2 x2)
          = GMany e1 (b1 `bodyUnion` b2) x2
          where b1 = addBlock (x1 `bcat` e2) bs1

        sp (GMany e1 b1 NothingO) (GMany NothingO b2 x2)
          = GMany e1 (b1 `bodyUnion` b2) x2

        sp _ _ = error "bogus GADT match failure"

bodyUnion :: forall a . LabelMap a -> LabelMap a -> LabelMap a
bodyUnion = mapUnionWithKey nodups
  where nodups l _ _ = error $ "duplicate blocks with label " ++ show l


gSplice :: NonLocal n => Graph n e a -> Graph n a x -> Graph n e x
gSplice = splice cat

cat :: Block n e O -> Block n O x -> Block n e x
cat b1@(BFirst {})     (BMiddle n)  = BHead   b1 n
cat b1@(BFirst {})  b2@(BLast{})    = BClosed b1 b2
cat b1@(BFirst {})  b2@(BTail{})    = BClosed b1 b2
cat b1@(BFirst {})     (BCat b2 b3) = (b1 `cat` b2) `cat` b3
cat b1@(BHead {})      (BCat b2 b3) = (b1 `cat` b2) `cat` b3
cat b1@(BHead {})      (BMiddle n)  = BHead   b1 n
cat b1@(BHead {})   b2@(BLast{})    = BClosed b1 b2
cat b1@(BHead {})   b2@(BTail{})    = BClosed b1 b2
cat b1@(BMiddle {}) b2@(BMiddle{})  = BCat    b1 b2
cat    (BMiddle n)  b2@(BLast{})    = BTail    n b2
cat b1@(BMiddle {}) b2@(BCat{})     = BCat    b1 b2
cat    (BMiddle n)  b2@(BTail{})    = BTail    n b2
cat    (BCat b1 b2) b3@(BLast{})    = b1 `cat` (b2 `cat` b3)
cat    (BCat b1 b2) b3@(BTail{})    = b1 `cat` (b2 `cat` b3)
cat b1@(BCat {})    b2@(BCat{})     = BCat    b1 b2
cat b1@(BCat {})    b2@(BMiddle{})  = BCat    b1 b2

