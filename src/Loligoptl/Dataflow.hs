{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, RankNTypes, 
 ScopedTypeVariables, MultiParamTypeClasses, PatternGuards #-}

{-
TODO:

integrate with compiler

-}

module Loligoptl.Dataflow
where

import Loligoptl.Label
import Loligoptl.Graph
import Loligoptl.Fuel

import Data.Maybe


type family   Fact x f :: *
type instance Fact O f = f
type instance Fact C f = FactBase f


data PassDirection = Fwd | Bwd deriving (Show, Eq)

data FwdPass m n f 
  = FwdPass { fpLattice :: DataflowLattice f
            , fpTransfer :: FwdTransfer n f
            , fpRewrite :: FwdRewrite m n f
            }

data DataflowLattice f = DataflowLattice
  { factBottom :: f -- necessary?
  , factJoin :: f -> f -> (ChangeFlag, f)
  }

-- are these foralls needed?
newtype FwdTransfer n f = FwdTransfer3 
  { getFwdTransfer3 :: 
      ( n C O -> f -> f
      , n O O -> f -> f
      , n O C -> f -> FactBase f
      )
  }


newtype FwdRewrite m n f = FwdRewrite3
  { getFwdRewrite3 :: 
      ( n C O -> f -> m (Maybe (FwdRev m n f C O))
      , n O O -> f -> m (Maybe (FwdRev m n f O O))
      , n O C -> f -> m (Maybe (FwdRev m n f O C))
      )
  }

-- fwd revision to graph
-- includes the new subgraph, and a new rewrite function to use when analyzing the new graph
data FwdRev m n f e x  = FwdRev (Graph n e x) (FwdRewrite m n f)


analyzeAndFwdRewrite
  :: (FuelMonad m, NonLocal n)
  => FwdPass m n f
  -> [Label]          -- entry points
  -> Graph n C C      -- might need to make more polymorphic on shape 
  -> FactBase f       -- input facts
  -> m (Graph n C C, FactBase f)

analyzeAndFwdRewrite pass entryLabels g fb = 
  do { (dg, f) <- afrGraph pass entries g fb
     ; return $ normalizeGraph dg }
  where entries = 
          case entryLabels of 
            [] -> error "closed graph not provided any entry points" -- This is an error I think
            otherwise -> JustC entryLabels
                    

normalizeGraph :: forall n f e x . NonLocal n => DG f n e x -> (Graph n e x, FactBase f)
normalizeGraph g = (dropFacts g, facts g)
  where
    dropFacts :: DG f n e x -> Graph n e x
    dropFacts GNil = GNil
    dropFacts (GUnit (DBlock f b)) = GUnit b
    dropFacts (GMany e b x) = GMany (fmap dropFact e) (mapMap dropFact b) (fmap dropFact x)
      where dropFact (DBlock _ b) = b

    facts :: DG f n e x -> FactBase f
    facts GNil = mapEmpty
    facts (GUnit _) = mapEmpty -- not sure why this should be empty
    facts (GMany _ body exit) = bodyFacts body `mapUnion` exitFacts exit
    bodyFacts :: LabelMap (DBlock f n C C) -> FactBase f
    bodyFacts body = mapFold pullFact mapEmpty body
      where pullFact :: forall n f x . (NonLocal n) => DBlock f n C x -> FactBase f ->  FactBase f -- sig necessary
            pullFact (DBlock f b) fb = mapInsert (entryLabel b) f fb
    exitFacts :: MaybeO x (DBlock f n C O) -> FactBase f
    exitFacts NothingO = mapEmpty
    exitFacts (JustO (DBlock f b)) = mapSingleton (entryLabel b) f

afrGraph
  :: forall m n f e x . (FuelMonad m, NonLocal n)
  => FwdPass m n f
  -> MaybeC e [Label]  -- entry points if graph is closed
  -> Graph n e x
  -> Fact e f          -- facts flowing in
  -> m (DG f n e x, Fact x f)

afrGraph pass entries = graph
  where
    node :: forall e x . (ShapeLifter e x) => n e x     
         ->  f -> m (DG f n e x, Fact x f)

    node n f = 
        do mrev <- frewrite pass n f >>= withFuel
           case mrev of 
             Nothing -> return (singletonDG f n, ftransfer pass n f)
             Just (FwdRev g rw) ->
                 let pass' = pass {fpRewrite = rw}
                     f' = fwdEntryFact n f
                 in afrGraph pass' (fwdEntryLabel n) g f'

    block :: forall e x. Block n e x 
          -> f -> m (DG f n e x, Fact x f)
    block (BFirst  n)  = node n
    block (BMiddle n)  = node n
    block (BLast   n)  = node n

    block (BCat b1 b2) = block b1 `cat` block b2

    block (BHead h n)  = block h  `cat` node n
    block (BTail n t)  = node  n  `cat` block t
    block (BClosed h t)= block h  `cat` block t


    body :: [Label] -> LabelMap (Block n C C) 
         -> Fact C f  -> m (DG f n C C, Fact C f) 

    body entries blockmap initFBase
        = fixpoint Fwd lattice doBlock blocks initFBase
      where
          blocks = forwardBlockList entries blockmap
          lattice = fpLattice pass
          doBlock b fb = block b entryFact
            where entryFact = getFact lattice (entryLabel b) fb
                                

    graph :: Graph n e x 
          -> Fact e f -> m (DG f n e x, Fact x f)
    graph GNil            = \f -> return (dgNil, f)
    graph (GUnit blk)     = block blk
    graph (GMany e bdy x) = (e `ebcat` bdy) `cat` exit x
     where
      ebcat :: MaybeO e (Block n O C) -> Body n -> Fact e f -> m (DG f n e C, Fact C f)
      exit  :: MaybeO x (Block n C O)           -> Fact C f -> m (DG f n C x, Fact x f)
      exit (JustO blk) = arfx block blk
      exit NothingO    = \fb -> return (dgNilC, fb)
      ebcat entry bdy = c entries entry
       where c :: MaybeC e [Label] -> MaybeO e (Block n O C)
                -> Fact e f -> m (DG f n e C, Fact C f)
             c NothingC (JustO entry)   = block entry `cat` body (successors entry) bdy
             c (JustC entries) NothingO = body entries bdy
             c _ _ = error "bogus GADT pattern match failure"

    -- this glues all the subgraphs together; afrGraph sort of
    -- operates in a monad like thing, for which this is bind
    cat :: forall e a x f1 f2 f3.
         (f1 -> m (DG f n e a, f2))
        -> (f2 -> m (DG f n a x, f3))
        -> (f1 -> m (DG f n e x, f3))
    cat t1 t2 f1 = do { (g1, f2) <- t1 f1
                      ; (g2, f3) <- t2 f2
                      ; return (g1 `dgSplice` g2, f3) }

    arfx :: forall thing x . NonLocal thing
         => (thing C x ->        f -> m (DG f n C x, Fact x f))
         -> (thing C x -> Fact C f -> m (DG f n C x, Fact x f))
    arfx arf thing fb = 
      arf thing $ fromJust $ mapLookup (entryLabel thing) $ joinInFacts lattice fb
     where lattice = fpLattice pass





data ChangeFlag = SomeChange | NoChange
data FixState n f 
  = FS { fsChange :: ChangeFlag
       , fsFBase :: FactBase f
       , fsDG :: DG f n C C
       , fsLabels :: LabelSet
       }

updateFact :: DataflowLattice f -> LabelSet 
           -> Label -> f -> (ChangeFlag, FactBase f)
           -> (ChangeFlag, FactBase f)
updateFact lat labels label newFact (ch, fbase)
  | NoChange <- ch' = (ch, fbase)
  | label `setMember` labels = (SomeChange, newFBase)
  | otherwise = (ch, newFBase)
  where
    (ch', resFact) 
       = case mapLookup label fbase of
           Nothing -> (SomeChange, new_fact_debug)
           Just oldFact -> join oldFact
         where join oldFact = 
                 factJoin lat {-label-} oldFact newFact
               (_, new_fact_debug) = join (factBottom lat)
    newFBase = mapInsert label resFact fbase
  

fixpoint :: forall m n f. (FuelMonad m, NonLocal n) 
         => PassDirection -> DataflowLattice f
         -> (Block n C C -> FactBase f -> m (DG f n C C, Fact C f))
         -> [Block n C C] -> FactBase f ->  m (DG f n C C, Fact C f)

fixpoint dir lattice doBlock blocks initFBase
  = do { fs <- loop initFBase
       -- return the dg produced, and the part of the factbase
       -- consisting of labels NOT in the subgraph being considered
       ; return (fsDG fs, mapDeleteList (map (fst . fst) taggedBlocks) (fsFBase fs))
       }
  where
    loop fbase = 
      do{ fs <- iterBlocks taggedBlocks (FS {fsChange = NoChange, fsFBase = fbase
                                          , fsDG = dgNilC, fsLabels = setEmpty})
        ; case fsChange fs of 
            NoChange -> return fs
            SomeChange -> loop $ fsFBase fs }

    taggedBlocks = map tag blocks
    tag :: NonLocal t => t C C -> ((Label, t C C), [Label])
    tag b = ((entryLabel b, b), 
             if (dir == Fwd) then [entryLabel b] 
             else successors b)
    iterBlocks :: [((Label, Block n C C), [Label])]
               -> FixState n f -> m (FixState n f)
    iterBlocks (((l, b), entries):bs) fs 
      = iterBlock l b entries fs >>= iterBlocks bs

    iterBlock :: Label -> Block n C C -> [Label] -> FixState n f -> m (FixState n f)
    iterBlock label block inLabels
      fs@(FS {fsChange = ch, fsFBase = fbase, fsDG = dg, fsLabels = labels})
        | dir == Fwd  && not (label `mapMember` fbase)
          = return $ fs {fsLabels = labels'}
        | otherwise
          = do { (dg', outFacts) <- doBlock block fbase
               ; let (ch', fbase') = mapFoldWithKey
                                     (updateFact lattice labels) 
                                     (ch,fbase) outFacts
               ; return $ 
                        FS { fsChange = ch'
                           , fsFBase = fbase'
                           , fsDG = dg `dgSplice` dg'
                           , fsLabels = labels' } }
        where
          labels' = labels `setUnion` setFromList inLabels
      



          
       

-- | Orders the blocks in a graph for data flow analysis.
-- | Could make this faster using folds
forwardBlockList :: NonLocal n => [Label] -> LabelMap (Block n C C) -> [Block n C C]
forwardBlockList entries body
  = map lookupBlock (reverse $ travHelp [] entries body) -- use cons, then reverse
  where
    travHelp :: [Label] -> [Label] -> LabelMap (Block n C C) -> [Label]
    travHelp seen [] body = seen
    travHelp seen (x:xs) body = 
        if x `elem` seen
          then travHelp seen xs body
          else travHelp (x:seen) ((successors $ lookupBlock x) ++ xs) body
    lookupBlock x = case (mapLookup x body) of 
                      Just b -> b
                      Nothing -> error "successors returned label not in graph body"

                                                   
joinInFacts :: DataflowLattice f -> FactBase f -> FactBase f
joinInFacts (lattice @ DataflowLattice {factBottom = bot, factJoin = fj}) fb =
  mkFactBase lattice $ map botJoin $ mapToList fb
    where botJoin (l, f) = (l, snd $ fj {-l-} bot f) -- hoopl uses an extra label param in join functions for debugging purposes
                        



class ShapeLifter e x where
  singletonG    ::      n e x -> Graph n e x
  singletonDG   :: f -> n e x -> DG f n e x
  fwdEntryFact  :: NonLocal n => n e x -> f -> Fact e f
  fwdEntryLabel :: NonLocal n => n e x -> MaybeC e [Label]
  ftransfer :: FwdPass m n f -> n e x -> f -> Fact x f
  frewrite  :: FwdPass m n f -> n e x 
           -> f -> m (Maybe (FwdRev m n f e x))
{- bwdEntryFact :: NonLocal n => DataflowLattice f -> n e x -> Fact e f -> f
 btransfer    :: BwdPass m n f -> n e x -> Fact x f -> f
 brewrite     :: BwdPass m n f -> n e x
              -> Fact x f -> m (Maybe (Graph n e x, BwdRewrite m n f))
-}
instance ShapeLifter C O where
  singletonG = gUnitCO . BFirst
  singletonDG f = gUnitCO . DBlock f . BFirst
  fwdEntryFact     n f  = mapSingleton (entryLabel n) f
--  bwdEntryFact lat n fb = getFact lat (entryLabel n) fb
  ftransfer (FwdPass {fpTransfer = FwdTransfer3 (ft, _, _)}) n f = ft n f
--  btransfer (BwdPass {bpTransfer = BwdTransfer3 (bt, _, _)}) n f = bt n f
  frewrite  (FwdPass {fpRewrite  = FwdRewrite3  (fr, _, _)}) n f = fr n f
--  brewrite  (BwdPass {bpRewrite  = BwdRewrite3  (br, _, _)}) n f = br n f
  fwdEntryLabel n = JustC [entryLabel n]

instance ShapeLifter O O where
  singletonG = gUnitOO . BMiddle
  singletonDG f = gUnitOO . DBlock f . BMiddle
  fwdEntryFact   _ f = f
  --bwdEntryFact _ _ f = f
  ftransfer (FwdPass {fpTransfer = FwdTransfer3 (_, ft, _)}) n f = ft n f
--  btransfer (BwdPass {bpTransfer = BwdTransfer3 (_, bt, _)}) n f = bt n f
  frewrite  (FwdPass {fpRewrite  = FwdRewrite3  (_, fr, _)}) n f = fr n f
--  brewrite  (BwdPass {bpRewrite  = BwdRewrite3  (_, br, _)}) n f = br n f
  fwdEntryLabel _ = NothingC

instance ShapeLifter O C where
  singletonG = gUnitOC . BLast
  singletonDG f = gUnitOC . DBlock f . BLast
  fwdEntryFact   _ f = f
--  bwdEntryFact _ _ f = f
  ftransfer (FwdPass {fpTransfer = FwdTransfer3 (_, _, ft)}) n f = ft n f
--  btransfer (BwdPass {bpTransfer = BwdTransfer3 (_, _, bt)}) n f = bt n f
  frewrite  (FwdPass {fpRewrite  = FwdRewrite3  (_, _, fr)}) n f = fr n f
--  brewrite  (BwdPass {bpRewrite  = BwdRewrite3  (_, _, br)}) n f = br n f
  fwdEntryLabel _ = NothingC

getFact  :: DataflowLattice f -> Label -> FactBase f -> f
getFact lat l fb = case mapLookup l fb of Just  f -> f
                                          Nothing -> factBottom lat

mkFactBase :: forall f . DataflowLattice f -> [(Label, f)] -> FactBase f
mkFactBase lattice = foldl add mapEmpty
  where add :: FactBase f -> (Label, f) -> FactBase f
        add map (label, f) = mapInsert label newFact map
          where newFact = case mapLookup label map of
                            Nothing -> f
                            Just f' -> snd $ join {-label-} f' f
                join = factJoin lattice