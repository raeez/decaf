{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, RankNTypes, 
 ScopedTypeVariables, MultiParamTypeClasses, PatternGuards #-}

module Loligoptl.Dataflow
  {-( DataflowLattice(..), JoinFun, OldFact(..), NewFact(..), Fact, mkFactBase
  , ChangeFlag(..), changeIf
  , FwdPass(..), FwdTransfer
  , FwdRewrite
  , analyzeAndFwdRewrite
  --,  analyzeAndRewriteBwd
  , ShapeLifter
  , singletonG
  )-}
where

import Loligoptl.Label
import Loligoptl.Graph
import Loligoptl.Fuel
import Loligoptl.Checkpoint
import Loligoptl.Util
import Loligoptl.Collections
import Loligoptl.Unique
import qualified Loligoptl.GraphUtil as U

import Data.Maybe

import Decaf.IR.LIR -- ehh

changeIf :: Bool -> ChangeFlag
changeIf changed = if changed then SomeChange else NoChange
type family   Fact x f :: *
type instance Fact O f = f
type instance Fact C f = FactBase f

newtype OldFact a = OldFact a
newtype NewFact a = NewFact a

type M = CheckingFuelMonad (SimpleUniqueMonad)

data FwdPass m n f 
  = FwdPass { fp_lattice  :: DataflowLattice f
            , fp_transfer :: FwdTransfer n f
            , fp_rewrite  :: FwdRewrite m n f
            }

data BwdPass m n f
  = BwdPass { bp_lattice  :: DataflowLattice f
            , bp_transfer :: BwdTransfer n f
            , bp_rewrite  :: BwdRewrite m n f }

data DataflowLattice a = DataflowLattice  
 { fact_name       :: String          -- Documentation
 , fact_bot        :: a               -- Lattice bottom element
 , fact_join       :: JoinFun a       -- Lattice join plus change flag
                                      -- (changes iff result > old fact)
 }

type JoinFun a = Label -> OldFact a -> NewFact a -> (ChangeFlag, a)
-- are these foralls needed?



-- pass -> graph -> fold function from node, associated fact, to result type -> initial -> final
-- this doesn't properly belong here; assumes some Decaf stuff
foldDataflowFactsFwd :: forall n f e x res . ({- CheckpointMonad m,-} NonLocal n)
                     => FwdPass M n f
                     -> Graph n C C
                     -> (forall e x. n e x -> f -> res -> res)
                     -> res -> res

foldDataflowFactsFwd pass graph func init = 
  -- fst takes the DG, drops the state
  let
      -- partly copied from afrgraph
      block :: forall e x. Block n e x 
            -> f -> res -> (Fact x f, res)
      block (BFirst  n)   = node n 
      block (BMiddle n)   = node n
      block (BLast   n)   = node n
      block (BCat b1 b2)  = block b1 `cat` block b2
      block (BHead h n)   = block h  `cat` node n
      block (BTail n t)   = node  n  `cat` block t
      block (BClosed h t) = block h  `cat` block t
                           
      node :: forall e x. (ShapeLifter e x) => n e x 
           -> f -> res -> (Fact x f, res)
      -- apply transfer function and accum function
      -- use fact flowing OUT of node
      -- so that definitions get joined to themself in web building
      node n f acc = (ftransfer pass n f, func n f acc)

--       cat :: (f -> res -> (f,res))
--           -> (f -> res -> (f,res))
--           -> (f -> res -> (f,res))
      cat t1 t2 f acc = let (f', acc') = t1 f acc
                        in t2 f' acc'

      (undg, fb, _) = runSimpleUniqueMonad $ runWithFuel infiniteFuel
                      $ (analyzeAndRewriteFwd pass entries graph (mapSingleton mainlab bottom))
--                      $ ((analyzeAndRewriteFwd pass entries graph (mapSingleton mainlab bottom)) :: CheckingFuelMonad (SimpleUniqueMonad) (Graph n e x, FactBase f, MaybeO x f))

      undgBody = case undg of
                   GMany _ body _ -> body -- this seems like a hack
      mainlab = LIRLabel "main" (-1)
      entries = JustC [mainlab]

      -- dataflow stuff
      bottom = (fact_bot . fp_lattice) pass

      blocks = mapToList undgBody -- list of label, block pairs
      -- reverse, lookup facts, apply block functions
      blofacts = map (\(l,b) -> (block b,  lookupFact l fb)) blocks
      
  in
    -- apply the tuples to each other, only save the res
    -- fold list of res -> res onto init
    foldr ($) init $ concatMap process blofacts
      where
--        process :: ((f -> res -> (Fact x f, res)), Maybe f) -> [res -> res]
        process (bl, Just f) = [\res -> snd $ bl f res]
        process (bl, Nothing) = []

foldDataflowFactsBwd :: forall n f e x res . (NonLocal n)
                     => BwdPass M n f
                     -> Graph n C C
                     -> (forall e x. n e x-> Fact x f -> res -> res)
                     -> res -> res

foldDataflowFactsBwd pass graph func init = 
  -- fst takes the DG, drops the state
  let
      -- partly copied from afrgraph
      block :: forall e x. Block n e x 
            -> Fact x f -> res -> (f, res)
      block (BFirst  n)   = node n
      block (BMiddle n)   = node n
      block (BLast   n)   = node n
      block (BCat b1 b2)  = block b1 `cat` block b2
      block (BHead h n)   = block h  `cat` node n
      block (BTail n t)   = node  n  `cat` block t
      block (BClosed h t) = block h  `cat` block t
                           
      node :: forall e x. (ShapeLifter e x) => n e x 
           -> Fact x f -> res -> (f, res)
      node n f acc = (btransfer pass n f, func n f acc) -- apply transfer function and accum function func

{-      cat :: (f -> res -> (f,res))
          -> (f -> res -> (f,res))
          -> (f -> res -> (f,res)) sort of -}
      cat t1 t2 f acc = let (f', acc') = t2 f acc
                        in t1 f' acc'

        
      (undg, fb, _) = runSimpleUniqueMonad $ runWithFuel infiniteFuel
                      $ analyzeAndRewriteBwd pass entries graph (mapSingleton mainlab bottom)

      undgBody = case undg of
                   GMany _ body _ -> body -- this seems like a hack

      mainlab = LIRLabel "main" (-1)
      entries = JustC [mainlab]

      -- dataflow stuff
      bottom = (fact_bot . bp_lattice) pass

      -- does this need to change?
      blocks :: [Block n C C]
      blocks = map snd $ mapToList undgBody
      
  in
    -- CHANGE?
    foldr ($) init (map (\f res -> snd $ f fb res) (map block blocks))



newtype FwdTransfer n f 
  = FwdTransfer3 { getFTransfer3 ::
                     ( n C O -> f -> f
                     , n O O -> f -> f
                     , n O C -> f -> FactBase f
                     ) }

newtype BwdTransfer n f 
  = BwdTransfer3 { getBTransfer3 ::
                     ( n C O -> f          -> f
                     , n O O -> f          -> f
                     , n O C -> FactBase f -> f
                     ) }

mkFactBase :: forall f. DataflowLattice f -> [(Label, f)] -> FactBase f
mkFactBase lattice = foldl add mapEmpty
  where add :: FactBase f -> (Label, f) -> FactBase f
        add map (lbl, f) = mapInsert lbl newFact map
          where newFact = case mapLookup lbl map of
                            Nothing -> f
                            Just f' -> snd $ join lbl (OldFact f') (NewFact f)
                join = fact_join lattice

newtype FwdRewrite m n f   -- see Note [Respects Fuel]
  = FwdRewrite3 { getFwdRewrite3 ::
                    ( n C O -> f -> m (Maybe (Graph n C O, FwdRewrite m n f))
                    , n O O -> f -> m (Maybe (Graph n O O, FwdRewrite m n f))
                    , n O C -> f -> m (Maybe (Graph n O C, FwdRewrite m n f))
                    ) }

newtype BwdRewrite m n f 
  = BwdRewrite3 { getBRewrite3 ::
                    ( n C O -> f          -> m (Maybe (Graph n C O, BwdRewrite m n f))
                    , n O O -> f          -> m (Maybe (Graph n O O, BwdRewrite m n f))
                    , n O C -> FactBase f -> m (Maybe (Graph n O C, BwdRewrite m n f))
                    ) }

{-analyzeAndFwdRewrite
  :: (FuelMonad m, NonLocal n)
  => FwdPass m n f
  -> [Label]          -- entry points
  -> Graph n C C      -- might need to make more polymorphic on shape 
  -> FactBase f       -- input facts
  -> m (Graph n C C, FactBase f)
-}
-- | if the graph being analyzed is open at the entry, there must
--   be no other entry point, or all goes horribly wrong...
analyzeAndRewriteFwd
   :: forall m n f e x entries. (CheckpointMonad m, NonLocal n, LabelsPtr entries)
   => FwdPass m n f
   -> MaybeC e entries
   -> Graph n e x -> Fact e f
   -> m (Graph n e x, FactBase f, MaybeO x f)
analyzeAndRewriteFwd pass entries g f =
  do (rg, fout) <- arfGraph pass (fmap targetLabels entries) g f
     let (g', fb) = normalizeGraph rg
     return (g', fb, distinguishedExitFact g' fout)

distinguishedExitFact :: forall n e x f . Graph n e x -> Fact x f -> MaybeO x f
distinguishedExitFact g f = maybe g
    where maybe :: Graph n e x -> MaybeO x f
          maybe GNil       = JustO f
          maybe (GUnit {}) = JustO f
          maybe (GMany _ _ x) = case x of NothingO -> NothingO
                                          JustO _  -> JustO f

analyzeAndRewriteBwd
   :: (CheckpointMonad m, NonLocal n, LabelsPtr entries)
   => BwdPass m n f
   -> MaybeC e entries -> Graph n e x -> Fact x f
   -> m (Graph n e x, FactBase f, MaybeO e f)
analyzeAndRewriteBwd pass entries g f =
  do (rg, fout) <- arbGraph pass (fmap targetLabels entries) g f
     let (g', fb) = normalizeGraph rg
     return (g', fb, distinguishedEntryFact g' fout)

distinguishedEntryFact :: forall n e x f . Graph n e x -> Fact e f -> MaybeO e f
distinguishedEntryFact g f = maybe g
    where maybe :: Graph n e x -> MaybeO e f
          maybe GNil       = JustO f
          maybe (GUnit {}) = JustO f
          maybe (GMany e _ _) = case e of NothingO -> NothingO
                                          JustO _  -> JustO f
type DG f  = Graph' (DBlock f)
data DBlock f n e x = DBlock f (Block n e x) -- ^ block decorated with fact
-- @ end dg.tex
instance NonLocal n => NonLocal (DBlock f n) where
  entryLabel (DBlock _ b) = entryLabel b
  successors (DBlock _ b) = successors b

dgFact (DBlock f _)  = f
dgBlock (DBlock _ b) = b

--- constructors

dgnil  :: DG f n O O
dgnil  = GNil
dgnilC :: DG f n C C
dgnilC = GMany NothingO emptyBody NothingO
dgSplice  :: NonLocal n => DG f n e a -> DG f n a x -> DG f n e x
dgSplice = U.splice fzCat
  where fzCat :: DBlock f n e O -> DBlock t n O x -> DBlock f n e x
        fzCat (DBlock f b1) (DBlock _ b2) = DBlock f (b1 `U.cat` b2)

---- observers

normalizeGraph :: forall n f e x . NonLocal n => DG f n e x -> (Graph n e x, FactBase f)
normalizeGraph g = (dropFacts g, facts g)
  where
    dropFacts :: DG f n e x -> Graph n e x
    dropFacts GNil = GNil
    dropFacts (GUnit (DBlock f b)) = GUnit b
    dropFacts (GMany e b x) = GMany (fmap dropFact e) (mapMap dropFact b) (fmap dropFact x)
      where
        dropFact :: forall f n e x. DBlock f n e x -> Block n e x
        dropFact (DBlock _ b) = b

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


type Entries e = MaybeC e [Label]

arfGraph :: forall m n f e x .
            (NonLocal n, CheckpointMonad m) => FwdPass m n f -> 
            Entries e -> Graph n e x -> Fact e f -> m (DG f n e x, Fact x f)
arfGraph pass entries = graph
  where
    {- nested type synonyms would be so lovely here
    type ARF  thing = forall e x . thing e x -> f        -> m (DG f n e x, Fact x f)
    type ARFX thing = forall e x . thing e x -> Fact e f -> m (DG f n e x, Fact x f)
    -}
    graph ::              Graph n e x -> Fact e f -> m (DG f n e x, Fact x f)
    block :: forall e x . 
             Block n e x -> f -> m (DG f n e x, Fact x f)
    node :: forall e x . (ShapeLifter e x) 
         => n e x -> f -> m (DG f n e x, Fact x f)
    body  :: [Label] -> LabelMap (Block n C C)
          -> Fact C f -> m (DG f n C C, Fact C f)
                    -- Outgoing factbase is restricted to Labels *not* in
                    -- in the Body; the facts for Labels *in*
                    -- the Body are in the 'DG f n C C'
    cat :: forall e a x f1 f2 f3. 
           (f1 -> m (DG f n e a, f2))
        -> (f2 -> m (DG f n a x, f3))
        -> (f1 -> m (DG f n e x, f3))

    graph GNil            = \f -> return (dgnil, f)
    graph (GUnit blk)     = block blk
    graph (GMany e bdy x) = (e `ebcat` bdy) `cat` exit x
     where
      ebcat :: MaybeO e (Block n O C) -> Body n -> Fact e f -> m (DG f n e C, Fact C f)
      exit  :: MaybeO x (Block n C O)           -> Fact C f -> m (DG f n C x, Fact x f)
      exit (JustO blk) = arfx block blk
      exit NothingO    = \fb -> return (dgnilC, fb)
      ebcat entry bdy = c entries entry
       where c :: MaybeC e [Label] -> MaybeO e (Block n O C)
                -> Fact e f -> m (DG f n e C, Fact C f)
             c NothingC (JustO entry)   = block entry `cat` body (successors entry) bdy
             c (JustC entries) NothingO = body entries bdy
             c _ _ = error "bogus GADT pattern match failure"

    -- Lift from nodes to blocks
    block (BFirst  n)  = node n
    block (BMiddle n)  = node n
    block (BLast   n)  = node n
    block (BCat b1 b2) = block b1 `cat` block b2
    block (BHead h n)  = block h  `cat` node n
    block (BTail n t)  = node  n  `cat` block t
    block (BClosed h t)= block h  `cat` block t

    node n f
     = do { grw <- frewrite pass n f
          ; case grw of
              Nothing -> return ( singletonDG f n
                                , ftransfer pass n f )
              Just (g, rw) ->
                  let pass' = pass { fp_rewrite = rw }
                      f'    = fwdEntryFact n f
                  in  arfGraph pass' (fwdEntryLabel n) g f' }


    -- | Compose fact transformers and concatenate the resulting
    -- rewritten graphs.
    {-# INLINE cat #-} 
    cat ft1 ft2 f = do { (g1,f1) <- ft1 f
                       ; (g2,f2) <- ft2 f1
                       ; return (g1 `dgSplice` g2, f2) }
    arfx :: forall thing x .
            NonLocal thing
         => (thing C x ->        f -> m (DG f n C x, Fact x f))
         -> (thing C x -> Fact C f -> m (DG f n C x, Fact x f))
    arfx arf thing fb = 
      arf thing $ fromJust $ lookupFact (entryLabel thing) $ joinInFacts lattice fb
     where lattice = fp_lattice pass
     -- joinInFacts adds debugging information


                    -- Outgoing factbase is restricted to Labels *not* in
                    -- in the Body; the facts for Labels *in*
                    -- the Body are in the 'DG f n C C'
-- @ start bodyfun.tex
    body entries blockmap init_fbase
      = fixpoint Fwd lattice do_block blocks init_fbase
      where
        blocks  = forwardBlockList entries blockmap
        lattice = fp_lattice pass
        do_block :: forall x. Block n C x -> FactBase f -> m (DG f n C x, Fact x f)
        do_block b fb = block b entryFact
          where entryFact = getFact lattice (entryLabel b) fb
-- @ end bodyfun.tex


arbGraph :: forall m n f e x .
            (NonLocal n, CheckpointMonad m) => BwdPass m n f -> 
            Entries e -> Graph n e x -> Fact x f -> m (DG f n e x, Fact e f)
arbGraph pass entries = graph
  where
    {- nested type synonyms would be so lovely here 
    type ARB  thing = forall e x . thing e x -> Fact x f -> m (DG f n e x, f)
    type ARBX thing = forall e x . thing e x -> Fact x f -> m (DG f n e x, Fact e f)
    -}
    graph ::              Graph n e x -> Fact x f -> m (DG f n e x, Fact e f)
    block :: forall e x . Block n e x -> Fact x f -> m (DG f n e x, f)
    node  :: forall e x . (ShapeLifter e x) 
                       => n e x       -> Fact x f -> m (DG f n e x, f)
    body  :: [Label] -> Body n -> Fact C f -> m (DG f n C C, Fact C f)
    cat :: forall e a x info info' info''.
           (info' -> m (DG f n e a, info''))
        -> (info  -> m (DG f n a x, info'))
        -> (info  -> m (DG f n e x, info''))

    graph GNil            = \f -> return (dgnil, f)
    graph (GUnit blk)     = block blk
    graph (GMany e bdy x) = (e `ebcat` bdy) `cat` exit x
     where
      ebcat :: MaybeO e (Block n O C) -> Body n -> Fact C f -> m (DG f n e C, Fact e f)
      exit  :: MaybeO x (Block n C O)           -> Fact x f -> m (DG f n C x, Fact C f)
      exit (JustO blk) = arbx block blk
      exit NothingO    = \fb -> return (dgnilC, fb)
      ebcat entry bdy = c entries entry
       where c :: MaybeC e [Label] -> MaybeO e (Block n O C)
                -> Fact C f -> m (DG f n e C, Fact e f)
             c NothingC (JustO entry)   = block entry `cat` body (successors entry) bdy
             c (JustC entries) NothingO = body entries bdy
             c _ _ = error "bogus GADT pattern match failure"

    -- Lift from nodes to blocks
    block (BFirst  n)  = node n
    block (BMiddle n)  = node n
    block (BLast   n)  = node n
    block (BCat b1 b2) = block b1 `cat` block b2
    block (BHead h n)  = block h  `cat` node n
    block (BTail n t)  = node  n  `cat` block t
    block (BClosed h t)= block h  `cat` block t

    node n f
      = do { bwdres <- brewrite pass n f
           ; case bwdres of
               Nothing -> return (singletonDG entry_f n, entry_f)
                            where entry_f = btransfer pass n f
               Just (g, rw) ->
                          do { let pass' = pass { bp_rewrite = rw }
                             ; (g, f) <- arbGraph pass' (fwdEntryLabel n) g f
                             ; return (g, bwdEntryFact (bp_lattice pass) n f)} }

    -- | Compose fact transformers and concatenate the resulting
    -- rewritten graphs.
    {-# INLINE cat #-} 
    cat ft1 ft2 f = do { (g2,f2) <- ft2 f
                       ; (g1,f1) <- ft1 f2
                       ; return (g1 `dgSplice` g2, f1) }

    arbx :: forall thing x .
            NonLocal thing
         => (thing C x -> Fact x f -> m (DG f n C x, f))
         -> (thing C x -> Fact x f -> m (DG f n C x, Fact C f))

    arbx arb thing f = do { (rg, f) <- arb thing f
                          ; let fb = joinInFacts (bp_lattice pass) $
                                     mapSingleton (entryLabel thing) f
                          ; return (rg, fb) }
     -- joinInFacts adds debugging information

                    -- Outgoing factbase is restricted to Labels *not* in
                    -- in the Body; the facts for Labels *in*
                    -- the Body are in the 'DG f n C C'
    body entries blockmap init_fbase
      = fixpoint Bwd (bp_lattice pass) do_block blocks init_fbase
      where
        blocks = backwardBlockList entries blockmap
        do_block :: forall x. Block n C x -> Fact x f -> m (DG f n C x, LabelMap f)
        do_block b f = do (g, f) <- block b f
                          return (g, mapSingleton (entryLabel b) f)


backwardBlockList :: (LabelsPtr entries, NonLocal n) => entries -> Body n -> [Block n C C]
-- This produces a list of blocks in order suitable for backward analysis,
-- along with the list of Labels it may depend on for facts.
backwardBlockList entries body = reverse $ forwardBlockList entries body

data ChangeFlag = SomeChange
                | NoChange
                deriving (Show, Eq)

data FixState n f 
  = FS { fsChange :: ChangeFlag
       , fsFBase :: FactBase f
       , fsDG :: DG f n C C
       , fsLabels :: LabelSet
       }

data TxFactBase n f
  = TxFB { tfb_fbase :: FactBase f
         , tfb_rg    :: DG f n C C -- Transformed blocks
         , tfb_cha   :: ChangeFlag
         , tfb_lbls  :: LabelSet }
-- @ end txfb.tex
     -- See Note [TxFactBase invariants]
-- @ start update.tex
updateFact :: DataflowLattice f -> LabelSet
           -> Label -> f -> (ChangeFlag, FactBase f)
           -> (ChangeFlag, FactBase f)
-- See Note [TxFactBase change flag]
updateFact lat lbls lbl new_fact (cha, fbase)
  | NoChange <- cha2     = (cha,        fbase)
  | lbl `setMember` lbls = (SomeChange, new_fbase)
  | otherwise            = (cha,        new_fbase)
  where
    (cha2, res_fact) -- Note [Unreachable blocks]
       = case lookupFact lbl fbase of
           Nothing -> (SomeChange, new_fact_debug)  -- Note [Unreachable blocks]
           Just old_fact -> join old_fact
         where join old_fact = 
                 fact_join lat lbl
                   (OldFact old_fact) (NewFact new_fact)
               (_, new_fact_debug) = join (fact_bot lat)
    new_fbase = mapInsert lbl res_fact fbase
  

data Direction = Fwd | Bwd
fixpoint :: forall m n f. (CheckpointMonad m, NonLocal n)
 => Direction
 -> DataflowLattice f
 -> (Block n C C -> Fact C f -> m (DG f n C C, Fact C f))
 -> [Block n C C]
 -> (Fact C f -> m (DG f n C C, Fact C f))
fixpoint direction lat do_block blocks init_fbase
  = do { tx_fb <- loop init_fbase
       ; return (tfb_rg tx_fb, 
                 map (fst . fst) tagged_blocks 
                    `mapDeleteList` tfb_fbase tx_fb ) }
    -- The successors of the Graph are the the Labels 
    -- for which we have facts and which are *not* in
    -- the blocks of the graph
  where
    tagged_blocks = map tag blocks
    is_fwd = case direction of { Fwd -> True; 
                                 Bwd -> False }
    tag :: NonLocal t => t C C -> ((Label, t C C), [Label])
    tag b = ((entryLabel b, b), 
             if is_fwd then [entryLabel b] 
                        else successors b)
     -- 'tag' adds the in-labels of the block; 
     -- see Note [TxFactBase invairants]

    tx_blocks :: [((Label, Block n C C), [Label])]
              -> TxFactBase n f -> m (TxFactBase n f)
    tx_blocks []              tx_fb = return tx_fb
    tx_blocks (((lbl,blk), in_lbls):bs) tx_fb 
      = tx_block lbl blk in_lbls tx_fb >>= tx_blocks bs
     -- "in_lbls" == Labels the block may 
     --                 _depend_ upon for facts

    tx_block :: Label -> Block n C C -> [Label]
             -> TxFactBase n f -> m (TxFactBase n f)
    tx_block lbl blk in_lbls 
        tx_fb@(TxFB { tfb_fbase = fbase, tfb_lbls = lbls
                    , tfb_rg = blks, tfb_cha = cha })
      | is_fwd && not (lbl `mapMember` fbase)
      = return (tx_fb {tfb_lbls = lbls'})       -- Note [Unreachable blocks]
      | otherwise
      = do { (rg, out_facts) <- do_block blk fbase
           ; let (cha', fbase') = mapFoldWithKey
                                  (updateFact lat lbls) 
                                  (cha,fbase) out_facts
           ; return $
               TxFB { tfb_lbls  = lbls'
                    , tfb_rg    = rg `dgSplice` blks
                    , tfb_fbase = fbase'
                    , tfb_cha = cha' } }
      where
        lbls' = lbls `setUnion` setFromList in_lbls
        

    loop :: FactBase f -> m (TxFactBase n f)
    loop fbase 
      = do { s <- checkpoint
           ; let init_tx :: TxFactBase n f
                 init_tx = TxFB { tfb_fbase = fbase
                                , tfb_cha   = NoChange
                                , tfb_rg    = dgnilC
                                , tfb_lbls  = setEmpty }
           ; tx_fb <- tx_blocks tagged_blocks init_tx
           ; case tfb_cha tx_fb of
               NoChange   -> return tx_fb
               SomeChange 
                 -> do { restart s
                       ; loop (tfb_fbase tx_fb) } }

-- Join all the incoming facts with bottom.
-- We know the results _shouldn't change_, but the transfer
-- functions might, for example, generate some debugging traces.
joinInFacts :: DataflowLattice f -> FactBase f -> FactBase f
joinInFacts (lattice @ DataflowLattice {fact_bot = bot, fact_join = fj}) fb =
  mkFactBase lattice $ map botJoin $ mapToList fb
    where botJoin (l, f) = (l, snd $ fj l (OldFact bot) (NewFact f))

forwardBlockList :: (NonLocal n, LabelsPtr entry)
                 => entry -> Body n -> [Block n C C]
-- This produces a list of blocks in order suitable for forward analysis,
-- along with the list of Labels it may depend on for facts.
forwardBlockList entries blks = postorder_dfs_from blks entries
                        



class ShapeLifter e x where
 singletonG    ::      n e x -> Graph n e x
 singletonDG   :: f -> n e x -> DG f n e x
 fwdEntryFact  :: NonLocal n => n e x -> f -> Fact e f
 fwdEntryLabel :: NonLocal n => n e x -> MaybeC e [Label]
 ftransfer :: FwdPass m n f -> n e x -> f -> Fact x f
 frewrite  :: FwdPass m n f -> n e x 
           -> f -> m (Maybe (Graph n e x, FwdRewrite m n f))
-- @ end node.tex
 bwdEntryFact :: NonLocal n => DataflowLattice f -> n e x -> Fact e f -> f
 btransfer    :: BwdPass m n f -> n e x -> Fact x f -> f
 brewrite     :: BwdPass m n f -> n e x
              -> Fact x f -> m (Maybe (Graph n e x, BwdRewrite m n f))

instance ShapeLifter C O where
  singletonG = gUnitCO . BFirst
  singletonDG f = gUnitCO . DBlock f . BFirst
  fwdEntryFact     n f  = mapSingleton (entryLabel n) f
  bwdEntryFact lat n fb = getFact lat (entryLabel n) fb
  ftransfer (FwdPass {fp_transfer = FwdTransfer3 (ft, _, _)}) n f = ft n f
  btransfer (BwdPass {bp_transfer = BwdTransfer3 (bt, _, _)}) n f = bt n f
  frewrite  (FwdPass {fp_rewrite  = FwdRewrite3  (fr, _, _)}) n f = fr n f
  brewrite  (BwdPass {bp_rewrite  = BwdRewrite3  (br, _, _)}) n f = br n f
  fwdEntryLabel n = JustC [entryLabel n]

instance ShapeLifter O O where
  singletonG = gUnitOO . BMiddle
  singletonDG f = gUnitOO . DBlock f . BMiddle
  fwdEntryFact   _ f = f
  bwdEntryFact _ _ f = f
  ftransfer (FwdPass {fp_transfer = FwdTransfer3 (_, ft, _)}) n f = ft n f
  btransfer (BwdPass {bp_transfer = BwdTransfer3 (_, bt, _)}) n f = bt n f
  frewrite  (FwdPass {fp_rewrite  = FwdRewrite3  (_, fr, _)}) n f = fr n f
  brewrite  (BwdPass {bp_rewrite  = BwdRewrite3  (_, br, _)}) n f = br n f
  fwdEntryLabel _ = NothingC

instance ShapeLifter O C where
  singletonG = gUnitOC . BLast
  singletonDG f = gUnitOC . DBlock f . BLast
  fwdEntryFact   _ f = f
  bwdEntryFact _ _ f = f
  ftransfer (FwdPass {fp_transfer = FwdTransfer3 (_, _, ft)}) n f = ft n f
  btransfer (BwdPass {bp_transfer = BwdTransfer3 (_, _, bt)}) n f = bt n f
  frewrite  (FwdPass {fp_rewrite  = FwdRewrite3  (_, _, fr)}) n f = fr n f
  brewrite  (BwdPass {bp_rewrite  = BwdRewrite3  (_, _, br)}) n f = br n f
  fwdEntryLabel _ = NothingC

-- Fact lookup: the fact `orelse` bottom
getFact  :: DataflowLattice f -> Label -> FactBase f -> f
getFact lat l fb = case lookupFact l fb of Just  f -> f
                                           Nothing -> fact_bot lat



{-  Note [Respects fuel]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}
-- $fuel
-- A value of type 'FwdRewrite' or 'BwdRewrite' /respects fuel/ if 
-- any function contained within the value satisfies the following properties:
--
--   * When fuel is exhausted, it always returns 'Nothing'.
--
--   * When it returns @Just g rw@, it consumes /exactly/ one unit
--     of fuel, and new rewrite 'rw' also respects fuel.
--
-- Provided that functions passed to 'mkFRewrite', 'mkFRewrite3', 
-- 'mkBRewrite', and 'mkBRewrite3' are not aware of the fuel supply,
-- the results respect fuel.
--
-- It is an /unchecked/ run-time error for the argument passed to 'wrapFR',
-- 'wrapFR2', 'wrapBR', or 'warpBR2' to return a function that does not respect fuel.
