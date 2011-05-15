{-# LANGUAGE PatternGuards, GADTs #-}

{- NOTES

When building IGraph:

  - live vars stores all (reg,node) pairs for the current def of reg;
we assume they all lie in the same web (right?)
  - bwd analysis uses fact flowing out of node, fwd uses fact flowing in...


TODO

fix spill/loads

fix enters?

-}

module Decaf.RegisterAllocator.Allocator 
  ( colorRegisters
  )
where

import Decaf.RegisterAllocator.IGraph
import Decaf.RegisterAllocator.ASMLiveVars
import Decaf.RegisterAllocator.ASMReachingDefs
import Decaf.RegisterAllocator.TrivialPass
import Decaf.RegisterAllocator.UnionFind
import Decaf.RegisterAllocator.GraphASM
import Decaf.IR.ASM
import Loligoptl
import Decaf.LIRNodes

import Data.Int

--import Data.Maybe

import Debug.Trace

import qualified Data.Set as S


{- -> attach web information to each line of code

WEBS:
at each line, 
   add each var to the web of its reaching def(s) (a,b -> c --> join(a,c); join (b,c))
   pointer from line to web

build empty graph from webs (and precolored registers?)

TODO
-}

type Key = (ASMReg, ASMNode)

type Web = UnionSet Key -- needs to store the register and which line it's on

type WebNode = Head Key

toKey :: WebNode -> Key
toKey (Head a) = a

-- Interferance graph, cost/coalescing graph
type IGraph = LabelGraph WebNode Color
mkIGraph regs = mkLabelGraph regs
type CGraph = LabelGraph WebNode Int
mkCGraph regs = mkLabelGraph regs

fromJust (Just x) = x
fromJust (Nothing) = error "from just error allocator"



-- I think type is right
makeWebs :: Graph ASMNode' C C -> Web
makeWebs g = foldDataflowFactsFwd trivialPass {-asmReachingDefs-} g webadd emptyUnion
  where 
    -- add a list of operands
    webjoins :: [ASMOperand] -> DefFact -> ASMNode -> Web -> Web
    webjoins ops f n w = webjoins' (concatMap isReg ops) f n w
      where isReg (ASMRegOperand r _) = return r -- == [r]
            isReg _ = []
    webjoins' :: [ASMReg] -> DefFact -> ASMNode -> Web -> Web
    webjoins' [] f n w = w
    webjoins' (reg:regs) f n w = webjoins' regs f n 
                                 $ case getDef reg f of 
                                     Just k -> join (reg, n) (reg,k) w
                                     Nothing -> insert (reg, n) w

    webadd :: ASMNode' e x -> DefFact -> Web -> Web
    webadd n' f = if nmop n then webjoins ops f n else webjoins' [RDX,RAX] f n
      where 
       n = asmDropPrime n'

       nmop n = 
        case n of
         n@(ASMMulNode {}) -> False
         n@(ASMDivNode {}) -> False
         n@(ASMModNode {}) -> False
         otherwise -> True
          
       ops = 
        case n of 
         n@(ASMAddNode i op1 op2) -> [op1,op2]
         n@(ASMSubNode i op1 op2) -> [op1,op2] 
         n@(ASMMovNode i op1 op2) -> [op1,op2]
         n@(ASMCmpNode i op1 op2) -> [op1,op2]
         n@(ASMAndNode i op1 op2) -> [op1,op2]
         n@(ASMOrNode  i op1 op2) -> [op1,op2]
         n@(ASMXorNode i op1 op2) -> [op1,op2]
         n@(ASMShlNode i op1 op2) -> [op1,op2]
         n@(ASMShrNode i op1 op2) -> [op1,op2]
         n@(ASMShraNode i op1 op2) -> [op1,op2]

         n@(ASMPushNode i op) -> [op]
         n@(ASMPopNode  i op) -> [op]
         n@(ASMNegNode  i op) -> [op]
         n@(ASMNotNode  i op) -> [op]
                            
         n@(ASMEnterNode i int) -> []
                          
         n@(ASMLabelNode i lab) -> []

         n@(ASMJmpNode i lab) -> []
         n@(ASMJeNode  i lab) -> []
         n@(ASMJneNode i lab) -> [] 
         n@(ASMJgNode  i lab) -> []
         n@(ASMJgeNode i lab) -> []
         n@(ASMJlNode  i lab) -> []
         n@(ASMJleNode i lab) -> [] 
    -- don't propagate 
         n@(ASMCallNode i sym) -> []
         n@(ASMRetNode  i) ->  []

            

makeIGraph :: Graph ASMNode' C C -> Web -> (IGraph, CGraph) -- Int's represent webs
makeIGraph g us =
  let parts = listPartitions us
      ginit = trace "made label graph" $! mkLabelGraph $ trace ("webs: " ++ (show parts) ++ "\n\nwhole: "++  (show us)) parts
      (g',cg) = (ginit, ginit)
--      (g',cg) = const (trace "live var fold done" $! foldDataflowFactsBwd asmLiveVars g igraphjoin (ginit, ginit)) $! ginit

  in (precolor g',cg)
  where 
    -- finds non-symbolic registers and labels them
    
    precolor ig = labelWithKey colorNonSymb ig
      where
        colorNonSymb :: WebNode -> Maybe Color
        colorNonSymb (Head (r, n)) = 
              case r of
                ASMSREG _ -> Nothing
                physReg -> Just (Reg physReg)


    -- join function for dataflow fold
    igraphjoin :: ASMNode' e x ->  Fact x VarFact -> (IGraph, CGraph) -> (IGraph, CGraph)
    igraphjoin (ASMJmpNode'   i _) = \f -> id
    igraphjoin (ASMJeNode'    i _ _) = \f -> id
    igraphjoin (ASMJneNode'   i _ _) = \f -> id
    igraphjoin (ASMJgNode'    i _ _) = \f -> id
    igraphjoin (ASMJgeNode'   i _ _) = \f -> id
    igraphjoin (ASMJlNode'    i _ _) = \f -> id
    igraphjoin (ASMJleNode'   i _ _) = \f -> id
    igraphjoin (ASMCallNode'  i _ _) = \f -> id
    igraphjoin (ASMRetNode'   i   ) = \f -> id

    igraphjoin n@(ASMLabelNode' i _  ) = igraphjoin' n

    igraphjoin n@(ASMAddNode'   i _ _) = igraphjoin' n
    igraphjoin n@(ASMSubNode'   i _ _) = igraphjoin' n
    igraphjoin n@(ASMMovNode'   i _ _) = igraphjoin' n
    igraphjoin n@(ASMCmpNode'   i _ _) = igraphjoin' n
    igraphjoin n@(ASMAndNode'   i _ _) = igraphjoin' n
    igraphjoin n@(ASMOrNode'    i _ _) = igraphjoin' n
    igraphjoin n@(ASMXorNode'   i _ _) = igraphjoin' n
    igraphjoin n@(ASMShlNode'   i _ _) = igraphjoin' n
    igraphjoin n@(ASMShrNode'   i _ _) = igraphjoin' n
    igraphjoin n@(ASMShraNode'  i _ _) = igraphjoin' n
    igraphjoin n@(ASMMulNode'   i _  ) = igraphjoin' n
    igraphjoin n@(ASMDivNode'   i _  ) = igraphjoin' n
    igraphjoin n@(ASMModNode'   i _  ) = igraphjoin' n
    igraphjoin n@(ASMPushNode'  i _  ) = igraphjoin' n
    igraphjoin n@(ASMPopNode'   i _  ) = igraphjoin' n
    igraphjoin n@(ASMNegNode'   i _  ) = igraphjoin' n
    igraphjoin n@(ASMNotNode'   i _  ) = igraphjoin' n
    igraphjoin n@(ASMEnterNode' i _  ) = igraphjoin' n

                                      
    igraphjoin' :: ASMNode' e O -> VarFact -> (IGraph, CGraph) -> (IGraph, CGraph)
    igraphjoin' n livevars (igraph, cgraph) = (ijoin igraph, cjoin cgraph)
      where
{-        livs = 
          case
    (ASMLabelNode' i o  ) -> (ASMLabelNode i o)
    (ASMAddNode' i op1 op2) -> (ASMAddNode i op1 op2)
    (ASMSubNode' i op1 op2) -> (ASMSubNode i op1 op2)
    (ASMMovNode' i op1 op2) -> (ASMMovNode i op1 op2)
    (ASMCmpNode' i op1 op2) -> (ASMCmpNode i op1 op2)
    (ASMAndNode' i op1 op2) -> (ASMAndNode i op1 op2)
    (ASMOrNode'  i op1 op2) -> (ASMOrNode i op1 op2)
    (ASMXorNode' i op1 op2) -> (ASMXorNode i op1 op2)
    (ASMShlNode' i op1 op2) -> (ASMShlNode i op1 op2)
    (ASMShrNode' i op1 op2) -> (ASMShrNode i op1 op2)
    (ASMShraNode' i op1 op2) -> (ASMShraNode i op1 op2)
    (ASMPushNode' i op) -> (ASMPushNode i op)
    (ASMPopNode'  i op) -> (ASMPopNode i op)
    (ASMNegNode'  i op) -> (ASMNegNode i op)
    (ASMNotNode'  i op) -> (ASMNotNode i op)
    (ASMMulNode' i op) -> (ASMMulNode i op)
    (ASMDivNode' i op) -> (ASMDivNode i op)
    (ASMModNode' i op) -> (ASMModNode i op)
-}
        addConflicts (ASMRegOperand r _) = addConflicts' r igraph
        addConflicts _ = igraph
        addConflicts' :: ASMReg -> IGraph -> IGraph
        addConflicts' r igraph =
          foldr (uncurry addEdge) igraph 
                (map (pair (lift (r, asmDropPrime n) us)) $ map ((flip lift) us) $ S.toList livevars)
                          
                -- ^  pair the current (r,n) with each live var; add edges

        -- add conflicts to igraph
        ijoin :: IGraph -> IGraph
        ijoin igraph = 
          case asmDropPrime n of 
            (ASMLabelNode i lab) -> igraph

            (ASMAddNode i op1 op2) -> addConflicts op1
            (ASMSubNode i op1 op2) -> addConflicts op1
            (ASMMovNode i op1 op2) -> addConflicts op1
            (ASMCmpNode i op1 op2) -> addConflicts op1
            (ASMAndNode i op1 op2) -> addConflicts op1
            (ASMOrNode  i op1 op2) -> addConflicts op1
            (ASMXorNode i op1 op2) -> addConflicts op1
            (ASMShlNode i op1 op2) -> addConflicts op1
            (ASMShrNode i op1 op2) -> addConflicts op1
            (ASMShraNode i op1 op2) -> addConflicts op1
            (ASMMulNode {}) -> addConflicts' RAX $ addConflicts' RDX igraph
            (ASMDivNode {}) -> addConflicts' RAX $ addConflicts' RDX igraph
            (ASMModNode {}) -> addConflicts' RAX $ addConflicts' RDX igraph

            (ASMPushNode i op) -> igraph
            (ASMPopNode  i op) -> addConflicts op
            (ASMNegNode  i op) -> addConflicts op
            (ASMNotNode  i op) -> addConflicts op
            (ASMEnterNode i int) -> igraph

            (ASMJmpNode i lab) -> igraph
            (ASMJeNode  i lab) -> igraph
            (ASMJneNode i lab) -> igraph 
            (ASMJgNode  i lab) -> igraph
            (ASMJgeNode i lab) -> igraph
            (ASMJlNode  i lab) -> igraph
            (ASMJleNode i lab) -> igraph 
            (ASMCallNode i sym) -> igraph
            (ASMRetNode  i) ->  igraph

{-          if mop n then -- mul/div/mod
            foldr (uncurry addEdge) igraph 
                  (concatMap (\rx -> map (curry id $ (rx, n)) (lookupReg r livevars)) [RAX,RDX])
          else if narith n then 
                 igraph
               else
                 case op of
                   (ASMRegOperand r _) -> foldr (uncurry addEdge) igraph 
                                                (map (curry id $ (r,n)) (lookupReg r livevars))
                   otherwise -> igraph-}
        -- add mov links to cgraph
        cjoin :: CGraph -> CGraph
        cjoin cgraph 
          | (ASMMovNode i op1 op2) <- asmDropPrime n
          , (ASMRegOperand r1 _)   <- op1
          , (ASMRegOperand r2 _)   <- op2
          = let n' = asmDropPrime n in -- lol
            addEdge (lift (r1,n') us) (lift (r2,n') us) cgraph -- get the parents in the web

          | otherwise = cgraph
{-        op = 
          case n of
            n@(ASMAddNode i op1 op2) -> op1
            n@(ASMSubNode i op1 op2) -> op1
            n@(ASMMovNode i op1 op2) -> op1
            n@(ASMCmpNode i op1 op2) -> op1
            n@(ASMAndNode i op1 op2) -> op1
            n@(ASMOrNode  i op1 op2) -> op1
            n@(ASMXorNode i op1 op2) -> op1
            n@(ASMShlNode i op1 op2) -> op1
            n@(ASMShrNode i op1 op2) -> op1
            n@(ASMShraNode i op1 op2) -> op1
                                         
                                         
            n@(ASMPushNode i op) -> op
            n@(ASMPopNode  i op) -> op
            n@(ASMNegNode  i op) -> op
            n@(ASMNotNode  i op) -> op
        
        mop n = 
          case n of
            n@(ASMMulNode {}) -> True
            n@(ASMDivNode {}) -> True
            n@(ASMModNode {}) -> True
            otherwise -> False

        narith n = 
          case n of
            (ASMJmpInst   o  ) -> True
            (ASMJeInst    o  ) -> True
            (ASMJneInst   o  ) -> True
            (ASMJgInst    o  ) -> True
            (ASMJgeInst   o  ) -> True
            (ASMJlInst    o  ) -> True
            (ASMJleInst   o  ) -> True
            (ASMLabelInst o  ) -> True
            (ASMCallInst  o  ) -> True
            (ASMEnterInst o  ) -> True
            (ASMRetInst      ) -> True
            otherwise -> False
            
  -}          
-- UTILITY
-- get parent in web; necessary for accessing IGraph
lift :: Key -> Web -> WebNode
lift key us = getParent key us
pair x y = (x, y)
maxL = foldl max 0

type SpillNo = Int

data SpillState = 
  SpillState
  { progL :: [(ASMInst, Int)]
  , toSpill :: [(ASMInst, Maybe Int)]
  , lineCount :: Int 
  }
mkSpillState i = SpillState [] [] i

-- final function
colorRegisters :: ASMProgram -> ASMProgram
colorRegisters prog@(ASMProgram pfls pxts (dsec, tsec)) = 
  let progg = numberListASM prog in 
    loop 0 $ trace (concat $ map ((++"\n") . show) progg) progg
  where
    regs = S.fromList $ map Reg [ RAX, RCX, RDX, RBX, RSI, RDI
                                , R8, R9, R10, R11, R12, R13, R14, R15]

    loop :: SpillNo -> [(ASMInst, Int)] -> ASMProgram
    loop numspills pr = 
      case colorGraph regs $ makeIGraph g us of
        Left spills -> loop (numspills + length spills) $ spill (zip [numspills..] spills) pr
        Right g -> registers numspills g pr
     where
       g = graphASMList pr 
       us = makeWebs g
       -- rewrite spills
       spill ::[(SpillNo,WebNode)] -> [(ASMInst, Int)] -> [(ASMInst, Int)]
       spill spills pr = dospill (mkSpillState $ maxL (map snd pr)) pr
         where 
           n = foldl (\a b -> max a (snd b)) 0 pr -- largest index used
           dospill :: SpillState -> [(ASMInst, Int)] -> [(ASMInst, Int)]
           dospill (SpillState p ((x,mc):xs) c) more = 
             case mc of
               Just l -> dospill (SpillState (p++[(x,l)]) xs c) more
               Nothing -> dospill (SpillState (p++[(x,c)]) xs (c+1)) more
           dospill (SpillState p [] c) ((inst, line):xs) = 
             let slines = map (getASMLine . snd . toKey . snd) spills
                 hits = filter ((== line) . fst) $ zip slines spills -- spill keys on this line
                 newlist = 
                   case hits of
                     hits | length hits == 0 -> [(inst,Just line)]
                          | otherwise -> 
                            let loads :: [(ASMInst, Maybe Int)]
                                loads = concatMap (doLoad . snd) hits --
                                stores :: [(ASMInst, Maybe Int)]
                                stores = concatMap (doStore . snd) hits 
                                -- is reg involved in current inst?
                                doLoad  (off, Head (reg,node)) =
                                  if pairHas reg (getASMSource inst) then load reg off else []
                                doStore (off, Head (reg,node)) = 
                                  if pairHas reg (getASMTarget inst) then store reg off else []

                                load, store :: ASMReg -> Int -> [(ASMInst, Maybe Int)]
                                load reg off = 
                                  [((ASMMovInst (ASMRegOperand reg 8) (memop off)), Nothing)]
                                store reg off =
                                  [((ASMMovInst (memop off) (ASMRegOperand reg 8)), Nothing)]
                                -- +1 needed?
                                memop off  = ASMMemOperand (ASMRegBase RBP) Nothing 
                                             (fromIntegral (-8 * (off+1)) :: ASMInt) 8
                                -- is reg in the tuple?
                                pairHas :: ASMReg -> (Maybe ASMReg, Maybe ASMReg) -> Bool 
                                pairHas reg t = 
                                  case t of
                                    (Just r1, Just r2) -> reg == r1 || reg == r2
                                    (Just r1, Nothing) -> reg == r1
                                    (Nothing, Just r2) -> reg == r2
                                    (Nothing, Nothing) -> False

                            in loads ++ [(inst, Just line)] ++ stores

             in 
               dospill (SpillState p newlist c) xs
           dospill (SpillState p [] c) [] = p



       -- color, then rebuild program
       registers :: Int -> IGraph -> [(ASMInst, Int)] -> ASMProgram
       registers numspills g pr = ASMProgram pfls pxts $
                                  (dsec, ASMTextSection $ castASMList $ map color pr)
             where
               keys' = keys g -- all (reg,node) pairs

               sw :: ASMOperand -> Int -> ASMOperand
               sw (ASMRegOperand reg ehh) i = 
                 ASMRegOperand 
                 (let things = zip3 (map (getASMLine . snd . toKey) keys') -- (line#,reg,key)
                                    (map (fst . toKey) keys') 
                                    (trace (show keys') keys')
                      things' = filter (\(l, r, _) -> (cheatParentLine reg i us) == l 
                                                       && (r == reg)) things
                  in case things' of
                       collist | length collist == 0 -> 
                                 error ("allocator produced no color for: " ++ show (reg,i))
                               | length collist > 1 -> 
                                 error ("allocator produced multiple colors for SREG " ++ show (reg,i))
                               | otherwise ->
                                 case fromJust $ getLabel (third (head collist)) g of
                                   Spill -> error "spill detected in final coloring routine"
                                   Reg reg' -> reg')
                  ehh

               sw op i = op -- not a reg

               third (a,b,c) = c

               color :: (ASMInst, Int) -> ASMInst
               color (inst, i) = 
                 case inst of 
                   (ASMAddInst op1 op2) -> ASMAddInst (sw op1 i) (sw op2 i) 
                   (ASMSubInst op1 op2) -> ASMSubInst (sw op1 i) (sw op2 i)
                   (ASMMovInst op1 op2) -> ASMMovInst (sw op1 i) (sw op2 i)
                   (ASMCmpInst op1 op2) -> ASMCmpInst (sw op1 i) (sw op2 i)
                   (ASMAndInst op1 op2) -> ASMAndInst (sw op1 i) (sw op2 i)
                   (ASMOrInst  op1 op2) -> ASMOrInst  (sw op1 i) (sw op2 i)
                   (ASMXorInst op1 op2) -> ASMXorInst (sw op1 i) (sw op2 i)

                   (ASMShlInst op1 op2)  -> ASMShlInst (sw op1 i) (sw op2 i)
                   (ASMShrInst op1 op2)  -> ASMShrInst (sw op1 i) (sw op2 i)
                   (ASMShraInst op1 op2) -> ASMShraInst (sw op1 i) (sw op2 i)

                   (ASMMulInst op1) -> ASMMulInst (sw op1 i)
                   (ASMDivInst op1) -> ASMDivInst (sw op1 i)
                   (ASMModInst op1) -> ASMModInst (sw op1 i)

                   (ASMPushInst op1) -> ASMPushInst (sw op1 i)
                   (ASMPopInst  op1) -> ASMPopInst  (sw op1 i)
                   (ASMNegInst  op1) -> ASMNegInst  (sw op1 i)
                   (ASMNotInst  op1) -> ASMNotInst  (sw op1 i)

                   (ASMEnterInst _) -> ASMEnterInst (fromIntegral numspills :: Int64) -- kind of bad

                   other -> other -- no operand, don't change



numberListASM :: ASMProgram -> [(ASMInst, Int)]
numberListASM prog = 
  let instructions = case snd (progSections prog) of -- text section
                       (ASMTextSection insts) -> insts
                       otherwise -> error "text and data sections out of order; see Allocator.hs"
  in zip (reverse $ castASMToList instructions) [1..]  



cheatParentLine :: ASMReg -> Int -> Web -> Int
cheatParentLine reg i us = 
  let us' = trace ("cheating " ++ show us ++ show i) 
       (map (\((a,ind),par) -> ((ind,a),par)) $ toList us :: [((Int, Key),Int)]) in 
  case snd $ head $ filter ((== i) . getASMLine . snd . snd . fst) us' of 
    x | x < 0 -> i
      | x > 0 -> getASMLine $ snd $ fromJust $ lookup x (map fst us')

{- this moved to Dataflow.hs

varGraph :: DecafGraph C C -> VarDataGraph
varGraph g = 
  -- fst takes the DG, drops the state
  fst $ runLFM $ (afrGraph pass entries g (mapSingleton mainlab bottom)) >>= fst
  where 
    mainlab = LIRLabel "main" (-1)
    entries = JustC [mainlab]

    pass = liveVarPass
    bottom = (factBottom . fpLattice) pass
-}


{- TODO
make function that gets all facts (from every line of SSA) and returns the list

use list to generate graph

write precoloring code for special registers

coalescing
spill costs?

fix function calls

write spill code (in assembler.hs ?)
-}