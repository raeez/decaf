{-# LANGUAGE PatternGuards #-}

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
import Loligoptl.Dataflow
import Decaf.HooplNodes



type VarDataGraph = DG VarFact Node C C

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

-- Interferance graph, cost/coalescing graph
type IGraph = LabelGraph Key Color
mkIGraph regs = mkLabelGraph regs
type CGraph = LabelGraph Key Int
mkCGraph regs = mkLabelGraph regs


-- I think type is right
makeWebs :: Graph ASMNode C C -> Web
makeWebs g = foldDataFlowFactsFwd DefFact g webadd emptyUnion
  where 
    -- add a list of operands
    webjoins :: [ASMOperand] -> DefFact -> ASMNode -> Web -> Web
    webjoins ops f n w = webjoins' (concatMap isReg ops) f n w
      where isReg (ASMRegOperand r _) = return r -- == [r]
            isReg _ = []
    webjoins' :: [ASMReg] -> DefFact -> ASMNode -> Web -> Web
    webjoins' [] f n w = w
    webjoins' (reg:regs) f n w = webjoins' regs f n $ join (reg,n) (reg,(getDef reg f)) w

    webadd :: ASMNode -> DefFact -> Web -> Web
    webadd n reachingdefs = if nmop n then webjoins ops f n else webjoins' [RDX,RAX] f n
      where 
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
         n@(ASMCallNode i sym -> []
         n@(ASMRetNode  i ->  []

            

makeIGraph :: Graph ASMNode C C -> Web -> (IGraph, CGraph) -- Int's represent webs
makeIGraph g us =
  let g' = snd $ foldDataFlowFactsBwd asmLiveVars g igraphjoin (mkIGraph $ listPartitions us)
  in precolor g'
  where 
    -- finds non-symbolic registers and labels them
    precolor (ig, cg) = (labelWithKey colorNonSymb ig, cg)
      where
        colorNonSymb :: Key -> Maybe ASMReg
        colorNonSymb (r,n) = 
              case r of
                ASMSREG _ -> Nothing
                physReg -> Just (Reg physReg)


    -- join function for dataflow fold
    igraphjoin :: ASMNode e x -> Fact x VarFact -> (IGraph, CGraph) -> (IGraph, CGraph)
    igraphjoin n livevars (igraph, cgraph) = (ijoin igraph, cjoin cgraph)
      where
        ijoin igraph = 
          if mop n then -- mul/div/mod
            foldr (uncurry addEdge) igraph 
                  (concatMap (\rx -> map (curry id $ (rx, n)) (lookupReg r livevars)) [RAX,RDX])
          else if narith n then 
                 igraph
               else
                 case op of
                   (ASMRegOperand r _) -> foldr (uncurry addEdge) igraph 
                                                (map (curry id $ (r,n)) (lookupReg r livevars))
                   otherwise -> igraph
        cjoin cgraph 
          | (ASMMovNode i op1 op2) <- n
          , (ASMRegOperand r1 _)   <- op1
          , (ASMRegOperand r2 _)   <- op2
          = addEdge (lift (r1,n)) (lift (r2,n)) cgraph -- get the parents in the web

          | otherwise = cgraph
        op = 
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
            
            

        lift key = getParent key us


-- final function
colorRegisters :: ASMProgram -> ASMProgram
colorRegisters prog@(ASMProgram pfls pxts (dsec, tsec)) = loop 0 $ numberListASM prog
  where
    loop :: [(ASMInst, Int)] -> Int -> ASMProgram
    loop pr numspills = 
      let g = graphASMList pr in
      case colorGraph regs $ makeIGraph g (makeWebs g) of
        Left spills -> loop (numspills + length spills) $ spill spills pr
        Right g -> registers numspills g pr

    regs = [RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15]

    -- rewrite spills
    spill :: [Key] -> [(ASMInst, Int)] -> [(ASMInst, Int)]
    spill spills pr = (\(x,_,_)->x) $ foldl rewspill ([],n,spills) pr
      where 
        n = foldl (\a b -> max a (snd b)) 0 pr -- largest index used
        rewspill :: ([(ASMInst,Int)],Int,Int) -> (ASMInst, Int) -> ([(ASMInst,Int)],Int)
        rewspill (is,m,count) (inst, line) = 
          let slines = map getASMLine spills
              hits = filter ((== line) . fst) $ zip slines spills -- spill keys on this line
              newlist = 
                case hits of
                  hits | length hits == 0 -> [inst]
                       | otherwise -> 
                         (concatMap doLoad hits) ++ [inst] ++ (concatMap doLoad hits)

              doLoad (reg,node) =
                if pairHas reg (getASMSource inst) then load reg else []
              doStore (reg,node) = 
                if pairhas reg (getASMTarget inst) then store reg else []
                  Nothing -> [inst] -- no spill code
              load reg = []
              store reg = []

              pairHas :: ASMReg -> (Maybe ASMReg, Maybe ASMReg) -- is reg in the tuple?
              pairHas reg t = 
                case t of
                  (Just r1, Just r2) -> reg == r1 || (reg == r2)
                  (Just r1, Nothing) -> reg == r1
                  (Nothing, Just r2) -> reg == r2
                  (Nothing, Nothing) -> False
          in (is++newlist, max m line, if length newlist > 0 then count+1 else count)


    registers :: Int -> IGraph -> [(ASMInst, Int)] -> ASMProgram
    registers numspills g pr = ASMProgram pfls pxts $
                               (dsec, castASMList $ map color pr)
          where
            keys = keys g -- all (reg,node) pairs

            sw :: ASMOperand -> Int -> ASMOperand
            sw (ASMRegOperand reg _) i = 
              ASMRegOperand 
              $ let things  = zip3 (map (getASMLine . snd) keys) -- (line#,reg,key)
                                   (map fst keys) 
                                   keys
                    things' = filter (\(l, r, _) -> (l == i) && (r == reg)) things
                in case things' of
                     collist | length collist == 0 -> 
                               error "allocator produced no color for SREG " ++ show (reg,i)
                             | length collist > 1 -> 
                               error "allocator produced multiple colors for SREG " ++ show (reg,i)
                             | case getLabel (third (head collist)) of
                                 Spill -> error "spill detected in final coloring routine"
                                 Reg reg' -> reg'

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

                (ASMEnterInst _) -> ASMEnterInst numspills -- kind of bad

                other -> other -- no operand, don't change
            


numberListASM :: ASMProgram -> [(ASMInst, Int)]
numberListASM prog = 
  let instructions = snd (progSections prog) -- text section
  in zip (castAsmToList instructions) [1..]


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