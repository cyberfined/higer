{-# OPTIONS_GHC -fmax-pmcheck-models=500 #-}
{-# LANGUAGE DataKinds #-}

module Tiger.Amd64.Codegen (codegen) where

import           Control.Monad           (foldM, forM)
import           Data.Bits               (Bits (..), countLeadingZeros, finiteBitSize)
import           Data.Foldable           (fold)
import           Data.HashMap.Strict     (HashMap)
import           Data.Int                (Int8)
import           Data.Proxy              (Proxy (..))
import           GHC.Stack               (HasCallStack)

import           Tiger.Amd64.Assem.Types
import           Tiger.Codegen           (TempReg (..))
import           Tiger.DList
import           Tiger.Frame             (Access (..), frameArgs, frameName, wordSize)
import           Tiger.IR.Types          hiding (Add, Call, Const, Label, Mul, Name,
                                          Relop (..), Ret, Sub, Temp, Xor)
import           Tiger.Temp              (MonadTemp (..))

import qualified Data.Graph.Inductive    as Graph
import qualified Data.HashMap.Strict     as HashMap

import qualified Tiger.IR                as IR
import qualified Tiger.Temp              as Temp

codegen :: forall f m. (HasCallStack, CallingConvention f, MonadTemp m)
        => IRData (ControlFlowGraph Stmt) f
        -> m (IRData (ControlFlowGraph (Instr (TempReg Reg))) f)
codegen ir@IRData{..} = do
    funcs' <- forM irFunctions $ \func@IRFunction{..} -> do
        let assignArg is (a:as) (r:rs) = case a of
                InReg t ->
                    let mov = Mov (Register $ tempToReg t) (Register $ Reg r)
                    in assignArg (is |> mov) as rs
                _       -> assignArg is as (r:rs)
            assignArg is _ _ = is
        let argsInstrs = assignArg empty (frameArgs irFuncFrame) (argsRegisters prxy)

        let (mFirstNode, nodes) = extract ((==0) . fst)
                                $ Graph.labNodes (cfgGraph irFuncBody)
        (firstNode, shouldInsertNode) <- case mFirstNode of
            Nothing -> error "Zero node not found"
            Just (node, block@Block{..}) -> do
                stmts' <- fold <$> mapM (genStmt prxy funcsMap) blockStmts
                case blockStmts of
                    (IR.Label{}:_) ->
                        let block' = block { blockStmts = toList stmts' }
                        in pure ((node, block'), True)
                    _ ->
                        let block' = block { blockStmts = toList $ argsInstrs <> stmts' }
                        in pure ((node, block'), False)

        nodes' <- forM nodes $ \(node, block@Block{..}) -> do
            stmts' <- fold <$> mapM (genStmt prxy funcsMap) blockStmts
            pure (node, block { blockStmts = toList stmts' })

        let cfg = Graph.mkGraph (firstNode:nodes') (Graph.labEdges $ cfgGraph irFuncBody)
        let body = if shouldInsertNode
                      then insertFirstCFGNode (irFuncBody { cfgGraph = cfg })
                                              (toList argsInstrs)
                                              (frameName irFuncFrame)
                      else irFuncBody { cfgGraph = cfg }
        pure $ func { irFuncBody = body }
    pure $ ir { irFunctions = funcs' }
  where prxy = Proxy @f
        collectFuncs fs IRFunction{..} = HashMap.insert (frameName irFuncFrame) irFuncFrame fs
        funcsMap = foldl collectFuncs HashMap.empty irFunctions

        extract f xs = let (xs1, xs2) = break f xs
                       in case xs2 of
                           (x:xs2') -> (Just x, xs1 ++ xs2')
                           _        -> (Nothing, xs)

genStmt :: forall f m. (HasCallStack, CallingConvention f, MonadTemp m)
        => Proxy f
        -> HashMap Temp.Label f
        -> Stmt
        -> m (DList (Instr (TempReg Reg)))
genStmt prxy funcsMap = \case
    IR.Label l -> pure $ singleton (Label l)
    IR.Move (IR.Temp dst) (IR.Call funName args) -> do
        is <- genCall funName args
        let mov = Mov (Register $ tempToReg dst) (Register $ Reg Rax)
        pure $ is |> mov
    IR.Expr (IR.Call funName args) -> genCall funName args
    IR.Expr _ -> pure empty
    IR.Move (IR.Temp dst) src -> genExpr src (tempToReg dst)
    IR.Move (IR.Mem (IR.Temp t)) src -> do
        let addr = AddrRegBase (Offset 0) (Base $ tempToReg t)
        movToMem addr src
    IR.Move (IR.Mem (Binop op e1 e2)) src
      | Just gen <- matchAddr op e1 e2 -> do
          (addr, is) <- gen Nothing
          (is<>) <$> movToMem addr src
    IR.Move (IR.Mem e) src -> do
        base <- Temp <$> newTemp
        is <- genExpr e base
        let addr = AddrRegBase (Offset 0) (Base base)
        (is<>) <$> movToMem addr src

    IR.CJump op (Mem addr) expr tLab fLab -> cmpMem (relop2Cond op) addr expr tLab fLab
    IR.CJump op expr (Mem addr) tLab fLab -> cmpMem (relop2Cond op) addr expr tLab fLab
    IR.CJump IR.Eq e1 (IR.Const 0) tLab fLab -> cmpZero Eq e1 tLab fLab
    IR.CJump IR.Eq (IR.Const 0) e2 tLab fLab -> cmpZero Eq e2 tLab fLab
    IR.CJump IR.Ne e1 (IR.Const 0) tLab fLab -> cmpZero Ne e1 tLab fLab
    IR.CJump IR.Ne (IR.Const 0) e2 tLab fLab -> cmpZero Ne e2 tLab fLab
    IR.CJump op (IR.Const c) e2 tLab fLab
      | Just c32 <- toIntN c -> cmpConst (notRelop op) c32 e2 tLab fLab
    IR.CJump op e1 (IR.Const c) tLab fLab
      | Just c32 <- toIntN c -> cmpConst op c32 e1 tLab fLab
    IR.CJump op e1 e2 tLab fLab -> do
        (op1, is1) <- tempOrGen e1
        (op2, is2) <- tempOrGen e2
        let cmp = Cmp (Register op1) (Register op2)
        let jcc = Jcc (relop2Cond op) tLab fLab
        pure $ is1 <> is2 <> fromList [cmp, jcc]

    IR.Jump l -> pure $ singleton $ Jmp l

    IR.Ret -> pure $ singleton Ret
    _ -> error "Statement should be linearized\n"
  where genCall funName args = do
            let (regArgs, stackArgs) = case HashMap.lookup funName funcsMap of
                    Just frame ->
                        let accesses = frameArgs frame
                        in partArgs accesses regs args [] []
                    Nothing -> (zip args regs, [])
            is1 <- foldM (\is e -> (is<>) <$> pushArg e) empty stackArgs
            (is2, is3) <- foldM calcRegArg (empty, []) regArgs
            let incStackSize = Const $ fromIntegral $ length stackArgs * wordSize prxy
            let call = Call funName (map (Register . Reg . snd) regArgs)
            let rest = call : [Add (Register $ Reg Rsp) incStackSize | not $ null stackArgs]
            pure $ is1 <> is2 <> fromList is3 <> fromList rest
          where pushArg = \case
                    IR.Const c
                      | Just c32 <- toIntN c -> pure $ singleton $ Push $ Const c32
                    IR.Temp t -> pure $ singleton $ Push $ Register $ tempToReg t
                    e -> do
                        t <- Temp <$> newTemp
                        is <- genExpr e t
                        pure $ is |> Push (Register t)

                calcRegArg (is, setIs) = \case
                    (IR.Const n, reg) ->
                        let mov = Mov (Register $ Reg reg) $ Const (fromIntegral n)
                        in pure (is, mov : setIs)
                    (IR.Name l, reg) ->
                        let mov = Mov (Register $ Reg reg) $ Name l
                        in pure (is, mov : setIs)
                    (IR.Temp t, reg) ->
                        let mov = Mov (Register $ Reg reg) (Register $ tempToReg t)
                        in pure (is, mov : setIs)
                    (e, reg) -> do
                        t <- Temp <$> newTemp
                        is' <- genExpr e t
                        let mov = Mov (Register $ Reg reg) (Register t)
                        pure (is <> is', mov : setIs)

                partArgs (a:as) (r:rs) (e:es) regArgs stackArgs = case a of
                    InReg{} -> partArgs as rs es ((e, r):regArgs) stackArgs
                    _       -> partArgs as (r:rs) es regArgs (e:stackArgs)
                partArgs (_:as) [] (e:es) regArgs stackArgs =
                    partArgs as [] es regArgs (e:stackArgs)
                partArgs _ _ _ regArgs stackArgs = (regArgs, stackArgs)

                regs = argsRegisters prxy

        movToMem addr src = do
            (mov, is) <- case src of
                IR.Temp t -> pure (Mov addr (Register $ tempToReg t), empty)
                IR.Const n
                  | Just n32 <- toIntN n -> pure (Mov addr (Const @32 n32), empty)
                _ -> do
                    srcReg <- Temp <$> newTemp
                    (Mov addr (Register srcReg),) <$> genExpr src srcReg
            pure $ is |> mov

        cmpZero cnd expr tLab fLab = do
            (op, is) <- tempOrGen expr
            let test = Test (Register op) (Register op)
            let jcc = Jcc cnd tLab fLab
            pure $ is <> fromList [test, jcc]

        cmpConst op c expr tLab fLab = do
            (op1, is) <- tempOrGen expr
            let cmp = Cmp (Register op1) (Const c)
            let jcc = Jcc (relop2Cond op) tLab fLab
            pure $ is <> fromList [cmp, jcc]

        -- TODO: cmp const with mem
        cmpMem cnd addrExpr expr tLab fLab = do
            (addr, is1) <- case addrExpr of
                IR.Temp t -> pure (AddrRegBase (Offset 0) (Base $ tempToReg t), empty)
                Binop op e1 e2
                  | Just gen <- matchAddr op e1 e2 -> gen Nothing
                _ -> do
                    base <- Temp <$> newTemp
                    is <- genExpr addrExpr base
                    let op = AddrRegBase (Offset 0) (Base base)
                    pure (op, is)
            (src, is2) <- tempOrGen expr
            let cmp = Cmp addr (Register src)
            let jcc = Jcc cnd tLab fLab
            pure $ is1 <> is2 <> fromList [cmp, jcc]

        relop2Cond :: IR.Relop -> Condition
        relop2Cond = \case
            IR.Eq -> Eq
            IR.Ne -> Ne
            IR.Lt -> Lt
            IR.Le -> Le
            IR.Gt -> Gt
            IR.Ge -> Ge


genExpr :: forall m. (HasCallStack, MonadTemp m)
        => Expr
        -> TempReg Reg
        -> m (DList (Instr (TempReg Reg)))
genExpr = \case
    IR.Temp t -> \dst -> pure $ singleton (Mov (Register dst) (Register $ tempToReg t))
    IR.Const n -> \dst -> pure $ singleton $ Mov (Register dst) (Const $ fromIntegral n)
    IR.Name l -> \dst -> pure $ singleton $ Mov (Register dst) (Name l)

    Binop op e1 e2
      | Just gen <- matchAddr op e1 e2 -> \dst -> do
          (addr, is) <- gen (Just dst)
          let lea = Lea (Register dst) addr
          pure $ is |> lea

    Binop IR.Sub (IR.Const 0) expr -> \dst -> do
        is <- genExpr expr dst
        let neg = Neg (Register dst)
        pure $ is |> neg

    Binop IR.Sub e1 e2 -> \dst -> do
        (op1, is1) <- tempOrGen e2
        is2 <- if op1 == dst
           then do
               (op2, is2) <- newTempGen e1
               let sub = Sub (Register op2) (Register dst)
               let mov = Mov (Register dst) (Register op2)
               pure $ is2 <> fromList [sub, mov]
           else do
               is2 <- genExpr e1 dst
               pure $ is2 |> Sub (Register dst) (Register op1)
        pure $ is1 <> is2

    Binop IR.Mul (IR.Const s) expr -> constMul s expr
    Binop IR.Mul expr (IR.Const s) -> constMul s expr
    Binop IR.Mul e1 e2 -> \dst -> do
        (op1, is1) <- tempOrGen e2
        (op2, is2) <- if op1 == dst
                         then newTempGen e1
                         else (dst,) <$> genExpr e1 dst
        let imul = Imul $ Imul2 (Register op2) (Register op1)
        let is3 = if op2 /= dst
                     then fromList [imul, Mov (Register dst) (Register op2)]
                     else singleton imul
        pure $ is1 <> is2 <> is3

    Binop IR.Div e1 (IR.Const s)
      | Just shft <- mkShift s -> \dst -> do
          is <- genExpr e1 dst
          let sar = Sar (Register dst) (Const shft)
          pure $ is |> sar
    Binop IR.Div e1 e2 -> \dst -> do
        (op, is1) <- tempOrGen e2
        is2 <- genExpr e1 (Reg Rax)
        let idiv = Idiv (Register op)
        let mov = Mov (Register dst) (Register $ Reg Rax)
        pure $ is1 <> is2 <> fromList [Cqo, idiv, mov]

    IR.Mem (IR.Temp t) -> \dst ->
        let addr = AddrRegBase (Offset 0) (Base $ tempToReg t)
            mov = Mov (Register dst) addr
        in pure $ singleton mov
    IR.Mem (IR.Binop op e1 e2)
      | Just gen <- matchAddr op e1 e2 -> \dst -> do
          (addr, is) <- gen (Just dst)
          let mov = Mov (Register dst) addr
          pure $ is |> mov
    IR.Mem e -> \dst -> do
        is <- genExpr e dst
        let addr = AddrRegBase (Offset 0) (Base dst)
        let mov = Mov (Register dst) addr
        pure $ is |> mov
    IR.ESeq{} -> error "ESeq detected, expression should be canonicalized"
    _ -> error "Unimplemented"
  where constMul s expr dst = case toIntN s of
            Just s32 -> do
                (op, is) <- tempOrDst dst expr
                let imul = Imul $ Imul3 (Register dst) (Register op) (Const s32)
                pure $ is |> imul
            Nothing -> do
                is <- genExpr expr dst
                op <- Temp <$> newTemp
                let mov = Mov (Register op) (Const $ fromIntegral s)
                let imul = Imul $ Imul2 (Register dst) (Register op)
                pure $ is <> fromList [mov, imul]

        mkShift :: Int -> Maybe Int8
        mkShift x
          | x .&. (x - 1) == 0 = Just
                               $ fromIntegral
                               $ finiteBitSize x - 1 - countLeadingZeros x
          | otherwise          = Nothing

matchAddr :: forall m. (HasCallStack, MonadTemp m)
          => Binop
          -> Expr
          -> Expr
          -> Maybe (  Maybe (TempReg Reg)
                   -> m (Operand (TempReg Reg) 'OpMem, DList (Instr (TempReg Reg)))
                   )
matchAddr = \cases
    IR.Add (IR.Const o) (Binop IR.Add baseExpr (Binop IR.Mul (IR.Const s) idxExpr))
      |  Just off <- mkOffset o, Just scale <- mkScale s
      -> Just $ offBaseIdxScale off scale baseExpr idxExpr
    IR.Add (IR.Const o) (Binop IR.Add baseExpr (Binop IR.Mul idxExpr (IR.Const s)))
      |  Just off <- mkOffset o, Just scale <- mkScale s
      -> Just $ offBaseIdxScale off scale baseExpr idxExpr
    IR.Add (IR.Const o) (Binop IR.Add (Binop IR.Mul (IR.Const s) idxExpr) baseExpr)
      |  Just off <- mkOffset o, Just scale <- mkScale s
      -> Just $ offBaseIdxScale off scale baseExpr idxExpr
    IR.Add (Binop IR.Add baseExpr (Binop IR.Mul (IR.Const s) idxExpr)) (IR.Const o)
      |  Just off <- mkOffset o, Just scale <- mkScale s
      -> Just $ offBaseIdxScale off scale baseExpr idxExpr
    IR.Add (Binop IR.Add baseExpr (Binop IR.Mul idxExpr (IR.Const s))) (IR.Const o)
      |  Just off <- mkOffset o, Just scale <- mkScale s
      -> Just $ offBaseIdxScale off scale baseExpr idxExpr
    IR.Add (Binop IR.Add (Binop IR.Mul (IR.Const s) idxExpr) baseExpr) (IR.Const o)
      |  Just off <- mkOffset o, Just scale <- mkScale s
      -> Just $ offBaseIdxScale off scale baseExpr idxExpr
    IR.Add (Binop IR.Add (Binop IR.Mul idxExpr (IR.Const s)) baseExpr) (IR.Const o)
      |  Just off <- mkOffset o, Just scale <- mkScale s
      -> Just $ offBaseIdxScale off scale baseExpr idxExpr

    IR.Add (IR.Const o) (Binop IR.Mul (IR.Const s) idxExpr)
      |  Just scale <- mkScale s, Just off <- mkOffset o
      -> Just $ offIdxScale off scale idxExpr
    IR.Add (IR.Const o) (Binop IR.Mul idxExpr (IR.Const s))
      |  Just off <- mkOffset o, Just scale <- mkScale s
      -> Just $ offIdxScale off scale idxExpr
    IR.Add (Binop IR.Mul (IR.Const s) idxExpr) (IR.Const o)
      |  Just off <- mkOffset o, Just scale <- mkScale s
      -> Just $ offIdxScale off scale idxExpr
    IR.Add (Binop IR.Mul idxExpr (IR.Const s)) (IR.Const o)
      |  Just off <- mkOffset o, Just scale <- mkScale s
      -> Just $ offIdxScale off scale idxExpr

    IR.Add (Binop IR.Mul (IR.Const s) idxExpr) baseExpr
      | Just scale <- mkScale s -> Just $ baseScaleIndex scale baseExpr idxExpr
    IR.Add (Binop IR.Mul idxExpr (IR.Const s)) baseExpr
      | Just scale <- mkScale s -> Just $ baseScaleIndex scale baseExpr idxExpr
    IR.Add baseExpr (Binop IR.Mul (IR.Const s) idxExpr)
      | Just scale <- mkScale s -> Just $ baseScaleIndex scale baseExpr idxExpr
    IR.Add baseExpr (Binop IR.Mul idxExpr (IR.Const s))
      | Just scale <- mkScale s -> Just $ baseScaleIndex scale baseExpr idxExpr

    IR.Add (IR.Const o) (Binop IR.Add baseExpr idxExpr)
      | Just off <- mkOffset o -> Just $ offBaseIdxScale off (Scale 1) baseExpr idxExpr
    IR.Add (Binop IR.Add baseExpr idxExpr) (IR.Const o)
      | Just off <- mkOffset o -> Just $ offBaseIdxScale off (Scale 1) baseExpr idxExpr

    IR.Add (IR.Const o) baseExpr
      | Just off <- mkOffset o -> Just $ offBase off baseExpr
    IR.Add baseExpr (IR.Const o)
      | Just off <- mkOffset o -> Just $ offBase off baseExpr

    IR.Add baseExpr idxExpr -> Just $ \mDst -> do
        (idx, is1) <- tempOrGen idxExpr
        (base, is2) <- if Just idx == mDst
                          then newTempGen baseExpr
                          else maybe (tempOrGen baseExpr) (`tempOrDst` baseExpr) mDst
        let op = AddrRegBaseIndex (Offset 0) (Just $ Base base) (Index idx) (Scale 1)
        pure (op, is1 <> is2)

    IR.Sub baseExpr (IR.Const o)
      | Just (Offset off) <- mkOffset o -> Just $ offBase (Offset $ -off) baseExpr

    IR.Mul (IR.Const s) idxExpr
      | Just scale <- mkScale s -> Just $ constMul scale idxExpr
    IR.Mul idxExpr (IR.Const s)
      | Just scale <- mkScale s -> Just $ constMul scale idxExpr

    _ _ _ -> Nothing
  where offBaseIdxScale off scale baseExpr idxExpr mDst = do
            (idx, is1) <- tempOrGen idxExpr
            (base, is2) <- if Just idx == mDst
                              then newTempGen baseExpr
                              else maybe (tempOrGen baseExpr) (`tempOrDst` baseExpr) mDst
            let op = AddrRegBaseIndex off (Just $ Base base) (Index idx) scale
            pure (op, is1 <> is2)

        offIdxScale off scale idxExpr mDst = do
            (idx, is) <- maybe (tempOrGen idxExpr) (`tempOrDst` idxExpr) mDst
            let op = AddrRegBaseIndex off Nothing (Index idx) scale
            pure (op, is)

        baseScaleIndex scale baseExpr idxExpr mDst = do
            (idx, is1) <- tempOrGen idxExpr
            (base, is2) <- if Just idx == mDst
                              then newTempGen baseExpr
                              else maybe (tempOrGen baseExpr) (`tempOrDst` baseExpr) mDst
            let op = AddrRegBaseIndex (Offset 0) (Just $ Base base) (Index idx) scale
            pure (op, is1 <> is2)

        offBase off baseExpr mDst = do
            (base, is) <- maybe (tempOrGen baseExpr) (`tempOrDst` baseExpr) mDst
            let op = AddrRegBase off (Base base)
            pure (op, is)

        constMul scale idxExpr mDst = do
            (dst, is) <- maybe (tempOrGen idxExpr) (`tempOrDst` idxExpr) mDst
            let op = AddrRegBaseIndex (Offset 0) Nothing (Index dst) scale
            pure (op, is)

        mkOffset :: Int -> Maybe Offset
        mkOffset = fmap Offset . toIntN

        mkScale :: Int -> Maybe Scale
        mkScale s
          | s == 1 || s == 2 || s == 4 || s == 8
          = Just $ Scale $ fromIntegral s
          | otherwise
          = Nothing

tempToReg :: Temp.Temp -> TempReg Reg
tempToReg = \case
    Temp.FP -> Reg Rbp
    Temp.RV -> Reg Rax
    t       -> Temp t

newTempGen :: MonadTemp m => Expr -> m (TempReg Reg, DList (Instr (TempReg Reg)))
newTempGen expr = do
    t <- Temp <$> newTemp
    (t,) <$> genExpr expr t

tempOrDst :: MonadTemp m
          => TempReg Reg
          -> Expr
          -> m (TempReg Reg, DList (Instr (TempReg Reg)))
tempOrDst dst = \case
    IR.Temp t -> pure (tempToReg t, empty)
    expr -> do
        instrs <- genExpr expr dst
        pure (dst, instrs)

tempOrGen :: MonadTemp m => Expr -> m (TempReg Reg, DList (Instr (TempReg Reg)))
tempOrGen = \case
    IR.Temp t -> pure (tempToReg t, empty)
    expr -> newTempGen expr

toIntN :: forall a b. (Bits a, Integral a, Bits b, Bounded b, Integral b) => a -> Maybe b
toIntN x = case (bitSizeMaybe (0 :: a), bitSizeMaybe (0 :: b)) of
    (Just aSize, Just bSize)
      | aSize >= bSize -> if aSize > fromIntegral (maxBound @b)
                             then Nothing
                             else Just $ fromIntegral x
      | otherwise      -> Just $ fromIntegral x
    _                  -> Nothing
