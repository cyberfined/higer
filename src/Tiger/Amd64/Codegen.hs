{-# OPTIONS_GHC -fmax-pmcheck-models=500 #-}
{-# LANGUAGE DataKinds #-}

module Tiger.Amd64.Codegen (genStmt) where

import           Control.Monad       (foldM)
import           Data.Bits           (Bits (..), countLeadingZeros, finiteBitSize)
import           Data.Foldable       (foldrM)
import           Data.Int            (Int8)
import           Data.Proxy          (Proxy (..))
import           GHC.Stack           (HasCallStack)

import           Tiger.Amd64.Assem
import           Tiger.Codegen.Assem (TempReg (..))
import           Tiger.DList
import           Tiger.Frame         (wordSize)
import           Tiger.IR            hiding (Add, Call, Const, Label, Mul, Name,
                                      Relop (..), Ret, Sub, Temp, Xor)
import           Tiger.Temp          (MonadTemp (..))

import qualified Tiger.IR            as IR
import qualified Tiger.Temp          as Temp

genStmt :: forall f m. (HasCallStack, CallingConvention f, MonadTemp m)
        => Proxy f
        -> Stmt
        -> m (DList (Instr (TempReg Reg)))
genStmt prxy = \case
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
    IR.CJump op e1 e2 tLab fLab -> do
        (op1, is1) <- tempOrGen e1
        (op2, is2) <- tempOrGen e2
        let cmp = Cmp (Register op1) (Register op2)
        let jcc = Jcc (relop2Cond op) tLab fLab
        pure $ is1 <> is2 <> fromList [cmp, jcc]

    IR.Jump l -> pure $ singleton $ Jmp l

    IR.Ret -> pure $ singleton Ret
    _ -> error "Statement should be linearized\n"
  where genCall :: Temp.Label -> [Expr] -> m (DList (Instr (TempReg Reg)))
        genCall funName args = do
            let regs = take (length args) $ argsRegisters prxy
            let (regArgs, stackArgs) = splitAt (length regs) args
            is <- foldrM genStackArg empty stackArgs
            (argTemps, is') <- foldM genRegArg ([], is) regArgs
            let go (Left c) r  = Mov (Register $ Reg r) (Const $ fromIntegral c)
                go (Right t) r = Mov (Register $ Reg r) (Register $ Temp t)
            let is2 = fromList $ zipWith go (reverse argTemps) regs
            let call = Call funName (map (Register . Reg) regs)
            let addOp = Const @32 (fromIntegral $ length stackArgs * wordSize prxy)
            let add = [Add (Register $ Reg Rsp) addOp | (not . null) stackArgs]
            pure $ is' <> is2 <> fromList (call : add)


        genRegArg (ts, is) = \case
            IR.Const c -> pure (Left c : ts, is)
            IR.Temp t@Temp.Temp{} -> pure (Right t : ts, is)
            arg -> do
                t <- newTemp
                is' <- genExpr arg (Temp t)
                pure (Right t : ts, is <> is')

        genStackArg arg is = case arg of
            IR.Const c
              | Just c32 <- toIntN c -> pure $ is |> Push (Const @32 c32)
            IR.Temp t -> pure $ is |> Push (Register $ tempToReg t)
            _ -> do
                t <- Temp <$> newTemp
                is' <- genExpr arg t
                pure $ (is <> is') |> Push (Register t)

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

    IR.Binop op e1 e2
      | Just gen <- matchAddr op e1 e2 -> \dst -> do
          (addr, is) <- gen (Just dst)
          let lea = Lea (Register dst) addr
          pure $ is |> lea

    Binop IR.Sub e1 e2 -> \dst -> do
        (op, is1) <- tempOrGen e2
        is2 <- genExpr e1 dst
        let sub = Sub (Register dst) (Register op)
        pure $ (is1 <> is2) |> sub

    Binop IR.Mul (IR.Const s) expr -> constMul s expr
    Binop IR.Mul expr (IR.Const s) -> constMul s expr
    Binop IR.Mul e1 e2 -> \dst -> do
        (op, is1) <- tempOrGen e2
        is2 <- genExpr e1 dst
        let imul = Imul $ Imul2 (Register dst) (Register op)
        pure $ (is1 <> is2) |> imul

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
        (base, is2) <- maybe (tempOrGen baseExpr) (`tempOrDst` baseExpr) mDst
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
            (base, is2) <- maybe (tempOrGen baseExpr) (`tempOrDst` baseExpr) mDst
            let op = AddrRegBaseIndex off (Just $ Base base) (Index idx) scale
            pure (op, is1 <> is2)

        offIdxScale off scale idxExpr mDst = do
            (idx, is) <- maybe (tempOrGen idxExpr) (`tempOrDst` idxExpr) mDst
            let op = AddrRegBaseIndex off Nothing (Index idx) scale
            pure (op, is)

        baseScaleIndex scale baseExpr idxExpr mDst = do
            (idx, is1) <- tempOrGen idxExpr
            (base, is2) <- maybe (tempOrGen baseExpr) (`tempOrDst` baseExpr) mDst
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
    Temp.FP -> Reg Rbx
    Temp.RV -> Reg Rax
    t       -> Temp t

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
    expr -> do
        dst <- Temp <$> newTemp
        instrs <- genExpr expr dst
        pure (dst, instrs)

toIntN :: forall a b. (Bits a, Integral a, Bits b, Bounded b, Integral b) => a -> Maybe b
toIntN x = case (bitSizeMaybe (0 :: a), bitSizeMaybe (0 :: b)) of
    (Just aSize, Just bSize)
      | aSize >= bSize -> if aSize > fromIntegral (maxBound @b)
                             then Nothing
                             else Just $ fromIntegral x
      | otherwise      -> Just $ fromIntegral x
    _                  -> Nothing
