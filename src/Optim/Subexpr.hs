module Optim.Subexpr (
    subExprElim
    ) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class(lift)
import Data.Functor.Identity
import Data.Maybe(isJust, fromJust, fromMaybe)
import Data.Bits((.&.), (.|.), unsafeShiftL, unsafeShiftR, xor)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.Graph
import Control.Monad((>=>), mapM, foldM)
import Data.Foldable(find)
import IR hiding(strings, sideEffectFuncs)
import qualified IR

data ConstValue = IntConst Int       -- t = 10
                | TempAlias Temp     -- t1 = t2
                | StrLit String      -- t = InitString(label) or function of string literals
                | ArrSize ConstValue -- t = InitArray(constSize/temp, initValue)

data OptState = OptState { constTemps :: IM.IntMap ConstValue -- temp to constant value
                         , exprTemps :: [M.Map IR Temp]       -- expression to temp
                         , strings :: M.Map String Label      -- static string data (copied from IRState)
                         , sideEffectFuncs :: S.Set String    -- set of side-effected functions (copied from IRState)
                         }

initOptState :: IRState -> OptState
initOptState irst = OptState { constTemps = IM.empty
                             , exprTemps = []
                             , strings = IR.strings irst
                             , sideEffectFuncs = IR.sideEffectFuncs irst
                             }

rollOptState :: OptState -> OptState
rollOptState ost = ost { constTemps = IM.empty
                       , exprTemps = []
                       }

type OptStateT a = StateT OptState (ExceptT String Identity) a

insertConst :: Temp -> IR -> OptStateT ()
insertConst (T t) ir = modify (\st -> st{constTemps = IM.insert t (cv st) $ constTemps st})
  where cv st = case ir of
            Const i -> IntConst i
            Temp t -> TempAlias t
            RuntimeCall rc -> case rc of
                InitString l -> StrLit $ fst $ fromJust $ find ((== l) . snd) $ M.assocs (strings st)
                InitArray sz _ -> case sz of
                    Const sz -> ArrSize (IntConst sz)
                    Temp sz -> ArrSize (TempAlias sz)

insertExpr :: IR -> Temp -> OptStateT ()
insertExpr k v = modify (\st -> let (ex:es) = exprTemps st
                                in st{exprTemps = M.insert k v ex:es})

getConstValue :: Temp -> OptStateT (Maybe ConstValue)
getConstValue (T t) = IM.lookup t . constTemps <$> get

getConstIR :: Temp -> OptStateT (Maybe IR)
getConstIR t = (=<<) cnv <$> getConstValue t
  where cnv cv = case cv of
            IntConst i -> return $ Const i
            TempAlias t -> return $ Temp t
            _ -> Nothing

getArrLen :: Temp -> OptStateT (Maybe IR)
getArrLen t = getConstValue t >>= \mcv -> case mcv of
    Just (ArrSize sz) -> case sz of
        IntConst sz -> return (return $ Const sz)
        TempAlias sz -> return (return $ Temp sz)
    _ -> return Nothing

getExpr :: IR -> OptStateT (Maybe Temp)
getExpr ir = lookup . exprTemps <$> get
  where lookup (e:es) = case M.lookup ir e of
            t@(Just _) -> t
            _ -> lookup es
        lookup _ = Nothing

enterBlock :: OptStateT ()
enterBlock = modify (\st -> st{exprTemps = M.empty:exprTemps st})

leaveBlock :: OptStateT ()
leaveBlock = modify (\st -> st{exprTemps = tail $ exprTemps st})

isSEFunc :: String -> OptStateT Bool
isSEFunc func = S.member func . sideEffectFuncs <$> get

subExprElim :: IRState -> Either String IRState
subExprElim irst =
    runIdentity (runExceptT $ evalStateT stateM $ initOptState irst) >>= \(fd, strings') ->
    return irst{ funDecs = fd
               , IR.strings = strings'
               }
  where stateM =
            mapM (subexprElimM >=> (\fd -> modify rollOptState >> return fd)) (funDecs irst) >>= \fd ->
            get >>= \st ->
            return (fd, strings st)

subexprElimM :: FunDec -> OptStateT FunDec
subexprElimM fd =
    subexprElim' (nodes fd) (dfs (dtree fd) 0) >>= \nodes' ->
    return fd{nodes = nodes'}
  where subexprElim' nodes (Branch r rest) =
            enterBlock >>
            mapM optStmt (node nodes r) >>= \node' ->
            foldM subexprElim' nodes rest >>= \nodes' ->
            leaveBlock >>
            return (updateNode nodes' r (const node'))

optStmt :: IR -> OptStateT IR
optStmt (Assign t e) = optIR e >>= \e' -> case e' of
    Const _ -> insertConst t e' >> return (Assign t e')
    Temp _ -> insertConst t e' >> return (Assign t e')
    BinOp{} -> assignLookup t e'
    Neg _ -> assignLookup t e'
    Load _ -> return (Assign t e')
    RuntimeCall rc -> case rc of
        InitArray{} -> insertConst t e' >> return (Assign t e')
        InitString{} -> insertConst t e' >> return (Assign t e')
        ArrLen _ -> assignLookup t e'
        ArrPtr _ -> assignLookup t e'
        RecPtr _ -> assignLookup t e'
        IsInBounds{} -> assignLookup t e'
        _ -> return $ Assign t e'
    Call func args ->
        isSEFunc func >>= \isse ->
        if isse
           then return $ Assign t e'
           else assignLookup t e'
    Phi _ -> assignLookup t e'
  where assignLookup t e = getExpr e >>= \mtr -> case mtr of
            Nothing -> insertExpr e t >> return (Assign t e)
            Just tr -> insertConst t (Temp tr) >> return (Assign t $ Temp tr)
optStmt ir = optIR ir

optIR :: IR -> OptStateT IR
optIR ir = case ir of
    Temp t -> fromMaybe ir <$> getConstIR t
    BinOp e1 op e2 ->
        optIR e1 >>= \e1' ->
        optIR e2 >>= \e2' ->
        optBinOp e1' op e2'
    Neg e ->
        optIR e >>= \e' -> case e' of
            Const c -> return $ Const (-c)
            _ -> return $ Neg e'
    CJump e l1 l2 -> optIR e >>= \e' -> return $ CJump e' l1 l2
    Load t ->
        optIR (Temp t) >>= \(Temp t') ->
        return (Load t')
    Store t v ->
        optIR (Temp t) >>= \(Temp t') ->
        optIR v >>= \v' ->
        return (Store t' v')
    RuntimeCall rc -> case rc of
        InitArray e1 e2 -> RuntimeCall .*. InitArray <$> optIR e1 <*> optIR e2
        InitRecord es -> RuntimeCall . InitRecord <$> mapM optIR es
        is@(InitString _) -> return $ RuntimeCall is
        ArrLen t -> optIR (Temp t) >>= \(Temp t') ->
                    fromMaybe (RuntimeCall $ ArrLen t') <$> getArrLen t'
        ArrPtr t -> optIR (Temp t) >>= \(Temp t') -> return (RuntimeCall $ ArrPtr t')
        RecPtr t -> optIR (Temp t) >>= \(Temp t') -> return (RuntimeCall $ RecPtr t')
        IsInBounds e1 e2 ->
            optIR e1 >>= \e1' ->
            optIR e2 >>= \e2' ->
            case (e1', e2') of
                (Const c1, Const c2) -> return $ Const $ fromBool $ c2 >= 0 && c2 < c1
                _ -> return $ RuntimeCall $ IsInBounds  e1' e2'
        PanicBounds sc e1 e2 -> RuntimeCall .*. PanicBounds sc <$> optIR e1 <*> optIR e2
        Exit e -> RuntimeCall . Exit <$> optIR e
    Call f args -> mapM optIR args >>= calcFuncValue f
    Ret mr -> Ret <$> maybe (return Nothing) (fmap Just . optIR) mr
    Phi ps -> mapM (\(e,l) -> optIR e >>= \e' -> return (e',l)) ps >>= \ps' ->
              if allSame (map fst ps')
                 then return $ fst $ head ps'
                 else return $ Phi ps'
    _ -> return ir

optBinOp :: IR -> BinOp -> IR -> OptStateT IR
optBinOp e1 op e2 = case (e1, e2) of
    (Const c1, Const c2) -> case op of
        Div | c2 == 0 -> lift $ throwE "Division by zero"
            | otherwise -> return $ Const (c1 `div` c2)
        _ -> return $ Const (c1 `intop` c2)
    (e1@(Const c1), e2) -> case op of
        Add | c1 == 0 -> return e2
            | otherwise -> return $ BinOp e1 Add e2
        Sub | c1 == 0 -> return $ Neg e2
            | otherwise -> return $ BinOp e1 Sub e2
        Mul | c1 == -1 -> return $ Neg e2
            | c1 == 0 -> return $ Const 0
            | c1 == 1 -> return e2
            | (c1 .&. (c1-1)) == 0 -> return $ BinOp e2 LShift (Const $ log2 c1)
            | otherwise -> return $ BinOp e1 Mul e2
        Div | c1 == 0 -> return $ Const 0
            | otherwise -> return $ BinOp e1 Div e2
        op -> return $ BinOp e1 op e2
    (e1, e2@(Const c2)) -> case op of
        Add | c2 == 0 -> return e1
            | otherwise -> return $ BinOp e2 Add e1
        Sub | c2 == 0 -> return e1
            | otherwise -> return $ BinOp e1 Sub e2
        Mul | c2 == -1 -> return $ Neg e1
            | c2 == 0 -> return $ Const 0
            | c2 == 1 -> return e1
            | (c2 .&. (c2-1)) == 0 -> return $ BinOp e1 LShift (Const $ log2 c2)
            | otherwise -> return $ BinOp e2 Mul e1
        Div | c2 == 0 -> lift $ throwE "Division by zero"
            | c2 == 1 -> return e1
            | (c2 .&. (c2-1)) == 0 -> return $ BinOp e1 RShift (Const $ log2 c2)
            | otherwise -> return $ BinOp e1 Div e2
        op  | isAssoc op -> return $ BinOp e2 op e1
            | otherwise -> return $ BinOp e1 op e2
    (e1, e2) | isAssoc op && e2 < e1 -> return $ BinOp e2 op e1
             | otherwise -> return $ BinOp e1 op e2
  where intop = case op of
          Add -> (+)
          Sub -> (-)
          Mul -> (*)
          LShift -> unsafeShiftL
          RShift -> unsafeShiftR
          And -> (.&.)
          Or -> (.|.)
          Xor -> xor
          Eq -> fromBool .*. (==)
          Ne -> fromBool .*. (/=)
          Gt -> fromBool .*. (>)
          Ge -> fromBool .*. (>=)
          Lt -> fromBool .*. (<)
          Le -> fromBool .*. (<=)
        isAssoc op = case op of
            Add -> True
            Mul -> True
            And -> True
            Or -> True
            Xor -> True
            Eq -> True
            Ne -> True
            _ -> False

calcFuncValue :: String -> [IR] -> OptStateT IR
calcFuncValue func args =
    isSEFunc func >>= \isse ->
-- TODO: Compile-time calculation of pure functions with constant arguments
    if isse || any notConst args
       then return (Call func args)
       else return (Call func args)
  where notConst ir = case ir of
            Const _ -> False
            _ -> True

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (x ==) xs

fromBool :: Bool -> Int
fromBool b = if b then 1 else 0

log2 :: Int -> Int
log2 = truncate . logBase 2 . fromIntegral

(.*.) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.*.) f g x y = f (g x y)
