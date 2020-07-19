module TypeCheck (
    runTypeCheck
    ) where

import qualified Data.Map as M
import Control.Monad(mapM, mapM_)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class(lift)
import Data.Functor.Identity
import qualified LibFuncs as LF
import Absyn

data Symtab = Symtab { types :: M.Map String Type
                     , vars :: M.Map String Type
                     }

initSymtab :: Symtab
initSymtab = Symtab types vars
  where types = M.fromList [("int", TInt), ("string", TString)]
        vars = M.fromList LF.funTypes

data CheckState = CheckState { env :: [Symtab]
                             , inLoop :: Bool
                             }

initCheckState :: CheckState
initCheckState = CheckState [initSymtab] False

type CheckStateT a = StateT CheckState (ExceptT String Identity) a

pushEnv :: CheckStateT ()
pushEnv = modify (\(CheckState (e:es) inLoop) -> CheckState (e:e:es) inLoop)

popEnv :: CheckStateT ()
popEnv = modify (\(CheckState (e:es) inLoop) -> CheckState es inLoop)

lookupType :: String -> CheckStateT (Maybe Type)
lookupType t = M.lookup t . types . head . env <$> get

insertType :: String -> Type -> CheckStateT ()
insertType v t = modify (\(CheckState (e:es) inLoop) -> CheckState ((e{types = M.insert v t $ types e}):es) inLoop)

lookupVType :: String -> CheckStateT (Maybe Type)
lookupVType v = M.lookup v . vars . head . env <$> get

insertVType :: String -> Type -> CheckStateT ()
insertVType v t = modify (\(CheckState (e:es) inLoop) -> CheckState ((e{vars = M.insert v t $ vars e}):es) inLoop)

getInLoop :: CheckStateT Bool
getInLoop = inLoop <$> get

setInLoop :: Bool -> CheckStateT ()
setInLoop v = modify (\(CheckState env _) -> CheckState env v)

withInLoop :: CheckStateT a -> CheckStateT a
withInLoop st = getInLoop >>= \inLoopBak -> setInLoop True >> st >>= \res -> setInLoop inLoopBak >> return res

throwErr :: Posable p => p -> String -> CheckStateT a
throwErr p msg = case position p of
    SourcePos src ln cl -> lift $ throwE $ "Error in " ++ src ++ " at (" ++ show ln ++ ',':show cl ++ "): " ++ msg

wrongType :: Posable p => p -> String -> String -> CheckStateT a
wrongType p t1 t2 = throwErr p $ "wrong type: expected " ++ show t1 ++ " but given " ++ show t2

runTypeCheck :: Expr -> Either String Expr
runTypeCheck expr = fst <$> runIdentity (runExceptT $ evalStateT (checkExpr expr) initCheckState)

checkExpr :: Expr -> CheckStateT (Expr, Type)
checkExpr exp = case exp of
    LVal sc lv -> checkLVal sc lv >>= \(lv', tlv) -> return (LVal sc lv', tlv)
    n@(Nil _) -> return (n, TNil)
    Seq sc exs -> mapM checkExpr exs >>= \exs' -> return (Seq sc (map fst exs'), snd $ last exs')
    i@(IntLit _ _) -> return (i, TInt)
    s@(StrLit _ _) -> return (s, TString)
    n@(Neg sc expr) -> checkExpr expr >>= \(expr', t) -> if t == TInt
                                                            then return (Neg sc expr', t)
                                                            else wrongType expr "int" (show t)
    c@Call{} -> checkCall c
    BinOp sc e1 op e2 -> case op of
        Eq -> eqne
        Ne -> eqne
        _ -> checkExpr e1 >>= \(e1', t1) ->
            if t1 == TInt
               then checkExpr e2 >>= \(e2', t2) ->
                   if t2 == TInt
                      then return (BinOp sc e1' op e2', TInt)
                      else wrongType e2 "int" (show t2)
               else wrongType e1 "int" (show t1)
      where eqne = checkExpr e1 >>= \(e1', t1) ->
              checkExpr e2 >>= \(e2', t2) ->
                  if isValidAssign t1 t2
                     then return (BinOp sc e1' op e2', TInt)
                     else throwErr sc $ "type mismatch in " ++ show op ++ " operation: trying to compare " ++ show t1 ++ " with " ++ show t2
    r@Record{} -> checkRecord r
    Array sc at e1 e2 -> lookupType at >>= \mat -> case mat of
        Nothing -> throwErr sc $ "undefined array type " ++ at
        Just at' -> case at' of
            rett@(TArray _) -> checkExpr e1 >>= \(e1', t1) ->
                if t1 == TInt
                   then checkExpr e2 >>= \(e2', t2) ->
                       if t2 == TInt
                          then return (Array sc at e1' e2', rett)
                          else wrongType e2 "int" (show t2)
                   else wrongType e1 "int" (show t1)
            _ -> throwErr sc $ at ++ " is not an array type"
    Assign sc lv exp -> checkLVal sc lv >>= \(lv', tlv) ->
        checkExpr exp >>= \(exp', texp) ->
            if isValidAssign tlv texp
               then return (Assign sc lv' exp', TUnit)
               else throwErr sc $ "type mismatch in assignment expression: expected " ++ show tlv ++ " but given " ++ show texp
    If sc cond th mel -> checkExpr cond >>= \(cond', tcond) ->
        if tcond == TInt
           then checkExpr th >>= \(th', tth) -> case mel of
               Nothing -> return (If sc cond' th' mel, TUnit)
               Just el -> checkExpr el >>= \(el', tel) ->
                   if isValidAssign tel tth
                      then return (If sc cond' th' (Just el'), tth)
                      else throwErr el $ "type mismatch in then(" ++ show tth ++ ") and else(" ++ show tel ++ ") expressions"
           else throwErr cond $ "type mismatch in if condition: expected int but given " ++ show tcond
    While sc cond th -> checkExpr cond >>= \(cond', tcond) ->
        if tcond == TInt
           then withInLoop (checkExpr th) >>= \(th', tth) -> return (While sc cond' th', TUnit)
           else throwErr cond $ "type mismatch in while condition: expected int but given " ++ show tcond
    For sc v esc from to th -> checkExpr from >>= \(from', tfrom) ->
        if tfrom == TInt
           then checkExpr to >>= \(to', tto) ->
               if tto == TInt
                  then pushEnv >> insertVType v TInt >> withInLoop (checkExpr th) >>= \(th', tth) -> popEnv >> return (For sc v esc from' to' th', TUnit)
                  else throwErr to $ "type mismatch in for to expression: expected int but given " ++ show tto
           else throwErr from $ "type mismatch in for assignment expression: expected int but given " ++ show tfrom
    Break sc -> getInLoop >>= \inLoop ->
        if inLoop
           then return (Break sc, TUnit)
           else throwErr sc "illegal break expression outside the loop"
    Let sc decs exp -> pushEnv >> checkDecs decs >>= \decs' ->
        checkExpr exp >>= \(exp', texp) -> popEnv >> return (Let sc decs' exp', texp)

isValidAssign :: Type -> Type -> Bool
isValidAssign t1 t2 = if t1 == t2
                         then True
                         else case (t1, t2) of
                             (TRecord _ _, TNil) -> True
                             (TNil, TRecord _ _) -> True
                             _ -> False

checkLVal :: Posable p => p -> LVal -> CheckStateT (LVal, Type)
checkLVal sc lv = case lv of
    LId x -> lookupVType x >>= \mt -> case mt of
        Nothing -> throwErr sc $ "undefined variable " ++ x
        Just t -> case t of
            TFunc{} -> throwErr sc $ x ++ " is a function, not a variable"
            _ -> return (LId x, t)
    LDot lv f _ -> checkLVal sc lv >>= \(lv', tlv) -> case tlv of
        r@(TRecord _ fs) -> case lookupField f fs of
            Nothing -> throwErr sc $ "record have no field " ++ f
            Just (n, t) -> case t of
                TRecursive -> return (LDot lv' f n, r)
                _ -> return (LDot lv' f n, t)
        _ -> throwErr sc $ "cannot access field " ++ f ++ " from non-record type"
    LArr lv ind -> checkLVal sc lv >>= \(lv', tlv) -> case tlv of
        TArray t -> checkExpr ind >>= \(ind', tind) ->
            if tind == TInt
               then return (LArr lv' ind', t)
               else wrongType ind "int" (show tind)
        _ -> throwErr sc "cannot index non-array variable"
  where lookupField = lookupField' 0
          where lookupField' n sf ((f,t):fs)
                  | sf == f = Just (n, t)
                  | otherwise = lookupField' (n+1) sf fs
                lookupField' _ _ _ = Nothing

checkCall :: Expr -> CheckStateT (Expr, Type)
checkCall (Call sc fun args) = lookupVType fun >>= \mf -> case mf of
    Nothing -> throwErr sc $ "undefined function " ++ fun
    Just t -> case t of
        TFunc targs tres -> checkNumArgs targs args tres
        _ -> throwErr sc $ fun ++ " is not a function"
  where checkNumArgs targs args tres
          | expNumArgs == givenNumArgs = mapM checkExpr args >>= \st ->
              let args' = map fst st
                  targs' = map snd st
              in checkTypes targs targs' args' >> return (Call sc fun args', tres)
          | otherwise = throwErr sc $ "wrong number of arguments for function call " ++ fun ++ ": expected " ++ show expNumArgs ++ " but given " ++ show givenNumArgs
          where expNumArgs = length targs
                givenNumArgs = length args
        checkTypes (t:ts) (a:as) (e:es)
          | t == a = checkTypes ts as es
          | otherwise = throwErr e $ "wrong argument type in function call " ++ fun ++ ": expected " ++ show t ++ " but given " ++ show a
        checkTypes _ _ _ = return ()

checkRecord :: Expr -> CheckStateT (Expr, Type)
checkRecord (Record sc rec fs) = lookupType rec >>= \mt -> case mt of
    Nothing -> throwErr sc $ "undefined record type " ++ rec
    Just t -> case t of
        rett@(TRecord _ tfs) -> checkTypes fs tfs >>= \fs' -> return (Record sc rec (complFields fs' tfs), rett)
          where checkTypes ((fid,fex):fs) tfs = case lookup fid tfs of
                  Nothing -> throwErr sc $ "record type " ++ rec ++ " have no field " ++ fid
                  Just expt -> checkExpr fex >>= \(fex', tfex) ->
                      if tfex == expt || expt == TRecursive && (tfex == rett || tfex == TNil)
                         then checkTypes fs tfs >>= \fs' -> return ((fid,fex'):fs')
                         else throwErr sc $ "wrong type of expression assigned to record " ++ rec ++ " field " ++ fid ++ ": expected " ++ show expt ++ " but given " ++ show tfex
                checkTypes _ _ = return []
                complFields fs ((t,_):tfs) = case lookup t fs of
                    Nothing -> (t, Nil sc):complFields fs tfs
                    Just v -> (t, v):complFields fs tfs
                complFields _ _ = []
        _ -> throwErr sc $ rec ++ " is not a record type"

checkDecs :: [Dec] -> CheckStateT [Dec]
checkDecs decs = fStage decs [] [] >>= sStage
  where fStage (d:ds) types vars = case d of
          TypeDec sc (UntypedType tid trv) ->
              if elem tid types
                 then throwErr sc $ "redeclaration of type " ++ tid
                 else case trv of
                     TypeId syn -> insertType' tid (TName syn)
                     TypeRecord fs -> insertType' tid (TRecord tid $ map (\(x,t) -> (x,TName t)) fs)
                     TypeArray t -> insertType' tid (TArray (TName t))
            where insertType' tid t = insertType tid t >> fStage ds (tid:types) vars >>= \decs' -> return (TypeDec sc (TypedType tid t):decs')
          v@(VarDec sc (UntypedVar vid _ _)) -> case lookup vid vars of
              Nothing -> fStage ds types ((vid,"variable"):vars) >>= \decs' -> return (v:decs')
              Just vt -> throwErr sc $ "redeclaration of " ++ vt ++ " " ++ vid
          f@(FunDec sc (UntypedFun fid _ _ _)) -> case lookup fid vars of
              Nothing -> fStage ds types ((fid,"function"):vars) >>= \decs' -> return (f:decs')
              Just vt -> throwErr sc $ "redeclaration of " ++ vt ++ " " ++ fid
        fStage _ _ _ = return []
        sStage (d:ds) = case d of
            TypeDec sc (TypedType tid t) -> resolveType sc t [] >>= \t' -> insertType tid t' >> sStage ds >>= \decs' -> return (TypeDec sc (TypedType tid t'):decs')
            VarDec sc (UntypedVar vid mt exp) -> checkExpr exp >>= \(exp', texp) ->
                case mt of
                    Nothing -> if texp == TUnit || texp == TNil
                                  then throwErr sc "assign no valued expression to untyped variable"
                                  else insertVType vid texp >> sStage ds >>= \decs' -> return (VarDec sc (TypedVar vid texp False exp'):decs')
                    Just svt -> lookupType svt >>= \mvt -> case mvt of
                        Nothing -> throwErr sc $ "undefined type " ++ svt
                        Just vt -> if isValidAssign vt texp
                                      then insertVType vid vt >> sStage ds >>= \decs' -> return (VarDec sc (TypedVar vid vt False exp'):decs')
                                      else throwErr sc $ "type mismatch in creation of variable " ++ vid ++ ": expected " ++ show vt ++ " but given " ++ show texp
            FunDec sc (UntypedFun fid args mres exp) -> mapM (\(x,t) -> lookupTypeErr t >>= \t' -> return (x,t',False)) args >>= \args' ->
                maybe (return TUnit) lookupTypeErr mres >>= \res ->
                    pushEnv >> mapM_ (\(x,t,_) -> insertVType x t) args' >>
                        let ftype = TFunc (map (\(_,t,_) -> t) args') res
                        in insertVType fid ftype >> checkExpr exp >>= \(exp', texp) ->
                            if texp == res
                               then popEnv >> insertVType fid ftype >>
                                   sStage ds >>= \decs' -> return (FunDec sc (TypedFun fid args' res exp'):decs')
                               else throwErr exp $ "type mismatch in body of function " ++ fid ++ ": expected " ++ show res ++ " but given " ++ show texp
              where lookupTypeErr t = lookupType t >>= \mt -> case mt of
                      Nothing -> throwErr sc $ "undefined type " ++ t
                      Just t -> return t
        sStage _ = return []
        resolveType sc t visited = case t of
            TInt -> return TInt
            TString -> return TString
            TArray t -> resolveType sc t visited >>= \t' ->
                if isArray t'
                   then throwErr sc "multidimensional arrays aren't supported"
                   else return $ TArray t'
            TRecord rec fs -> TRecord rec <$> mapM (\(x,t) -> (if isRecursiveType rec t then return TRecursive else resolveType sc t []) >>= \t' -> return (x,t')) fs
            TName syn -> if elem syn visited
                            then throwErr sc "cycle in type synonym"
                            else lookupType syn >>= \mt -> case mt of
                                Nothing -> throwErr sc $ "undefined type " ++ syn
                                Just t -> resolveType sc t (syn:visited) >>= \t' ->
                                    insertType syn t' >> return t'
        isRecursiveType n t = case t of
            TName tn -> n == tn
            _ -> False
        isArray t = case t of
            TArray _ -> True
            _ -> False
