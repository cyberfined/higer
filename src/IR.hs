module IR (
    Platform(..),
    Temp(..),
    Label(..),
    FunDec(..),
    IR(..),
    BinOp(..),
    RuntimeCall(..),
    TempInfo(..),
    IRState(..),
    runIRTranslation
    ) where

import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.Maybe(fromMaybe, fromJust, isJust)
import Data.List((\\), intercalate, delete)
import Control.Monad((>=>), mapM, mapM_, replicateM, when, unless, foldM, void)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Ix(range)
import Data.Array(bounds)
import Data.Graph
import qualified LibFuncs as LF
import qualified Absyn as A

class Platform p where
    wordSize :: p -> Int

newtype Temp = T { unTemp :: Int } deriving (Eq, Ord)

instance Show Temp where
    show (T t) = "t" ++ show t

newtype Label = L { unLabel :: Int } deriving (Eq, Ord)

instance Show Label where
    show (L l) = "l" ++ show l

data FunDec = LFunDec String Int [Temp] [IR]                                 -- Linear func: name level args blocks
            | CFunDec String Int [Temp] Graph (Vertex -> ([IR], Int, [Int])) -- CFG func: name level args CFG(graph, nodeFromKey)

instance Show FunDec where
    show fd = case fd of
        LFunDec name nesting args body -> wrap name nesting args (sbody body)
        CFunDec name nesting args cfg nfk -> wrap name nesting args (sgraph nfk cfg)
      where wrap name nesting args body = name ++ '(':sargs ++ ") [level " ++ show nesting ++ "] {\n" ++ body ++ "\n}"
              where sargs = intercalate ", " (map show args)
            sbody = intercalate "\n" . map show
            sgraph nfk = intercalate "\n\n" . map (\i -> let (node,_,_) = nfk i in sbody node) . range . bounds

data IR = Const Int
        | Temp Temp
        | Label Label
        | Assign Temp IR
        | BinOp IR BinOp IR
        | Neg IR
        | Jump Label
        | CJump IR Label Label
        | Load Temp
        | Store Temp IR
        | RuntimeCall RuntimeCall
        | Call String [IR]
        | Ret (Maybe IR)
        | Phi [(IR, Label)]
        deriving (Eq, Ord)

instance Show IR where
    show ir = case ir of
        Const i -> show i
        Temp t -> show t
        Label l -> show l ++ ":"
        Assign t i -> show t ++ " = " ++ show i
        BinOp i1 op i2 -> show i1 ++ ' ':show op ++ ' ':show i2
        Neg i -> "neg " ++ show i
        Jump l -> "jump " ++ show l
        CJump cond l1 l2 -> "cjump " ++ show cond ++ ", iftrue " ++ show l1 ++ ", iffalse " ++ show l2
        Load t -> "load " ++ show t
        Store t v -> "store " ++ show t ++ ", " ++ show v
        RuntimeCall rc -> show rc
        Call f args -> f ++ '(':showArgs args ++ ")"
        Ret mir -> maybe "ret" (\ir -> "ret " ++ show ir) mir
        Phi vals -> "phi " ++ intercalate ", " (map (\(ir,l) -> '[':show ir ++ ", " ++ show l ++ "]") vals)
      where showArgs (a:as@(_:_)) = sarg a ++ ", " ++ showArgs as
            showArgs [a] = sarg a
            showArgs _ = ""
            sarg a = case a of
                Label l -> show l
                _ -> show a

data BinOp = Add
           | Sub
           | Mul
           | Div
           | LShift
           | RShift
           | And
           | Or
           | Xor
           | Eq
           | Ne
           | Gt
           | Ge
           | Lt
           | Le
           deriving (Eq, Ord)

instance Show BinOp where
    show op = case op of
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        LShift -> "<<"
        RShift -> ">>"
        And -> "&"
        Or -> "|"
        Xor -> "^"
        Eq -> "=="
        Ne -> "!="
        Gt -> ">"
        Ge -> ">="
        Lt -> "<"
        Le -> "<="

data RuntimeCall = InitArray IR IR               -- size init
                 | InitRecord [IR]               -- fields
                 | InitString Label              -- label on which string data is placed
                 | ArrLen Temp                   -- temp containing array address
                 | ArrPtr Temp                   -- temp containing array address
                 | RecPtr Temp                   -- temp containing record address
                 | IsInBounds IR IR              -- size index
                 | PanicBounds A.SourcePos IR IR -- position size index
                 | Exit IR                       -- exit code
                 deriving (Eq, Ord)

instance Show RuntimeCall where
    show rc = case rc of
        InitArray sz ini -> "initArray(" ++ show sz ++ ", " ++ show ini ++ ")"
        InitRecord args -> "initRecord(" ++ intercalate "," (map show args) ++ ")"
        InitString l -> "initString(" ++ show l ++ ")"
        ArrLen t -> "arrLen(" ++ show t ++ ")"
        ArrPtr t -> "arrPtr(" ++ show t ++ ")"
        RecPtr t -> "recPtr(" ++ show t ++ ")"
        IsInBounds sz ind -> "isInBounds(" ++ show sz ++ ", " ++ show ind ++ ")"
        PanicBounds _ sz ind -> "panicBounds(" ++ show sz ++ ", " ++ show ind ++ ")"
        Exit code -> "exit(" ++ show code ++ ")"

data TempInfo = Escaped Int Int -- level and number
              deriving (Eq, Show)

data IRState = IRState { nextTemp :: Int                    -- number of next temp
                       , nextLabel :: Int                   -- number of next label
                       , funcRepls :: [M.Map String String] -- old function name to new one
                       , tempRepls :: [M.Map String Temp]   -- temp replacements for variables
                       , tempInfos :: IM.IntMap TempInfo    -- information about temps
                       , escapeNums :: [Int]                -- count of escaping variables
                       , funArgs :: [[Temp]]                -- function arguments
                       , funBodies :: [[IR]]                -- functions bodies
                       , breaks :: [Label]                  -- labels for breaks
                       , lastLabel :: Maybe Label           -- last inserted label
                       , funDecs :: [FunDec]                -- function declarations
                       , prefixes :: [String]               -- function names for renaming nested functions
                       , strings :: M.Map String Label      -- static string data
                       , stWordSize :: Int                  -- word size
                       , curNesting :: Int                  -- cur nesting
                       , maxNesting :: Int                  -- max nesting
                       }

type IRStateT a = StateT IRState Identity a

runIRTranslation :: Platform p => p -> A.Expr -> IRState
runIRTranslation p expr = st{funDecs = map buildCFG $ funDecs st}
  where st = runIdentity $ execStateT mainExpr (initIRState p)
        mainExpr = transExpr expr >>
                   modify (\st -> st{funDecs = LFunDec "main" 0 [] (reverse $ Ret Nothing:head (funBodies st)):funDecs st})

buildCFG :: FunDec -> FunDec
buildCFG (LFunDec fun nesting args body) = CFunDec fun nesting args graph nfk
  where (graph, nfk, _) = graphFromEdges (sStage blockLabels bs curBlock (initNeighs bs curBlock) [] [])
        initNeighs bs curBlock = case bs of
            ((ir:irs):bs) -> case ir of
                Jump _ -> []
                CJump{} -> []
                RuntimeCall (Exit _) -> []
                Ret _ -> []
                _ -> [curBlock-1]
        (blockLabels, bs, curBlock) = fStage IM.empty [[]] 0 body
        fStage blockLabels (b:bs) curBlock (ir:irs) = case ir of
            Label (L i) -> let (bs', curBlock') = case b of
                                                      [] -> ([ir]:bs, curBlock)
                                                      _ -> ([ir]:b:bs, curBlock+1)
                           in fStage (IM.insert i curBlock' blockLabels) bs' curBlock' irs
            Jump _ -> next
            CJump{} -> next
            RuntimeCall (Exit _) -> next
            _ -> fStage blockLabels ((ir:b):bs) curBlock irs
          where next = fStage blockLabels ([]:(ir:b):bs) (curBlock+1) irs
        fStage blockLabels bs curBlock _ = (blockLabels, bs, curBlock)
        sStage blockLabels ((ir:irs):bs) curBlock neighs node nodes = case ir of
            Jump (L i) -> sStage blockLabels (irs:bs) curBlock (neighs' neighs [i]) (ir:node) nodes
            CJump _ (L i1) (L i2) -> sStage blockLabels (irs:bs) curBlock (neighs' neighs [i1,i2]) (ir:node) nodes
            _ -> sStage blockLabels (irs:bs) curBlock neighs (ir:node) nodes
          where neighs' = foldr (\i a -> fromJust (IM.lookup i blockLabels):a)
        sStage blockLabels (_:bs) curBlock neighs node nodes = sStage blockLabels bs (curBlock-1) (initNeighs bs (curBlock-1)) [] ((node, curBlock, neighs):nodes)
        sStage blockLabels _ _ _ _ nodes = nodes

initIRState :: Platform p => p ->  IRState
initIRState p = IRState { nextTemp = 0
                        , nextLabel = 0
                        , funcRepls = [M.empty]
                        , tempRepls = [M.empty]
                        , tempInfos = IM.empty
                        , escapeNums = [0]
                        , funArgs = []
                        , funBodies = [[]]
                        , breaks = []
                        , lastLabel = Nothing
                        , funDecs = []
                        , prefixes = ["main"]
                        , strings = M.empty
                        , stWordSize = wordSize p
                        , curNesting = 0
                        , maxNesting = 0
                        }

newTemp :: IRStateT Temp
newTemp = get >>= \st -> let nt = nextTemp st
                         in put (st{nextTemp = nt+1}) >>
                            return (T nt)

newLabel :: IRStateT Label
newLabel = get >>= \st -> let nl = nextLabel st in put(st{nextLabel = nl+1}) >> return (L nl)

getFRepl :: String -> IRStateT String
getFRepl fn = fromMaybe fn . M.lookup fn . head . funcRepls <$> get

getMTRepl :: String -> IRStateT (Maybe Temp)
getMTRepl var = lookup . tempRepls <$> get
  where lookup (r:reps) = case M.lookup var r of
            Nothing -> lookup reps
            tr -> tr
        lookup _ = Nothing

getTRepl :: String -> IRStateT Temp
getTRepl var = fromJust <$> getMTRepl var

getTInfo :: Temp -> IRStateT (Maybe TempInfo)
getTInfo (T k) = IM.lookup k . tempInfos <$> get

hasLastLabel :: IRStateT Bool
hasLastLabel = isJust . lastLabel <$> get

getLastLabel :: IRStateT Label
getLastLabel = fromJust . lastLabel <$> get

getWordSize :: IRStateT Int
getWordSize = stWordSize <$> get

insertTRepl :: String -> Temp -> IRStateT ()
insertTRepl v t = modify (\st -> let (r:reps) = tempRepls st in st{tempRepls = M.insert v t r:reps})

updateTRepl :: String -> Temp -> IRStateT ()
updateTRepl v t = modify (\st -> st{tempRepls = ins (tempRepls st)})
  where ins (r:reps)
          | M.member v r = M.insert v t r:reps
          | otherwise = r:ins reps
        ins _ = []

insertTInfo :: Temp -> IRStateT ()
insertTInfo (T k) = modify (\st -> let (n:ns) = escapeNums st
                                       cn = curNesting st
                                   in st{ tempInfos = IM.insert k (Escaped cn n) $ tempInfos st
                                        , escapeNums = (n+1):ns
                                        })

insertIR :: IR -> IRStateT ()
insertIR ir = modify (\st -> let (b:bs) = funBodies st
                                 lastLabel' = case ir of
                                     Label l -> Just l
                                     _ -> lastLabel st
                             in st{ funBodies = (ir:b):bs
                                  , lastLabel = lastLabel'
                                  })

insertBreak :: Label -> IRStateT ()
insertBreak br = modify (\st -> st{breaks = br:breaks st})

removeBreak :: IRStateT ()
removeBreak = modify (\st -> st{breaks = tail $ breaks st})

getBreak :: IRStateT Label
getBreak = head . breaks <$> get

incEscapes :: IRStateT ()
incEscapes = modify (\st -> let (e:es) = escapeNums st in st{escapeNums = (e+1):es})

insertString :: String -> IRStateT Label
insertString str = get >>= \st ->
    let ss = strings st
    in case M.lookup str ss of
        Nothing ->
            newLabel >>= \l1 ->
            modify (\st -> st{strings = M.insert str l1 $ strings st}) >>
            return l1
        Just l1 -> return l1

enterFunction :: String -> [(String, A.Type, Bool)] -> IRStateT ()
enterFunction fun args = mapM (const newTemp) args >>= \argTemps ->
    modify (\st -> let (fr:freps) = funcRepls st
                       frepsHead = M.insert fun newName fr
                       tr = M.fromList $ zipWith (\(v,_,_) t -> (v,t)) args argTemps
                       (escArgs, escTemps) = filtEsc args argTemps
                       filtEsc (a@(_,_,e):as) (t:ts)
                         | e = let (as', ts') = filtEsc as ts in (a:as', t:ts')
                         | otherwise = filtEsc as ts
                       filtEsc _ _ = ([],[])
                       tempInfos' = foldr (\(T t, n) ti -> IM.insert t (Escaped curNesting' n) ti) (tempInfos st) $ zip escTemps [1..]
                       prefixes' = fun:prefixes st
                       newName = intercalate "." prefixes'
                       curNesting' = curNesting st + 1
                       maxNesting' = max (maxNesting st) curNesting'
                   in st{ funcRepls = frepsHead:frepsHead:freps
                        , tempRepls = tr:tempRepls st
                        , tempInfos = tempInfos'
                        , escapeNums = length escArgs: escapeNums st
                        , funArgs = argTemps:funArgs st
                        , funBodies = []:funBodies st
                        , prefixes = prefixes'
                        , curNesting = curNesting'
                        , maxNesting = maxNesting'
                        })

leaveFunction :: IRStateT ()
leaveFunction = modify (\st -> let (fun:prefixes') = prefixes st
                                   (body:funBodies') = funBodies st
                                   (a:as') = funArgs st
                                   (fr:funcRepls') = funcRepls st
                                   frepl = fromMaybe fun $ M.lookup fun fr
                                   cnest = curNesting st
                               in st{ funcRepls = funcRepls'
                                    , tempRepls = tail $ tempRepls st
                                    , escapeNums = tail $ escapeNums st
                                    , funArgs = as'
                                    , funBodies = funBodies'
                                    , funDecs = LFunDec frepl cnest a (reverse body):funDecs st
                                    , prefixes = prefixes'
                                    , curNesting = cnest - 1
                                    })

enterLet :: IRStateT ()
enterLet = modify (\st -> let (fr:freps) = funcRepls st
                          in st{ funcRepls = fr:fr:freps
                               , tempRepls = M.empty:tempRepls st
                               , prefixes = "let":prefixes st
                               })

leaveLet :: IRStateT ()
leaveLet = modify (\st -> st{ funcRepls = tail $ funcRepls st
                            , tempRepls = tail $ tempRepls st
                            , prefixes = tail $ prefixes st
                            })

backupVar :: String -> IRStateT (Maybe Temp)
backupVar v = M.lookup v . head . tempRepls <$> get

restoreVar :: String -> Maybe Temp -> IRStateT ()
restoreVar v mtr = modify (\st -> let (r:reps) = tempRepls st
                                      tempRepls' = case mtr of
                                          Nothing -> M.delete v r:reps
                                          Just tr -> M.insert v tr r:reps
                                  in st{tempRepls = tempRepls'})

getGeneratedIR :: IRStateT a -> IRStateT ([IR], a)
getGeneratedIR expr =
    modify (\st -> st{funBodies = []:funBodies st}) >>
    expr >>= \res ->
    get >>= \st ->
    let (irs:rest) = funBodies st
    in put (st{funBodies = rest}) >>
       return (irs, res)

transDec :: A.Dec -> IRStateT ()
transDec dec = case dec of
    A.TypeDec{} -> return ()
    A.VarDec _ (A.TypedVar v _ esc expr) ->
        assignExpr expr >>= \expr' ->
        newTemp >>= \t1 ->
        insertTRepl v t1 >>
        if esc
           then insertTInfo t1 >> incEscapes >> case expr' of
               c@(Const _) -> insertIR (Store t1 c)
               l@(Label _) -> insertIR (Store t1 l)
               _ -> newTemp >>= \t2 ->
                    insertIR (Assign t2 expr') >>
                    insertIR (Store t1 (Temp t2))
           else insertIR (Assign t1 expr')
    A.FunDec _ (A.TypedFun fun args t expr) ->
        enterFunction fun args >>
        body >>
        leaveFunction
      where body = case t of
                A.TUnit -> transExpr expr >> insertIR (Ret Nothing)
                _ -> transValuedExpr expr >>= \expr' ->
                     insertIR (Ret $ Just expr')

transExpr :: A.Expr -> IRStateT ()
transExpr expr = case expr of
    A.Seq _ exs -> mapM_ transExpr exs
    A.Call _ fun args -> getFRepl fun >>= \frepl ->
        mapM transValuedExpr args >>= \args' ->
            insertIR (Call frepl args')
    A.Assign sc lv expr -> assignExpr expr >>= \expr' ->
        case lv of
            A.LId v -> getTRepl v >>= \tr ->
                getTInfo tr >>= \mti -> case mti of
                    Nothing -> newTemp >>= \t1 ->
                               insertIR (Assign t1 expr') >>
                               updateTRepl v t1
                    Just _ -> case expr' of
                        c@(Const _) -> insertIR (Store tr c)
                        _ -> newTemp >>= \t1 ->
                             insertIR (Assign t1 expr') >>
                             insertIR (Store tr (Temp t1))
            _ -> case expr' of
                     Const _ -> transLVal sc (`Store` expr') lv >>= insertIR
                     _ -> newTemp >>= \t1 ->
                          insertIR (Assign t1 expr') >>
                          transLVal sc (\t2 -> Store t2 (Temp t1)) lv >>= insertIR
    A.If{} -> void $ transIf transExpr expr
    A.While _ cond expr ->
        seqAssVars [cond, expr] >>= \assVars ->
        let numVars = length assVars
        in mapM getTRepl assVars >>= \treps ->
           replicateM numVars newTemp >>= \temps ->
           replicateM 4 newLabel >>= \[l1, l2, l3, l4] ->
           insertBreak l4 >>
           insertIR (Label l1) >>
           insertIR (Jump l2) >>
           insertIR (Label l2) >>
           mapM_ (uncurry updateTRepl) (zip assVars temps) >>
           getGeneratedIR (transValuedExpr cond) >>= \(condir, cond') ->
           getGeneratedIR (insertIR (Label l3) >> transExpr expr) >>= \(exprir, _) ->
           getLastLabel >>= \ll ->
           mapM_ (\(v, nt, ft) -> getTRepl v >>= \st -> insertIR (Assign nt $ Phi [(Temp ft, l1), (Temp st, ll)])) (zip3 assVars temps treps) >>
           mapM_ insertIR (reverse condir) >>
           insertIR (CJump cond' l3 l4) >>
           mapM_ insertIR (reverse exprir) >>
           insertIR (Jump l2) >>
           insertIR (Label l4) >>
           mapM_ (uncurry updateTRepl) (zip assVars temps) >>
           removeBreak
    A.For _ v esc from to expr ->
        backupVar v >>= \bak ->
        newTemp >>= \t1 ->
        assignExpr from >>= \from' ->
        transValuedExpr to >>= \to' ->
        insertTRepl v t1 >>
        when esc (insertTInfo t1) >>
        initV t1 from' >>
        getAssVars expr >>= \as1 ->
        let assVars = if esc || elem v as1 then as1 else v:as1
            numTemps = length assVars
        in mapM getTRepl assVars >>= \treps ->
           replicateM numTemps newTemp >>= \temps ->
           replicateM 4 newLabel >>= \[l1, l2, l3, l4] ->
           insertBreak l4 >>
           insertIR (Label l1) >>
           insertIR (Jump l2) >>
           insertIR (Label l2) >>
           mapM_ (uncurry updateTRepl) (zip assVars temps) >>
           getGeneratedIR (cond to') >>= \(condir, cond') ->
           getGeneratedIR (insertIR (Label l3) >> transExpr expr >> incV) >>= \(exprir, _) ->
           getLastLabel >>= \ll ->
           mapM_ (\(v, nt, ft) -> getTRepl v >>= \st -> insertIR (Assign nt $ Phi [(Temp ft, l1), (Temp st, ll)])) (zip3 assVars temps treps) >>
           mapM_ insertIR (reverse condir) >>
           insertIR (CJump cond' l3 l4) >>
           mapM_ insertIR (reverse exprir) >>
           insertIR (Jump l2) >>
           insertIR (Label l4) >>
           mapM_ (uncurry updateTRepl) (zip assVars temps) >>
           removeBreak >>
           restoreVar v bak
      where initV tr from = if esc
                               then insertIR (Store tr from)
                               else insertIR (Assign tr from)
            cond to = if esc
                         then replicateM 2 newTemp >>= \[t1, t2] ->
                              getTRepl v >>= \tr ->
                              insertIR (Assign t1 $ Load tr) >>
                              insertIR (Assign t2 $ BinOp (Temp t1) Lt to) >>
                              return (Temp t2)
                         else newTemp >>= \t1 ->
                              getTRepl v >>= \tr ->
                              insertIR (Assign t1 $ BinOp (Temp tr) Lt to) >>
                              return (Temp t1)
            incV = if esc
                      then replicateM 2 newTemp >>= \[t1, t2] ->
                           getTRepl v >>= \tr ->
                           insertIR (Assign t1 $ Load tr) >>
                           insertIR (Assign t2 $ BinOp (Temp t1) Add (Const 1)) >>
                           insertIR (Store tr (Temp t2))
                      else newTemp >>= \t1 ->
                           getTRepl v >>= \tr ->
                           insertIR (Assign t1 $ BinOp (Temp tr) Add (Const 1)) >>
                           updateTRepl v t1
    A.Break _ -> getBreak >>= \l1 -> insertIR (Jump l1)
    A.Let _ decs expr ->
        enterLet >>
        mapM_ transDec decs >>
        transExpr expr >>
        leaveLet
    _ -> return ()

transValuedExpr :: A.Expr -> IRStateT IR
transValuedExpr e = assignExpr e >>= \e' -> case e' of
    t@(Temp _) -> return t
    c@(Const _) -> return c
    e' -> newTemp >>= \t1 ->
          insertIR (Assign t1 e') >>
          return (Temp t1)

assignExpr :: A.Expr -> IRStateT IR
assignExpr expr = case expr of
    A.LVal sc lv -> transLVal sc Load lv
    A.Nil _ -> return (Const 0)
    A.Seq _ exs -> assignSeq exs
    A.IntLit _ i -> return (Const i)
    A.StrLit _ s -> RuntimeCall . InitString <$> insertString s
    A.Neg _ expr -> transValuedExpr expr >>= \expr' -> return (Neg expr')
    A.Call _ fun args -> getFRepl fun >>= \frepl ->
        mapM transValuedExpr args >>= \args' ->
            return (Call frepl args')
    A.BinOp _ e1 op e2 -> case op of
        A.Add -> simpleBinOp e1 Add e2
        A.Sub -> simpleBinOp e1 Sub e2
        A.Mul -> simpleBinOp e1 Mul e2
        A.Div -> simpleBinOp e1 Div e2
        A.Eq -> simpleBinOp e1 Eq e2
        A.Ne -> simpleBinOp e1 Ne e2
        A.Gt -> simpleBinOp e1 Gt e2
        A.Ge -> simpleBinOp e1 Ge e2
        A.Lt -> simpleBinOp e1 Lt e2
        A.Le -> simpleBinOp e1 Le e2
        A.And -> assignBool expr
        A.Or -> assignBool expr
      where simpleBinOp e1 irop e2 = BinOp <$> transValuedExpr e1 <*> return irop <*> transValuedExpr e2
            assignBool expr =
                replicateM 3 newLabel >>= \[l1, l2, l3] ->
                transBool expr l1 l2 >>
                insertIR (Label l1) >>
                insertIR (Jump l3) >>
                insertIR (Label l2) >>
                insertIR (Jump l3) >>
                insertIR (Label l3) >>
                return (Phi [(Const 0, l2), (Const 1, l1)])
            transBool expr l1 l2 = case expr of
                A.BinOp _ e1 op e2 -> case op of
                    A.And -> newLabel >>= \l3 ->
                             transBool e1 l3 l2 >>
                             insertIR (Label l3) >>
                             transBool e2 l1 l2
                    A.Or -> newLabel >>= \l3 ->
                            transBool e1 l1 l3 >>
                            insertIR (Label l3) >>
                            transBool e2 l1 l2
                    _ -> restBool expr l1 l2
                _ -> restBool expr l1 l2
            restBool expr l1 l2 = transValuedExpr expr >>= \expr' ->
                                  insertIR (CJump expr' l1 l2)
    A.Record _ _ fs -> mapM (transValuedExpr . snd) fs >>= \fs' ->
        return (RuntimeCall $ InitRecord fs')
    A.Array _ _ e1 e2 -> transValuedExpr e1 >>= \e1' -> transValuedExpr e2 >>= \e2' ->
            return (RuntimeCall $ InitArray e1' e2')
    A.If{} -> Phi <$> transIf transValuedExpr expr
    A.Let _ decs expr ->
        enterLet >>
        mapM_ transDec decs >>
        assignExpr expr >>= \expr' ->
        leaveLet >>
        return expr'
  where assignSeq (e:es@(_:_)) = transExpr e >> assignSeq es
        assignSeq [e] = assignExpr e

transLVal :: A.SourcePos -> (Temp -> IR) -> A.LVal -> IRStateT IR
transLVal sc stload lv = case lv of
    A.LId v -> getTRepl v >>= \tr ->
        getTInfo tr >>= \mti -> case mti of
            Nothing -> return (Temp tr)
            Just _ -> return (stload tr)
    A.LDot lv _ off -> transLVal sc Load lv >>= \res -> case res of
        Temp t1 -> fromTemp t1
        ld -> newTemp >>= \t1 ->
              insertIR (Assign t1 ld) >>
              fromTemp t1
      where fromTemp t1 =
                replicateM 2 newTemp >>= \[t2, t3] ->
                getWordSize >>= \ws ->
                insertIR (Assign t2 (RuntimeCall $ RecPtr t1)) >>
                insertIR (Assign t3 $ BinOp (Temp t2) Add (Const $ off*ws)) >>
                return (stload t3)
    A.LArr lv expr -> transLVal sc Load lv >>= \res -> case res of
        Temp t1 -> fromTemp t1
        ld -> newTemp >>= \t1 ->
              insertIR (Assign t1 ld) >>
              fromTemp t1
      where fromTemp t1 =
                replicateM 6 newTemp >>= \[t2, t3, t4, t5, t6, t7] ->
                replicateM 2 newLabel >>= \[l1, l2] ->
                getWordSize >>= \ws ->
                transValuedExpr expr >>= \ind ->
                insertIR (Assign t2 (RuntimeCall $ ArrLen t1)) >>
                insertIR (Assign t3 (RuntimeCall $ IsInBounds (Temp t2) ind)) >>
                insertIR (CJump (Temp t3) l1 l2) >>
                insertIR (Label l2) >>
                insertIR (Assign t4 (RuntimeCall $ PanicBounds sc (Temp t2) ind)) >>
                insertIR (RuntimeCall $ Exit (Temp t4)) >>
                insertIR (Label l1) >>
                insertIR (Assign t5 (RuntimeCall $ ArrPtr t1)) >>
                insertIR (Assign t6 $ BinOp ind Mul (Const ws)) >>
                insertIR (Assign t7 $ BinOp (Temp t5) Add (Temp t6)) >>
                return (stload t7)

transIf :: (A.Expr -> IRStateT a) -> A.Expr -> IRStateT [(a, Label)]
transIf trans expr =
    newLabel >>= \l1 ->
    hasLastLabel >>= \hll ->
    unless hll (newLabel >>= \l2 -> insertIR $ Label l2) >>
    getAssVars expr >>= \assVars ->
    transIf' l1 expr assVars [] >>= \(phis, as) ->
    mapM_ (\(v,ph) -> newTemp >>= \t1 -> insertIR (Assign t1 $ Phi ph) >> updateTRepl v t1) (zip assVars phis) >>
    return as
  where transIf' l1 (A.If _ cond th mel) assVars phis =
            newLabel >>= \l2 ->
            transValuedExpr cond >>= \cond' ->
            initPhis phis assVars >>= \curPhis ->
            get >>= \bakSt ->
            maybe (return l1) (const newLabel) mel >>= \l3 ->
            insertIR (CJump cond' l2 l3) >>
            insertIR (Label l2) >>
            trans th >>= \th' ->
            getLastLabel >>= \ll ->
            insertIR (Jump l1) >>
            insertIR (Label l3) >>
            mapM (getTRepl >=> \tr -> return (Temp tr, ll)) assVars >>= \ph ->
            let newPhis = phis' curPhis ph
            in modify (\st -> st{tempRepls = tempRepls bakSt}) >>
               case mel of
                   Just expr -> transIf' l1 expr assVars newPhis >>= \(phis'', as) -> return (phis'', (th',ll):as)
                   Nothing -> return (newPhis, [(th',ll)])
        transIf' l1 expr assVars phis =
            trans expr >>= \expr' ->
            getLastLabel >>= \ll ->
            mapM (getTRepl >=> \tr -> return (Temp tr, ll)) assVars >>= \ph ->
            let newPhis = phis' phis ph
            in insertIR (Jump l1) >>
               insertIR (Label l1) >>
               return (map init newPhis, [(expr',ll)])
        phis' (p:ps) (i:is) = (i:p):phis' ps is
        phis' _ _ = []
        initPhis phis assVars
          | null phis = getLastLabel >>= \initL -> mapM (getTRepl >=> \tr -> return [(Temp tr, initL)]) assVars
          | otherwise = return phis

getAssVars :: A.Expr -> IRStateT [String]
getAssVars expr = case expr of
    A.Seq _ exs -> seqAssVars exs
    A.Neg _ expr -> getAssVars expr
    A.Call _ _ args -> seqAssVars args
    A.BinOp _ e1 _ e2 -> seqAssVars [e1, e2]
    A.Record _ _ fs -> seqAssVars $ map snd fs
    A.Array _ _ e1 e2 -> seqAssVars [e1, e2]
    A.Assign _ (A.LId v) expr ->
        getAssVars expr >>= \as ->
        getMTRepl v >>= \mtr -> case mtr of
            Just tr -> getTInfo tr >>= \mti -> case mti of
                Nothing -> return $ if elem v as then as else v:as
                Just _ -> return as
            Nothing -> return as
    A.If _ cond th mel -> case mel of
        Nothing -> seqAssVars [cond, th]
        Just el -> seqAssVars [cond, th, el]
    A.While _ cond expr -> seqAssVars [cond, expr]
    A.For _ v _ from to expr -> delete v <$> seqAssVars [from, to, expr]
    A.Let _ decs expr ->
        let (exs, vs) = decsExpr decs
        in seqAssVars (expr:exs) >>= \as ->
           return (as \\ vs)
    _ -> return []
  where decsExpr (d:ds) = case d of
            A.VarDec _ (A.TypedVar v _ _ expr) ->
                let (exs, vs) = decsExpr ds
                in (expr:exs, v:vs)
            _ -> decsExpr ds
        decsExpr _ = ([], [])

seqAssVars :: [A.Expr] -> IRStateT [String]
seqAssVars = foldM (\as e -> foldl (\as a -> if elem a as then as else a:as) as <$> getAssVars e) []
