module Tiger.IR.Canon
    ( ControlFlowGraph
    , Block(..)
    , canonicalize
    ) where

import           Control.Monad        (forM)
import           Data.HashMap.Strict  (HashMap)
import           Data.HashSet         (HashSet)
import           Data.List.NonEmpty   (NonEmpty (..), (<|))
import           Data.Maybe           (catMaybes, fromJust, maybeToList)

import           Tiger.Frame          (Frame)
import           Tiger.IR.Types
import           Tiger.Temp           (MonadTemp (..))

import qualified Data.Graph.Inductive as Graph
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.HashSet         as HashSet
import qualified Data.List.NonEmpty   as NonEmpty

import qualified Tiger.Frame          as Frame
import qualified Tiger.Temp           as Temp

canonicalize :: (Frame f, MonadTemp m)
             => IRData Stmt f
             -> m (IRData (ControlFlowGraph Stmt) f)
canonicalize IRData{..} = do
    cfgFunctions <- forM irFunctions $ \IRFunction{..} -> do
        let funName = Frame.frameName irFuncFrame
        stmts <- removeLabelAliases <$> linearize irFuncBody
        let blocks = basicBlocks funName stmts
        traces <- reduceBasicBlocks (getTraces blocks)
        let cfg = createControlFlowGraph traces
        pure $ IRFunction { irFuncBody    = cfg
                          , irFuncFrame   = irFuncFrame
                          , irFuncRetType = irFuncRetType
                          }
    pure $ IRData { irStrings   = irStrings
                  , irFunctions = cfgFunctions
                  }

linearize :: MonadTemp m => Stmt -> m [Stmt]
linearize = fmap (linearize' []) . reorderStmt
  where linearize' :: [Stmt] -> Stmt -> [Stmt]
        linearize' stmts = \case
            Seq xs -> foldr (flip linearize') stmts xs
            stmt   -> stmt : stmts

basicBlocks :: Temp.Label -> [Stmt] -> [Block Stmt]
basicBlocks funName = basicBlocks' [] (Block [] funName ZeroNeighs)
  where basicBlocks' blocks block@Block{..} (s:ss) = case s of
            Label l
                |  null blockStmts
                -> basicBlocks' blocks (Block [s] l ZeroNeighs) ss
                |  otherwise
                -> let block' = block { blockStmts  = Jump l : blockStmts
                                      , blockNeighs = OneNeigh l
                                      }
                   in  basicBlocks' (block' : blocks) (Block [s] l ZeroNeighs) ss
            Jump l -> let block' = block { blockStmts  = s : blockStmts
                                         , blockNeighs = OneNeigh l
                                         }
                      in basicBlocks' (block' : blocks) (Block [] l ZeroNeighs) ss
            CJump _ _ _ tLab fLab ->
                let block' = block { blockStmts  = s : blockStmts
                                   , blockNeighs = TwoNeighs tLab fLab
                                   }
                in basicBlocks' (block' : blocks) (Block [] tLab ZeroNeighs) ss
            Ret -> let block' = block { blockStmts  = s : blockStmts
                                      , blockNeighs = ZeroNeighs
                                      }
                   in basicBlocks' (block' : blocks) (Block [] funName ZeroNeighs) ss
            _ -> basicBlocks' blocks block { blockStmts = s : blockStmts } ss
        basicBlocks' blocks block@Block{..} _
            | null blockStmts = reverse blocks
            | otherwise       = reverse $ block : blocks

data DfsResult = DfsResult
    { dfsLen       :: !Int
    , dfsTrace     :: ![Block Stmt]
    , dfsTraces    :: ![[Block Stmt]]
    , dfsBlocksMap :: !(HashMap Temp.Label (Block Stmt))
    }

getTraces :: [Block Stmt] -> [[Block Stmt]]
getTraces blocks = case blocks of
    (b : _) -> let res = dfsBlock initBlocksMap [] b
               in dfsTrace res : dfsTraces res
    _       -> []
  where insertBlock blocksMap block@Block{..} = HashMap.insert blockLabel block blocksMap
        initBlocksMap = foldl insertBlock HashMap.empty blocks

        dfsBlock :: HashMap Temp.Label (Block Stmt)
                 -> [[Block Stmt]]
                 -> Block Stmt
                 -> DfsResult
        dfsBlock blocksMap traces b@Block{..} = case blockNeighs of
            ZeroNeighs -> zeroNeighs
            OneNeigh l
              | Just n <- HashMap.lookup l blocksMap' -> oneNeigh blocksMap' traces b n
              | otherwise -> zeroNeighs
            TwoNeighs l1 l2 -> twoNeighs blocksMap' traces b l1 l2
          where zeroNeighs = DfsResult 0 [b] traces blocksMap'
                blocksMap' = HashMap.delete blockLabel blocksMap

        oneNeigh :: HashMap Temp.Label (Block Stmt)
                 -> [[Block Stmt]]
                 -> Block Stmt
                 -> Block Stmt
                 -> DfsResult
        oneNeigh blocksMap traces prevBlock curBlock = result
          where result = DfsResult (dfsLen + 1) trace' dfsTraces dfsBlocksMap
                DfsResult{..} = dfsBlock blocksMap traces curBlock
                trace' = prevBlock : dfsTrace

        twoNeighs :: HashMap Temp.Label (Block Stmt)
                  -> [[Block Stmt]]
                  -> Block Stmt
                  -> Temp.Label
                  -> Temp.Label
                  -> DfsResult
        twoNeighs blocksMap traces prevBlock l1 l2 = case (mb1, mb2) of
            (Just b1, Just b2) ->
                let res1 = dfsBlock blocksMap traces b1
                    res2 = dfsBlock blocksMap traces b2
                in if dfsLen res1 >= dfsLen res2
                   then combine res1 b2
                   else combine res2 b1
            (Just b1, Nothing) -> oneNeigh blocksMap traces prevBlock b1
            (Nothing, Just b2) -> oneNeigh blocksMap traces prevBlock b2
            (Nothing, Nothing) -> DfsResult 0 [prevBlock] traces blocksMap
          where mb1 = HashMap.lookup l1 blocksMap
                mb2 = HashMap.lookup l2 blocksMap

                combine maxRes n@Block{..}
                  | HashMap.member blockLabel (dfsBlocksMap maxRes)
                  = let minRes = dfsBlock (dfsBlocksMap maxRes) (dfsTraces maxRes) n
                        traces' = if null (dfsTrace minRes)
                                  then dfsTraces minRes
                                  else dfsTrace minRes : dfsTraces minRes
                    in DfsResult { dfsLen       = dfsLen maxRes + 1
                                 , dfsTrace     = prevBlock : dfsTrace maxRes
                                 , dfsTraces    = traces'
                                 , dfsBlocksMap = dfsBlocksMap minRes
                                 }
                  | otherwise
                  = maxRes { dfsLen   = dfsLen maxRes + 1
                           , dfsTrace = prevBlock : dfsTrace maxRes
                           }

createControlFlowGraph :: [[Block Stmt]] -> ControlFlowGraph Stmt
createControlFlowGraph traces = ControlFlowGraph {..}
  where cfgNodes = fst $ foldl numberTrace (HashMap.empty, 0) traces
        cfgGraph = fst $ foldl insertTrace (Graph.empty, 0) traces

        numberTrace = foldl numberBlock
        numberBlock (ns, n) b = (HashMap.insert (blockLabel b) n ns, n + 1)

        insertTrace = foldl insertBlock
        insertBlock (gr, n) b@Block{..} = (Graph.insert ([], n, b, neighs) gr, n + 1)
          where neighs = case blockNeighs of
                    ZeroNeighs -> []
                    OneNeigh n1 -> [((), fromJust $ HashMap.lookup n1 cfgNodes)]
                    TwoNeighs n1 n2 -> [ ((), fromJust $ HashMap.lookup n1 cfgNodes)
                                       , ((), fromJust $ HashMap.lookup n2 cfgNodes)
                                       ]

reduceBasicBlocks :: MonadTemp m => [[Block Stmt]] -> m [[Block Stmt]]
reduceBasicBlocks traces = do
    (traces', usedLabels) <- reduceJumpsInTraces traces HashSet.empty
    pure $ map (reduceBasicBlocks' usedLabels . map reverseBlock) traces'
  where reduceBasicBlocks' :: HashSet Temp.Label -> [Block Stmt] -> [Block Stmt]
        reduceBasicBlocks' usedLabels (b1 : b2 : bs)
          | HashSet.member (blockLabel b2) usedLabels
          = b1 : reduceBasicBlocks' usedLabels (b2 : bs)
          | otherwise
          = case blockStmts b2 of
                (_ : ss) ->
                    let b1' = b1 { blockStmts  = blockStmts b1 ++ ss
                                 , blockNeighs = blockNeighs b2
                                 }
                    in  reduceBasicBlocks' usedLabels (b1' : bs)
                _ -> let b1' = b1 { blockNeighs = blockNeighs b2 }
                     in  b1' : reduceBasicBlocks' usedLabels bs
        reduceBasicBlocks' _ bs = bs

        reduceJumpsInTraces :: MonadTemp m
                            => [[Block Stmt]]
                            -> HashSet Temp.Label
                            -> m ([[Block Stmt]], HashSet Temp.Label)
        reduceJumpsInTraces (t : ts) ls = do
            (t', ls') <- reduceJumps t ls
            (ts', ls'') <- reduceJumpsInTraces ts ls'
            pure (t' : ts', ls'')
        reduceJumpsInTraces ts ls = pure (ts, ls)

        reverseBlock :: Block Stmt -> Block Stmt
        reverseBlock b = b { blockStmts = reverse $ blockStmts b }

reduceJumps :: MonadTemp m
            => [Block Stmt]
            -> HashSet Temp.Label
            -> m ([Block Stmt], HashSet Temp.Label)
reduceJumps (b1 : bs@(b2 : _)) usedLabels = case blockStmts b1 of
    (Jump{} : stmts) -> do
        (restBlocks, usedLabels') <- reduceJumps bs usedLabels
        let b1' = b1 { blockStmts = stmts }
        pure (b1' : restBlocks, usedLabels')
    (CJump op e1 e2 tLab fLab : stmts) -> do
        let usedLabels' = HashSet.insert tLab $ HashSet.insert fLab usedLabels
        (restBlocks, usedLabels'') <- reduceJumps bs usedLabels'
        let cjump = if blockLabel b2 == tLab
                    then CJump (notRelop op) e1 e2 fLab tLab
                    else CJump op e1 e2 tLab fLab
        let b1' = b1 { blockStmts = cjump : stmts }
        pure (b1' : restBlocks, usedLabels'')
    _ -> do
        (restBlocks, usedLabels') <- reduceJumps bs usedLabels
        pure (b1 : restBlocks, usedLabels')
reduceJumps (b : bs) usedLabels = case blockStmts b of
    Jump l : _ -> pure (b : bs, HashSet.insert l usedLabels)
    CJump op e1 e2 tLab fLab : _ -> do
        fLab' <- newLabel
        let cjump = CJump op e1 e2 tLab fLab'
        let b' = b { blockStmts = cjump : blockStmts b }
        let newBlock = Block [Jump fLab, Label fLab'] fLab' (OneNeigh fLab)
        let usedLabels' = HashSet.insert tLab
                        $ HashSet.insert fLab
                        $ HashSet.insert fLab' usedLabels
        pure (b' : newBlock : bs, usedLabels')
    _ -> pure (b : bs, usedLabels)
reduceJumps bs usedLabels = pure (bs, usedLabels)

removeLabelAliases :: [Stmt] -> [Stmt]
removeLabelAliases initStmts = uncurry replaceAliases
                             $ collectAliases initStmts HashMap.empty
  where replaceAliases :: HashMap Temp.Label Temp.Label -> [Stmt] -> [Stmt]
        replaceAliases aliases = map $ \case
            Jump l -> Jump (transLookup l aliases)
            CJump op e1 e2 tLab fLab ->
                let tLab' = transLookup tLab aliases
                    fLab' = transLookup fLab aliases
                in CJump op e1 e2 tLab' fLab'
            s -> s

        transLookup :: Temp.Label -> HashMap Temp.Label Temp.Label -> Temp.Label
        transLookup l aliases = case HashMap.lookup l aliases of
            Nothing    -> l
            Just alias -> transLookup alias aliases

        collectAliases :: [Stmt]
                       -> HashMap Temp.Label Temp.Label
                       -> (HashMap Temp.Label Temp.Label, [Stmt])
        collectAliases (Label l1 : Label l2 : ss) aliases =
            collectAliases (Label l1 : ss) (HashMap.insert l2 l1 aliases)
        collectAliases (j : Label l1 : Jump l2 : ss) aliases
          | isJump j  = (j:) <$> collectAliases ss (HashMap.insert l1 l2 aliases)
          | otherwise = (\x -> j : Jump l2 : x)
                     <$> collectAliases ss (HashMap.insert l1 l2 aliases)
        collectAliases (s:ss) aliases = (s:) <$> collectAliases ss aliases
        collectAliases ss aliases = (aliases, ss)

        isJump :: Stmt -> Bool
        isJump = \case
            Jump{}  -> True
            CJump{} -> True
            Ret     -> True
            _       -> False

reorderStmt :: MonadTemp m => Stmt -> m Stmt
reorderStmt = \case
    Move (Mem dst) src -> do
        (preDst, rDst) <- reorderExpr dst
        (preSrc, rSrc) <- reorderExpr src
        pure $ seqPrepMaybes [preDst, preSrc] $ Move (Mem rDst) rSrc
    Move dst src -> do
        (preSrc, rSrc) <- reorderExpr src
        pure $ seqPrepMaybes [preSrc] $ Move dst rSrc
    Expr (Call funName args) -> fst <$> reorderCall True funName args
    Expr e -> do
        (preE, rE) <- reorderExpr e
        pure $ seqPrepMaybes [preE] $ Expr rE
    Jump l -> pure $ Jump l
    CJump op (ESeq s e1) e2 tLab fLab -> do
        (preE1, rE1) <- reorderExpr e1
        (preE2, rE2) <- reorderExpr e2
        rS <- reorderStmt s
        pure $ Seq $ rS :| catMaybes [preE1, preE2, Just $ CJump op rE1 rE2 tLab fLab]
    CJump op e1 (ESeq s e2) tLab fLab -> do
        (preE1, rE1) <- reorderExpr e1
        (preE2, rE2) <- reorderExpr e2
        rS <- reorderStmt s
        if isCommute s e1
           then do
            let seqRest = catMaybes [preE1, preE2, Just $ CJump op rE1 rE2 tLab fLab]
            pure $ Seq $ rS :| seqRest
           else do
            t <- newTemp
            let resStmt = CJump op (Temp t) rE2 tLab fLab
            let seqRest = rS : maybe [resStmt] (:[resStmt]) preE2
            let moveE1 = Move (Temp t) rE1
            pure $ Seq $ case preE1 of
                Nothing   -> moveE1 :| seqRest
                Just stmt -> stmt :| (moveE1 : seqRest)
    CJump op e1 e2 tLab fLab -> do
        (preE1, rE1) <- reorderExpr e1
        (preE2, rE2) <- reorderExpr e2
        pure $ seqPrepMaybes [preE1, preE2] $ CJump op rE1 rE2 tLab fLab
    Seq xs -> Seq <$> mapM reorderStmt xs
    Label l -> pure $ Label l
    Ret -> pure Ret
  where seqPrepMaybes :: [Maybe Stmt] -> Stmt -> Stmt
        seqPrepMaybes xs y = case catMaybes xs of
            []  -> y
            xs' -> Seq $ NonEmpty.prependList xs' (y :| [])

reorderExpr :: MonadTemp m => Expr -> m (Maybe Stmt, Expr)
reorderExpr = \case
    c@Const{} -> pure (Nothing, c)
    l@Name{} -> pure (Nothing, l)
    t@Temp{} -> pure (Nothing, t)
    Binop op (ESeq s e1) e2 -> do
        (preE1, rE1) <- reorderExpr e1
        (preE2, rE2) <- reorderExpr e2
        rS <- reorderStmt s
        pure (Just $ Seq $ rS :| catMaybes [preE1, preE2], Binop op rE1 rE2)
    Binop op e1 (ESeq s e2) -> do
        (preE1, rE1) <- reorderExpr e1
        (preE2, rE2) <- reorderExpr e2
        rS <- reorderStmt s
        if isCommute s e1
           then pure (Just $ seqApMaybes rS [preE1, preE2], Binop op rE1 rE2)
           else do
            t <- newTemp
            let preSeqRest = rS : maybeToList preE2
            let moveE1 = Move (Temp t) rE1
            let preSeq = Seq $ case preE1 of
                    Nothing   -> moveE1 :| preSeqRest
                    Just stmt -> stmt :| (moveE1 : preSeqRest)
            pure (Just preSeq, Binop op (Temp t) rE2)
    Binop op e1 e2 -> do
        (preE1, rE1) <- reorderExpr e1
        (preE2, rE2) <- reorderExpr e2
        let resExpr = Binop op rE1 rE2
        case NonEmpty.nonEmpty $ catMaybes [preE1, preE2] of
            Just xs -> pure (Just $ Seq xs, resExpr)
            Nothing -> pure (Nothing, resExpr)
    Mem (ESeq s e) -> do
        (preE, rE) <- reorderExpr e
        rS <- reorderStmt s
        pure $ (Just $ seqApMaybes rS [preE], Mem rE)
    Mem e -> fmap Mem <$> reorderExpr e
    Call funName args -> do
        (preCall, rCall) <- reorderCall False funName args
        pure (Just preCall, rCall)
    ESeq s1 (ESeq s2 e) -> do
        rS1 <- reorderStmt s1
        rS2 <- reorderStmt s2
        (preE, rE) <- reorderExpr e
        pure $ (Just $ Seq $ rS1 :| rS2 : maybeToList preE, rE)
    ESeq s e -> do
        rS <- reorderStmt s
        (preE, rE) <- reorderExpr e
        pure (Just $ seqApMaybes rS [preE], rE)
  where seqApMaybes :: Stmt -> [Maybe Stmt] -> Stmt
        seqApMaybes y xs = case catMaybes xs of
            []  -> y
            xs' -> Seq $ y :| xs'

reorderCall :: MonadTemp m => Bool -> Temp.Label -> [Expr] -> m (Stmt, Expr)
reorderCall isStmt funName = reorderCall' [] []
  where reorderCall' preArgs rArgs (e:es) = do
            (preE, rE) <- reorderExpr e
            reorderCall' (maybe preArgs (:preArgs) preE) (rE : rArgs) es
        reorderCall' preArgs rArgs _ = do
            (lastStmt, resExpr) <- if isStmt
                                   then pure $ (Expr callExpr, callExpr)
                                   else do
                                    t <- newTemp
                                    pure $ (Move (Temp t) callExpr, Temp t)
            case NonEmpty.nonEmpty preArgs of
                Nothing        -> pure (lastStmt, resExpr)
                Just nePreArgs -> pure ( Seq $ NonEmpty.reverse $ lastStmt <| nePreArgs
                                       , resExpr
                                       )
          where callExpr = Call funName (reverse rArgs)

isCommute :: Stmt -> Expr -> Bool
isCommute (Expr (Const _)) _ = True
isCommute _ (Name _)         = True
isCommute _ (Const _)        = True
isCommute _ _                = False
