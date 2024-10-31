module Common
    ( EqExpr(..)
    , lval
    , var
    , dot
    , index
    , nil
    , int
    , str
    , neg
    , add
    , sub
    , mul
    , div
    , lt
    , le
    , gt
    , ge
    , ne
    , eq
    , and
    , or
    , record
    , field
    , array
    , assign
    , if'
    , while
    , for
    , esc
    , rem
    , seq
    , call
    , break
    , let'
    , typeDecs
    , typeAlias
    , recordType
    , arrayType
    , varDec
    , funDecs
    , funDec
    , decField
    , recField
    , genericParser
    , genericSemant
    , genericCompileToIR
    , genericCanonicalizeIR
    , genericCodegen
    , runInterpreter
    ) where

import           Control.Monad          ((>=>))
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import           Data.Typeable          (Typeable)
import           Prelude                hiding (and, break, div, or, rem, seq, span)
import           Test.Tasty.HUnit

import           Tiger.Codegen          (Instruction, TempReg)
import           Tiger.EscapeAnalysis   (escapeAnalyze)
import           Tiger.Expr
import           Tiger.Frame            (Frame, Instr, Reg, codegen)
import           Tiger.IR               (ControlFlowGraph (..), IRData (..), Stmt,
                                         canonicalize)
import           Tiger.Parser           (parse)
import           Tiger.RegMachine       (FrameEmulator, FrameRegister (..), Interpretable,
                                         InterpreterResult (..), ReturnRegister (..),
                                         newEmulator)
import           Tiger.Semant           (PosedSemantException (..), posedExceptionToText,
                                         semantAnalyze)
import           Tiger.Temp             (InitLabel (..), InitTemp (..), TempM, runTempM)
import           Tiger.TextUtils        (TextBuildable (..))

import qualified Data.List.NonEmpty     as NonEmpty
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LazyText
import qualified Data.Text.Lazy.Builder as Builder

import qualified Tiger.RegMachine       as RegMachine

newtype EqExpr = EqExpr { unEqExpr :: Expr }

instance Eq EqExpr where
    EqExpr eqE1 == EqExpr eqE2 = case (eqE1, eqE2) of
        (LVal l1, LVal l2)                           -> EqLVal l1 == EqLVal l2
        (Nil _, Nil _)                               -> True
        (IntLit v1 _, IntLit v2 _)                   -> v1 == v2
        (StrLit v1 _, StrLit v2 _)                   -> v1 == v2
        (Neg e1 _, Neg e2 _)                         -> EqExpr e1 == EqExpr e2
        (Binop e11 op1 e12 _, Binop e21 op2 e22 _)   -> EqOp op1 == EqOp op2
                                                     && EqExpr e11 == EqExpr e21
                                                     && EqExpr e12 == EqExpr e22
        (Record t1 fs1 _, Record t2 fs2 _)           -> t1 == t2
                                                     && map EqField fs1 == map EqField fs2
        (Array t1 e11 e12 _, Array t2 e21 e22 _)     -> t1 == t2
                                                     && EqExpr e11 == EqExpr e21
                                                     && EqExpr e12 == EqExpr e22
        (Assign l1 e1 _, Assign l2 e2 _)             -> EqLVal l1 == EqLVal l2
                                                     && EqExpr e1 == EqExpr e2
        (If c1 t1 e1 _, If c2 t2 e2 _)               -> EqExpr c1 == EqExpr c2
                                                     && EqExpr t1 == EqExpr t2
                                                     && (EqExpr <$> e1) == (EqExpr <$> e2)
        (While e11 e12 _, While e21 e22 _)           -> EqExpr e11 == EqExpr e21
                                                     && EqExpr e12 == EqExpr e22
        (For v1 e1 f1 t1 b1 _, For v2 e2 f2 t2 b2 _) -> v1 == v2
                                                     && EqEscaping e1 == EqEscaping e2
                                                     && EqExpr f1 == EqExpr f2
                                                     && EqExpr t1 == EqExpr t2
                                                     && EqExpr b1 == EqExpr b2
        (Seq es1 _, Seq es2 _)                       -> (EqExpr <$> es1)
                                                     == (EqExpr <$> es2)
        (Call f1 es1 _, Call f2 es2 _)               -> f1 == f2
                                                     && (EqExpr <$> es1)
                                                     == (EqExpr <$> es2)
        (Break _, Break _)                           -> True
        (Let ds1 e1 _, Let ds2 e2 _)                 -> (EqDec <$> ds1) == (EqDec <$> ds2)
                                                     && EqExpr e1 == EqExpr e2
        _                                            -> False

instance Show EqExpr where
    show = LazyText.unpack . Builder.toLazyText . toTextBuilder . unEqExpr

newtype EqLVal = EqLVal LVal

instance Eq EqLVal where
    EqLVal eqL1 == EqLVal eqL2 = case (eqL1, eqL2) of
        (Var v1 _, Var v2 _)           -> v1 == v2
        (Dot l1 f1 _, Dot l2 f2 _)     -> f1 == f2 && EqLVal l1 == EqLVal l2
        (Index l1 e1 _, Index l2 e2 _) -> EqLVal l1 == EqLVal l2
                                       && EqExpr e1 == EqExpr e2
        _                              -> False

newtype EqOp = EqOp Binop

instance Eq EqOp where
    EqOp op1 == EqOp op2 = case (op1, op2) of
        (Add, Add) -> True
        (Sub, Sub) -> True
        (Mul, Mul) -> True
        (Div, Div) -> True
        (Lt, Lt)   -> True
        (Le, Le)   -> True
        (Gt, Gt)   -> True
        (Ge, Ge)   -> True
        (Ne, Ne)   -> True
        (Eq, Eq)   -> True
        (And, And) -> True
        (Or, Or)   -> True
        _          -> False

newtype EqField = EqField Field

instance Eq EqField where
    EqField f1 == EqField f2 =  fieldName f1 == fieldName f2
                             && EqExpr (fieldValue f1) == EqExpr (fieldValue f2)

newtype EqEscaping = EqEscaping Escaping

instance Eq EqEscaping where
    EqEscaping e1 == EqEscaping e2 = case (e1, e2) of
        (Escaping, Escaping)   -> True
        (Remaining, Remaining) -> True
        _                      -> False

newtype EqDec = EqDec Dec

instance Eq EqDec where
    EqDec d1 == EqDec d2 = case (d1, d2) of
        (TypeDecs ds1, TypeDecs ds2)                     -> (EqTypeDec <$> ds1)
                                                         == (EqTypeDec <$> ds2)
        (VarDec v1 t1 e1 esc1 _, VarDec v2 t2 e2 esc2 _) -> v1 == v2
                                                         && t1 == t2
                                                         && EqExpr e1 == EqExpr e2
                                                         && EqEscaping esc1
                                                         == EqEscaping esc2
        (FunDecs ds1, FunDecs ds2)                       -> (EqFunDec <$> ds1)
                                                         == (EqFunDec <$> ds2)
        _                                                -> False

newtype EqTypeDec = EqTypeDec TypeDec

instance Eq EqTypeDec where
    EqTypeDec (TypeDec n1 b1 _) == EqTypeDec (TypeDec n2 b2 _)
      = n1 == n2 && EqTypeDecBody b1 == EqTypeDecBody b2

newtype EqTypeDecBody = EqTypeDecBody TypeDecBody

instance Eq EqTypeDecBody where
    EqTypeDecBody b1 == EqTypeDecBody b2 = case (b1, b2) of
        (TypeAlias t1 _, TypeAlias t2 _)     -> t1 == t2
        (RecordType ds1 _, RecordType ds2 _) -> (EqRecordField <$> ds1)
                                             == (EqRecordField <$> ds2)
        (ArrayType t1 _, ArrayType t2 _)     -> t1 == t2
        _                                    -> False

newtype EqDecField = EqDecField DecField

instance Eq EqDecField where
    EqDecField (DecField n1 e1 t1 _) == EqDecField (DecField n2 e2 t2 _) =
        n1 == n2 && EqEscaping e1 == EqEscaping e2 && t1 == t2

newtype EqRecordField = EqRecordField RecordField

instance Eq EqRecordField where
    EqRecordField (RecordField n1 t1 _) == EqRecordField (RecordField n2 t2 _) =
        n1 == n2 && t1 == t2

newtype EqFunDec = EqFunDec FunDec

instance Eq EqFunDec where
    EqFunDec (FunDec n1 as1 t1 b1 _) == EqFunDec (FunDec n2 as2 t2 b2 _)
      =  n1 == n2 && (EqDecField <$> as1) == (EqDecField <$> as2) && t1 == t2
      && EqExpr b1 == EqExpr b2

lval :: LVal -> EqExpr
lval = EqExpr . LVal

var :: Text -> LVal
var n = Var n span

dot :: LVal -> Text -> LVal
dot l f = Dot l f span

index :: LVal -> EqExpr -> LVal
index l (EqExpr e) = Index l e span

span :: Span
span = Span (Position 0 0) (Position 0 0)

nil :: EqExpr
nil = EqExpr $ Nil span

int :: Int -> EqExpr
int v = EqExpr $ IntLit v span

str :: Text -> EqExpr
str v = EqExpr $ StrLit v span

neg :: EqExpr -> EqExpr
neg (EqExpr e) = EqExpr (Neg e span)

add, sub, mul, div, lt, le, gt, ge, ne, eq, and, or :: EqExpr -> EqExpr -> EqExpr
add = binop Add
sub = binop Sub
mul = binop Mul
div = binop Div
lt  = binop Lt
le  = binop Le
gt  = binop Gt
ge  = binop Ge
ne  = binop Ne
eq  = binop Eq
and = binop And
or  = binop Or

binop :: Binop -> EqExpr -> EqExpr -> EqExpr
binop op (EqExpr e1) (EqExpr e2) = EqExpr (Binop e1 op e2 span)

record :: Text -> [Field] -> EqExpr
record r fs = EqExpr $ Record r fs span

field :: Text -> EqExpr -> Field
field n (EqExpr e) = Field n e span

array :: Text -> EqExpr -> EqExpr -> EqExpr
array t (EqExpr e1) (EqExpr e2) = EqExpr $ Array t e1 e2 span

assign :: LVal -> EqExpr -> EqExpr
assign l (EqExpr e) = EqExpr $ Assign l e span

if' :: [EqExpr] -> EqExpr
if' [EqExpr c, EqExpr t]           = EqExpr $ If c t Nothing span
if' [EqExpr c, EqExpr t, EqExpr e] = EqExpr $ If c t (Just e) span
if' _                              = error "wrong if expression"

while :: EqExpr -> EqExpr -> EqExpr
while (EqExpr e1) (EqExpr e2) = EqExpr $ While e1 e2 span

for :: Text -> Escaping -> EqExpr -> EqExpr -> EqExpr -> EqExpr
for v e (EqExpr e1) (EqExpr e2) (EqExpr e3) = EqExpr $ For v e e1 e2 e3 span

esc :: Escaping
esc = Escaping

rem :: Escaping
rem = Remaining

seq :: [EqExpr] -> EqExpr
seq es = EqExpr $ Seq (map unEqExpr es) span

call :: Text -> [EqExpr] -> EqExpr
call f as = EqExpr $ Call f (map unEqExpr as) span

break :: EqExpr
break = EqExpr $ Break span

let' :: [Dec] -> EqExpr -> EqExpr
let' ds (EqExpr e) = EqExpr $ Let (NonEmpty.fromList ds) e span

typeDecs :: [TypeDec] -> Dec
typeDecs = TypeDecs . NonEmpty.fromList

typeAlias :: Text -> Text -> TypeDec
typeAlias n a = TypeDec n (TypeAlias a span) span

recordType :: Text -> [RecordField] -> TypeDec
recordType n fs = TypeDec n (RecordType fs span) span

arrayType :: Text -> Text -> TypeDec
arrayType n a = TypeDec n (ArrayType a span) span

varDec :: Text -> Maybe Text -> Escaping -> EqExpr -> Dec
varDec v t es (EqExpr e) = VarDec v t e es span

funDecs :: [FunDec] -> Dec
funDecs = FunDecs . NonEmpty.fromList

funDec :: Text -> [DecField] -> Maybe Text -> EqExpr -> FunDec
funDec n as t (EqExpr e) = FunDec n as t e span

decField :: Text -> Escaping -> Text -> DecField
decField n e t = DecField n e t span

recField :: Text -> Text -> RecordField
recField n t = RecordField n t span

genericParser :: FilePath -> Text -> IO Expr
genericParser path src = case parse path src of
    Left err   -> assertFailure $  "unexpected parsing error `"
                                ++ path
                                ++ ":`\n"
                                ++ Text.unpack err
    Right expr -> pure expr

genericSemant :: forall f a. Frame f
           => FilePath
           -> Text
           -> Proxy f
           -> (Either PosedSemantException (Expr, IRData Stmt f) -> TempM a)
           -> IO a
genericSemant path src _ cont = do
    expr <- genericParser path src
    escapeResult <- escapeAnalyze expr
    runTempM (InitTemp 0) (InitLabel 0) $
        semantAnalyze @f path escapeResult >>= cont . fmap (expr,)

genericCompileToIR :: Frame f
                   => Text
                   -> Proxy f
                   -> (IRData Stmt f -> TempM a)
                   -> IO a
genericCompileToIR src prxy cont = genericSemant "test.tig" src prxy $ \case
    Left err      -> liftIO $ assertFailure $  "unexpected type error `"
                                            ++ Text.unpack (posedExceptionToText err)
    Right (_, ir) -> cont ir

genericCanonicalizeIR :: Frame f
                      => Text
                      -> Proxy f
                      -> (IRData (ControlFlowGraph Stmt) f -> TempM a)
                      -> IO a
genericCanonicalizeIR src prxy cont = genericCompileToIR src prxy (canonicalize >=> cont)

genericCodegen :: ( Frame f
                  , Instruction (Instr f) (TempReg (Reg f))
                  , Instruction (Instr f) (Reg f)
                  )
               => Text
               -> Proxy f
               -> (IRData (ControlFlowGraph (Instr f (TempReg (Reg f)))) f -> TempM a)
               -> IO a
genericCodegen src prxy cont = genericCanonicalizeIR src prxy (codegen >=> cont)

runInterpreter :: forall f e r s b. ( Typeable r
                                    , Enum r
                                    , TextBuildable r
                                    , Typeable s
                                    , TextBuildable s
                                    , FrameEmulator f e r s
                                    , Interpretable f e r s b
                                    )
               => ReturnRegister r
               -> FrameRegister r
               -> Proxy e
               -> IRData b f
               -> Text
               -> IO (InterpreterResult r s)
runInterpreter rv fp _ ir input = do
    emu <- newEmulator @e (Proxy @f)
    res <- RegMachine.runInterpreter rv fp emu ir input
    case resError res of
        Just err -> assertFailure $  "unexpected IR interpreter error \n"
                                  ++ "input: "
                                  ++ Text.unpack input
                                  ++ "\noutput: "
                                  ++ LazyText.unpack (resOutput res)
                                  ++ "\nError: "
                                  ++ show err
        Nothing  -> pure res
