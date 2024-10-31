{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Tiger.Amd64.Assem.Types
    ( Reg(..)
    , Offset(..)
    , Base(..)
    , Index(..)
    , Scale(..)
    , OperandType(..)
    , Const32
    , Const64
    , RegMem
    , Operand(..)
    , Instr(..)
    , Imul(..)
    , Condition(..)
    , InstrOperands
    , MovConstSize
    , CallingConvention(..)
    ) where

import           Data.Int                   (Int32, Int64, Int8)
import           Data.Kind                  (Constraint, Type)
import           Data.List                  (nub)
import           Data.Proxy                 (Proxy (..))
import           Data.Text.Lazy.Builder     (Builder)
import           GHC.Natural
import           GHC.TypeLits               (ErrorMessage (..), TypeError)

import           Tiger.Amd64.Frame          (Frame)
import           Tiger.Codegen              (Destinations (..), Instruction (..),
                                             Sources (..), TempReg (..),
                                             WithOperands (..))
import           Tiger.Temp                 (Label)
import           Tiger.TextUtils            (TextBuildable (..))

import qualified Data.Text.Lazy.Builder.Int as Builder

import qualified Tiger.Frame                as FrameClass

data Reg
    = Rax
    | Rbx
    | Rcx
    | Rdx
    | Rdi
    | Rsi
    | Rbp
    | Rsp
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    deriving stock (Eq, Enum, Bounded)

instance TextBuildable Reg where
    toTextBuilder r = case r of
        Rax -> "%rax"
        Rbx -> "%rbx"
        Rcx -> "%rcx"
        Rdx -> "%rdx"
        Rdi -> "%rdi"
        Rsi -> "%rsi"
        Rbp -> "%rbp"
        Rsp -> "%rsp"
        R8  -> "%r8"
        R9  -> "%r9"
        R10 -> "%r10"
        R11 -> "%r11"
        R12 -> "%r12"
        R13 -> "%r13"
        R14 -> "%r14"
        R15 -> "%r15"

newtype Offset = Offset Int32

newtype Base r = Base { getBase :: r }

newtype Index r = Index r

newtype Scale = Scale Int8

data OperandType
    = OpReg
    | OpMem
    | OpConst !Natural

type family ConstType (n :: Natural) :: Type where
    ConstType 8  = Int8
    ConstType 32 = Int32
    ConstType 64 = Int64

type family InstrOperands (t1 :: OperandType) (t2 :: OperandType) :: Constraint where
    InstrOperands 'OpReg _ = ()
    InstrOperands 'OpMem 'OpMem = TypeError (Text "mem to mem instructions are not allowed")
    InstrOperands 'OpMem _ = ()
    InstrOperands ('OpConst _) _ = TypeError (Text "constant can't be a destination")

type family ConstSize (t :: OperandType) (c :: OperandType) :: Constraint where
    ConstSize ('OpConst a) b = 'OpConst a ~ b
    ConstSize _ _ = ()

type family Const32 (t :: OperandType) :: Constraint where
    Const32 t = ConstSize t ('OpConst 32)

type family Const64 (t :: OperandType) :: Constraint where
    Const64 t = ConstSize t ('OpConst 64)

type family RegMem (t :: OperandType) :: Constraint where
    RegMem 'OpReg = ()
    RegMem 'OpMem = ()
    RegMem ('OpConst _) = TypeError (Text "only r/m64 operands are supported")

data Operand (r :: Type) (t :: OperandType) where
    AddrRegBase      :: !Offset -> !(Base r) -> Operand r 'OpMem
    AddrRegBaseIndex :: !Offset
                     -> !(Maybe (Base r))
                     -> !(Index r)
                     -> !Scale
                     -> Operand r 'OpMem
    Register         :: !r -> Operand r 'OpReg
    Const            :: Integral (ConstType n) => !(ConstType n) -> Operand r ('OpConst n)
    Name             :: !Label -> Operand r ('OpConst 64)

type family MovConstSize (t1 :: OperandType) (t2 :: OperandType) :: Constraint where
    MovConstSize 'OpReg t2 = Const64 t2
    MovConstSize _ t2      = Const32 t2

data Instr r where
    Add :: (InstrOperands t1 t2, Const32 t2)
        => !(Operand r t1)
        -> !(Operand r t2)
        -> Instr r
    Sub :: (InstrOperands t1 t2, Const32 t2)
        => !(Operand r t1)
        -> !(Operand r t2)
        -> Instr r
    Xor :: (InstrOperands t1 t2, Const32 t2)
        => !(Operand r t1)
        -> !(Operand r t2)
        -> Instr r
    Lea :: !(Operand r 'OpReg) -> !(Operand r 'OpMem) -> Instr r
    Sal :: RegMem t1 => !(Operand r t1) -> !(Operand r ('OpConst 8)) -> Instr r
    Sar :: RegMem t1 => !(Operand r t1) -> !(Operand r ('OpConst 8)) -> Instr r
    Imul :: !(Imul r) -> Instr r
    Idiv :: !(Operand r 'OpReg) -> Instr r
    Cqo :: Instr r
    Mov :: (InstrOperands t1 t2, MovConstSize t1 t2)
        => !(Operand r t1)
        -> !(Operand r t2)
        -> Instr r
    Test :: (InstrOperands t1 t2, Const32 t2)
         => !(Operand r t1)
         -> !(Operand r t2)
         -> Instr r
    Cmp :: (InstrOperands t1 t2, Const32 t2)
        => !(Operand r t1)
        -> !(Operand r t2)
        -> Instr r
    Jmp :: !Label -> Instr r
    Jcc :: !Condition -> !Label -> !Label -> Instr r
    Ret :: Instr r
    Label :: !Label -> Instr r
    Call :: !Label -> ![Operand r 'OpReg] -> Instr r
    Push :: Const32 t => !(Operand r t) -> Instr r
    Neg :: RegMem t => !(Operand r t) -> Instr r

data Imul (r :: Type) where
    Imul2 :: RegMem t
          => !(Operand r 'OpReg)
          -> !(Operand r t)
          -> Imul r
    Imul3 :: RegMem t
          => !(Operand r 'OpReg)
          -> !(Operand r t)
          -> !(Operand r ('OpConst 32))
          -> Imul r

data Condition
    = Ge
    | Gt
    | Le
    | Lt
    | Eq
    | Ne

instance TextBuildable r => TextBuildable (Instr r) where
    toTextBuilder instr = case instr of
        Add dst src -> "addq " <> opToAsm src <> ", " <> opToAsm dst
        Sub dst src -> "subq " <> opToAsm src <> ", " <> opToAsm dst
        Xor dst src -> "xorq " <> opToAsm src <> ", " <> opToAsm dst
        Lea dst src -> "leaq " <> opToAsm src <> ", " <> opToAsm dst
        Sal dst src -> "salq " <> opToAsm src <> ", " <> opToAsm dst
        Sar dst src -> "sarq " <> opToAsm src <> ", " <> opToAsm dst
        Imul imul -> case imul of
            Imul2 dst src -> "imulq " <> opToAsm src <> ", " <> opToAsm dst
            Imul3 dst s1 s2 -> "imulq "
                            <> opToAsm s2
                            <> ", " <> opToAsm s1
                            <> ", " <> opToAsm dst
        Idiv src -> "idivq " <> opToAsm src
        Cqo -> "cqo"
        Mov dst src -> "movq " <> opToAsm src <> ", " <> opToAsm dst
        Test dst src -> "testq " <> opToAsm src <> ", " <> opToAsm dst
        Cmp dst src -> "сmpq " <> opToAsm src <> ", " <> opToAsm dst
        Jmp l -> "jmp " <> toTextBuilder l
        Jcc cnd tLab _ -> conditionToAsm cnd <> " " <> toTextBuilder tLab
        Ret -> "ret"
        Label l -> toTextBuilder l <> ":"
        Call funName _ -> "call " <> toTextBuilder funName
        Push src -> "pushq " <> opToAsm src
        Neg dst -> "negq " <> opToAsm dst
      where opToAsm :: Operand r t -> Builder
            opToAsm = \case
                AddrRegBase (Offset off) (Base b) ->
                    let common = "(" <> toTextBuilder b <> ")"
                    in if off == 0 then common else hexadecimal off <> common
                AddrRegBaseIndex (Offset off) base (Index i) (Scale s) ->
                    let baseAsm = maybe "" (toTextBuilder . getBase) base
                        common =  "(" <> baseAsm
                               <> "," <> toTextBuilder i
                               <> "," <> hexadecimal s <> ")"
                    in if off == 0 then common else hexadecimal off <> common
                Register r -> toTextBuilder r
                Const c -> "$" <> hexadecimal c
                Name n  -> "$" <> toTextBuilder n

            conditionToAsm :: Condition -> Builder
            conditionToAsm = \case
                Ge -> "jge"
                Gt -> "jg"
                Le -> "jle"
                Lt -> "jl"
                Eq -> "je"
                Ne -> "jne"

            hexadecimal :: Integral a => a -> Builder
            hexadecimal x = prefix <> Builder.hexadecimal x'
              where (prefix, x') = if x < 0 then ("-0x", -x) else ("0x", x)

class FrameClass.Frame f => CallingConvention f where
    argsRegisters :: Proxy f -> [Reg]
    toAmd64Frame  :: f -> Frame

instance WithOperands Instr (TempReg Reg) where
    getOperands = getOperandsG Reg

instance WithOperands Instr Reg where
    getOperands = getOperandsG id

getOperandsG :: Eq r => (Reg -> r) -> Instr r -> (Destinations r, Sources r)
getOperandsG f = makeUniq . \case
    Add dst src -> flip getSrcOperandRegs src <$> getDstOperandRegs dst
    Sub dst src -> flip getSrcOperandRegs src <$> getDstOperandRegs dst
    Xor dst src -> flip getSrcOperandRegs src <$> getDstOperandRegs dst
    Lea dst src -> flip getSrcOperandRegs src <$> getDstOperandRegs dst
    Sal dst src -> flip getSrcOperandRegs src <$> getDstOperandRegs dst
    Sar dst src -> flip getSrcOperandRegs src <$> getDstOperandRegs dst
    Imul imul -> case imul of
        Imul2 dst src   -> flip getSrcOperandRegs src <$> getDstOperandRegs dst
        Imul3 dst src _ -> flip getSrcOperandRegs src <$> getDstOperandRegs dst
    Idiv src ->
        let Sources ss = getSrcOperandRegs (Sources []) src
        in (Destinations [f Rax, f Rdx], Sources $ f Rax : f Rdx : ss)
    Cqo -> (Destinations [f Rax, f Rdx], Sources [f Rax])
    Mov dst src -> flip getSrcOperandRegs src <$> getDstOperandRegs dst
    Test r1 r2 -> ( Destinations []
                  , getSrcOperandRegs (getSrcOperandRegs (Sources []) r1) r2
                  )
    Cmp r1 r2 -> ( Destinations []
                 , getSrcOperandRegs (getSrcOperandRegs (Sources []) r1) r2
                 )
    Jmp{} -> (Destinations [], Sources [])
    Jcc{} -> (Destinations [], Sources [])
    Ret{} -> (Destinations [], Sources [])
    Label{} -> (Destinations [], Sources [])
    Call _ args ->
        let srcs = map (\(Register r) -> r) args
        in (Destinations [f Rax, f Rdx], Sources (f Rsp : srcs))
    Push src -> (Destinations [f Rsp], getSrcOperandRegs (Sources []) src)
    Neg dst -> case dst of
        Register r -> (Destinations [r], Sources [r])
        _          -> getDstOperandRegs dst
  where getDstOperandRegs :: Operand r t -> (Destinations r, Sources r)
        getDstOperandRegs = \case
            AddrRegBase _ (Base b)                         -> ( Destinations []
                                                              , Sources [b]
                                                              )
            AddrRegBaseIndex _ Nothing (Index i) _         -> ( Destinations []
                                                              , Sources [i]
                                                              )
            AddrRegBaseIndex _ (Just (Base b)) (Index i) _ -> ( Destinations []
                                                              , Sources [b, i]
                                                              )
            Register r                                     -> ( Destinations [r]
                                                              , Sources []
                                                              )
            Const{}                                        -> (Destinations [], Sources [])
            Name{}                                         -> (Destinations [], Sources [])

        getSrcOperandRegs :: Sources r -> Operand r t -> Sources r
        getSrcOperandRegs (Sources rs) = \case
            AddrRegBase _ (Base b)                         -> Sources (b : rs)
            AddrRegBaseIndex _ Nothing (Index i) _         -> Sources (i : rs)
            AddrRegBaseIndex _ (Just (Base b)) (Index i) _ -> Sources (b : i : rs)
            Register r                                     -> Sources (r : rs)
            Const{}                                        -> Sources rs
            Name{}                                         -> Sources rs

        makeUniq :: Eq r => (Destinations r, Sources r) -> (Destinations r, Sources r)
        makeUniq (Destinations dd, Sources ss) = (Destinations $ nub dd, Sources $ nub ss)


instance (Eq r, Enum r, WithOperands Instr r) => Instruction Instr r where
    isMove = \case
        Mov{} -> True
        _     -> False
    matchLabel = \case
        Label l -> Just l
        _       -> Nothing
    jumpsTo = \case
        Jmp l -> [l]
        Jcc _ tLab fLab -> [tLab, fLab]
        _               -> []
    substOperands :: forall b. [(r, b)] -> Instr r -> Instr b
    substOperands ss = \case
        Add t1 t2 -> Add (substOperand t1) (substOperand t2)
        Sub t1 t2 -> Sub (substOperand t1) (substOperand t2)
        Xor t1 t2 -> Xor (substOperand t1) (substOperand t2)
        Lea t1 t2 -> Lea (substOperand t1) (substOperand t2)
        Sal t1 (Const c) -> Sal (substOperand t1) (Const c)
        Sar t1 (Const c) -> Sar (substOperand t1) (Const c)
        Imul imul -> Imul $ case imul of
            Imul2 t1 t2 -> Imul2 (substOperand t1) (substOperand t2)
            Imul3 t1 t2 (Const c) -> Imul3 (substOperand t1)
                                           (substOperand t2)
                                           (Const c)
        Idiv (Register r) -> Idiv (Register $ findSubst ss r)
        Cqo -> Cqo
        Mov t1 t2 -> Mov (substOperand t1) (substOperand t2)
        Test t1 t2 -> Test (substOperand t1) (substOperand t2)
        Cmp t1 t2 -> Cmp (substOperand t1) (substOperand t2)
        Jmp l -> Jmp l
        Jcc cnd tLab fLab -> Jcc cnd tLab fLab
        Ret -> Ret
        Label l -> Label l
        Call fn args -> Call fn $ map substOperand args
        Push t -> Push $ substOperand t
        Neg t -> Neg $ substOperand t
      where substOperand :: Operand r t -> Operand b t
            substOperand = \case
                AddrRegBase off (Base b) -> AddrRegBase off (Base $ findSubst ss b)
                AddrRegBaseIndex off base (Index i) scale ->
                    let b' = Base . findSubst ss . getBase <$> base
                        i' = Index $ findSubst ss i
                    in AddrRegBaseIndex off b' i' scale
                Register r -> Register (findSubst ss r)
                Const c -> Const c
                Name n  -> Name n

            findSubst :: Eq a => [(a, b)] -> a -> b
            findSubst xs x = case lookup x xs of
                Just b  -> b
                Nothing -> error "Failed to find substitution"
