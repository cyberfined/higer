{-# LANGUAGE FunctionalDependencies #-}

module Tiger.Codegen
    ( TempReg(..)
    , WithOperands(..)
    , Instruction(..)
    , InstructionBuilder(..)
    , Sources(..)
    , Destinations(..)
    ) where

import           Data.Proxy      (Proxy)

import           Tiger.Temp      (Label, Temp)
import           Tiger.TextUtils (TextBuildable (..))

data TempReg r = Reg r
               | Temp !Temp
               deriving stock (Eq, Functor)

instance (Enum r, Bounded r) => Enum (TempReg r) where
    fromEnum = \case
        Reg r  -> fromEnum r
        Temp x -> fromEnum (maxBound @r) + 1 + fromEnum x

    toEnum x
      | x >= fromEnum (minBound @r) && x <= fromEnum (maxBound @r) = Reg $ toEnum x
      | otherwise = Temp $ toEnum $ x - fromEnum (maxBound @r) - 1

instance TextBuildable r => TextBuildable (TempReg r) where
    toTextBuilder = \case
        Reg r  -> toTextBuilder r
        Temp t -> toTextBuilder t

newtype Sources r = Sources { getSource :: [r] }

newtype Destinations r = Destinations { getDestination :: [r] }

class (Eq r, Enum r) => WithOperands a r where
    getOperands :: a r -> (Destinations r, Sources r)

class (Eq r, Enum r, WithOperands a r) => Instruction a r where
    isMove          :: a r -> Bool
    matchLabel      :: a r -> Maybe Label
    jumpsTo         :: a r -> [Label]
    substOperands   :: [(r, b)] -> a r -> a b

class Instruction a r => InstructionBuilder b a r | b -> a, a -> b, r -> b where
    loadRegister  :: Proxy b -> Int -> r -> a r
    storeRegister :: Proxy b -> Int -> r -> a r
    moveRegister  :: Proxy b -> r -> r -> a r
