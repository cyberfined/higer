module Tiger.Semant.Type
    ( Type(..)
    , TypeRef(..)
    , isTypesMatch
    , typeToText
    ) where

import           Data.IORef   (IORef)
import           Data.Text    (Text)

import           Tiger.Unique (Unique)

import qualified Data.Text    as Text

data Type
    = TInt
    | TString
    | TRecord !Text ![(Text, Type)] !Unique
    | TArray !Type !Unique
    | TNil
    | TUnit
    | TName !TypeRef
    deriving Eq

newtype TypeRef = TypeRef (IORef (Maybe Type)) deriving Eq

isTypesMatch :: Type -> Type -> Bool
isTypesMatch TInt TInt                         = True
isTypesMatch TString TString                   = True
isTypesMatch TRecord{} TNil                    = True
isTypesMatch TNil TRecord{}                    = True
isTypesMatch (TRecord _ _ u1) (TRecord _ _ u2) = u1 == u2
isTypesMatch (TArray _ u1) (TArray _ u2)       = u1 == u2
isTypesMatch TNil TNil                         = True
isTypesMatch TUnit TUnit                       = True
isTypesMatch (TName r1) (TName r2)             = r1 == r2
isTypesMatch _ _                               = False

instance Show Type where
    show = Text.unpack . typeToText

typeToText :: Type -> Text
typeToText = \case
    TInt            -> "int"
    TString         -> "string"
    TRecord rec _ _ -> rec
    TArray typ _    -> typeToText typ <> "[]"
    TNil            -> "nil"
    TUnit           -> "unit"
    TName{}         -> "name"
