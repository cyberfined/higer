module Tiger.Semant.LibFunctions
    ( LibFunction(..)
    , libFunctions
    ) where

import           Data.Text         (Text)

import           Tiger.Semant.Type (Type (..))

data LibFunction = LibFunction
    { libFunName :: !Text
    , libFunArgs :: ![Type]
    , libFunRet  :: Type
    }

libFunctions :: [LibFunction]
libFunctions = [ LibFunction "print" [TString] TUnit
               , LibFunction "flush" [] TUnit
               , LibFunction "getchar" [] TString
               , LibFunction "ord" [TString] TInt
               , LibFunction "chr" [TInt] TString
               , LibFunction "size" [TString] TInt
               , LibFunction "substring" [TString, TInt, TInt] TString
               , LibFunction "concat" [TString, TString] TString
               , LibFunction "not" [TInt] TInt
               , LibFunction "exit" [TInt] TUnit
               ]

