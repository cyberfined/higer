module LibFuncs (
    funTypes,
    sideEffectFuncs,
    pureFuncs
    ) where

import Absyn(Type(..))

funTypes :: [(String, Type)]
funTypes = [ ("print", TFunc [TString] TUnit)
           , ("flush", TFunc [] TUnit)
           , ("getchar", TFunc [] TString)
           , ("ord", TFunc [TString] TInt)
           , ("chr", TFunc [TInt] TString)
           , ("size", TFunc [TString] TInt)
           , ("substring", TFunc [TString, TInt, TInt] TString)
           , ("concat", TFunc [TString, TString] TString)
           , ("not", TFunc [TInt] TInt)
           , ("exit", TFunc [TInt] TUnit)
           ]

sideEffectFuncs :: [String]
sideEffectFuncs = [ "print"
                  , "flush"
                  , "getchar"
                  , "exit"
                  ]

pureFuncs :: [String]
pureFuncs = [ "ord"
            , "chr"
            , "size"
            , "substring"
            , "concat"
            , "not"
            ]
