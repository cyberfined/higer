module LibFuncs (
    funTypes,
    sideEffectFuncs,
    initArrayFunc,
    initRecordFunc,
    initStringFunc,
    fatalErrorFunc,
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
                  , initArrayFunc
                  , initRecordFunc
                  , fatalErrorFunc
                  ]

initArrayFunc :: String
initArrayFunc = "initArray"

initRecordFunc :: String
initRecordFunc = "initRecord"

initStringFunc :: String
initStringFunc = "initString"

fatalErrorFunc :: String
fatalErrorFunc = "fatal"
