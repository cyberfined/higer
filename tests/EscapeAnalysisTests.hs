{-# LANGUAGE OverloadedStrings #-}

module EscapeAnalysisTests (tests) where

import Test.HUnit
import Data.Text (Text, unpack, unlines)
import Prelude hiding (unlines)

import Common
import Tiger.EscapeAnalysis
import Tiger.Expr
import Tiger.Parser
import Tiger.TypeCheck

tests :: Test
tests = mkTestLabel "escape analysis tests"
    [ assertEscapeAnalysisML
        [ "let var v := 1"
        , "in v"
        , "end"
        ] $ mkLet
        [ mkTypedVar "v" TInt Remaining (mkIntLit 1) ] $ mkLVal (Var "v")
    , assertEscapeAnalysisML
        [ "let var x := 1"
        , "in"
        , "let var y := 2"
        , "in x + y"
        , "end"
        , "end"
        ] $ mkLet
        [ mkTypedVar "x" TInt Remaining (mkIntLit 1)
        ] $ mkLet
        [ mkTypedVar "y" TInt Remaining (mkIntLit 2)
        ] $ mkLVal (Var "x") $+ mkLVal (Var "y")
    , assertEscapeAnalysisML
        [ "let var x := 7"
        , "function fn(): int = x"
        , "in fn()"
        , "end"
        ] $ mkLet
        [ mkTypedVar "x" TInt Escaping (mkIntLit 7)
        , mkTypedFun "fn" [] TInt (mkLVal (Var "x"))
        ] $ mkCall "fn" []
    , assertEscapeAnalysisML
        [ "let var x := 7"
        , "function fn(x: int): int = x + 1"
        , "in fn(1)"
        , "end"
        ] $ mkLet
        [ mkTypedVar "x" TInt Remaining (mkIntLit 7)
        , mkTypedFun "fn"
            [TypedFunArg "x" TInt Remaining] TInt (mkLVal (Var "x") $+ mkIntLit 1)
        ] $ mkCall "fn" [mkIntLit 1]
    , assertEscapeAnalysisML
        [ "let var x := 7"
        , "function fn(): int ="
        , "let var y := 8"
        , "in"
        , "let var z := 9"
        , "in x + y + z"
        , "end"
        , "end"
        , "in fn()"
        , "end"
        ] $ mkLet
        [ mkTypedVar "x" TInt Escaping (mkIntLit 7)
        , mkTypedFun "fn" [] TInt $ mkLet
            [ mkTypedVar "y" TInt Remaining (mkIntLit 8)
            ] $ mkLet
            [ mkTypedVar "z" TInt Remaining (mkIntLit 9)
            ] $ (mkLVal (Var "x") $+ mkLVal (Var "y")) $+ mkLVal (Var "z")
        ] $ mkCall "fn" []
    , assertEscapeAnalysisML
        [ "let var x := 7"
        , "function fn(): int ="
        , "let var x := 8"
        , "in x + 1"
        , "end"
        , "in fn()"
        , "end"
        ] $ mkLet
        [ mkTypedVar "x" TInt Remaining (mkIntLit 7)
        , mkTypedFun "fn" [] TInt $ mkLet
            [ mkTypedVar "x" TInt Remaining (mkIntLit 8)
            ] $ mkLVal (Var "x") $+ mkIntLit 1
        ] $ mkCall "fn" []
    , assertEscapeAnalysisML
        [ "let function fn1(x: int): int ="
        , "let function fn2():int = x + 1"
        , "in fn2()"
        , "end"
        , "in fn1(1)"
        , "end"
        ] $ mkLet
        [ mkTypedFun "fn1" [TypedFunArg "x" TInt Escaping] TInt $ mkLet
            [ mkTypedFun "fn2" [] TInt $ mkLVal (Var "x") $+ mkIntLit 1
            ] $ mkCall "fn2" []
        ] $ mkCall "fn1" [mkIntLit 1]
    , assertEscapeAnalysisML
        [ "let function fn1(x: int): int ="
        , "let function fn2(): int ="
        , "let var x := 1"
        , "in x + 7"
        , "end"
        , "in fn2()"
        , "end"
        , "in fn1(1)"
        , "end"
        ] $ mkLet
        [ mkTypedFun "fn1" [TypedFunArg "x" TInt Remaining] TInt $ mkLet
            [ mkTypedFun "fn2" [] TInt $ mkLet
                [ mkTypedVar "x" TInt Remaining (mkIntLit 1)
                ] $ mkLVal (Var "x") $+ mkIntLit 7
            ] $ mkCall "fn2" []
        ] $ mkCall "fn1" [mkIntLit 1]
    , assertEscapeAnalysisML
        [ "let function fn1(x: int): int ="
        , "let function fn2(): int ="
        , "(1; let var x := 7 in x+2 end; x := x + 7; x)"
        , "in fn2()"
        , "end"
        , "in fn1(1)"
        , "end"
        ] $ mkLet
        [ mkTypedFun "fn1" [TypedFunArg "x" TInt Escaping] TInt $ mkLet
            [ mkTypedFun "fn2" [] TInt $ mkSeq
                [ mkIntLit 1
                , mkLet [ mkTypedVar "x" TInt Remaining (mkIntLit 7)
                        ] $ mkLVal (Var "x") $+ mkIntLit 2
                , mkAssign (Var "x") (mkLVal (Var "x") $+ mkIntLit 7)
                , mkLVal (Var "x")
                ]
            ] $ mkCall "fn2" []
        ] $ mkCall "fn1" [mkIntLit 1]
    , assertEscapeAnalysisML
        [ "let function fn1(): int = "
        , "let var i := 7"
        , "function fn2(): int ="
        , "if 2 < 1 then 0 else i + 1"
        , "in fn2()"
        , "end"
        , "in fn1()"
        , "end"
        ] $ mkLet
        [ mkTypedFun "fn1" [] TInt $ mkLet
            [ mkTypedVar "i" TInt Escaping (mkIntLit 7)
            , mkTypedFun "fn2" [] TInt $ mkIf
                [ mkIntLit 2 $< mkIntLit 1
                , mkIntLit 0
                , mkLVal (Var "i") $+ mkIntLit 1
                ]
            ] $ mkCall "fn2" []
        ] $ mkCall "fn1" []
    , assertEscapeAnalysisML
        [ "let function fn1(): int ="
        , "let var i := 7"
        , "function fn2(): int ="
        , "if 2 < 1"
        , "then let function fn3(): int = i + 1 in fn3() end"
        , "else 0"
        , "in fn2()"
        , "end"
        , "in fn1()"
        , "end"
        ] $ mkLet
        [ mkTypedFun "fn1" [] TInt $ mkLet
            [ mkTypedVar "i" TInt Escaping (mkIntLit 7)
            , mkTypedFun "fn2" [] TInt $ mkIf
                [ mkIntLit 2 $< mkIntLit 1
                , mkLet
                    [ mkTypedFun "fn3" [] TInt (mkLVal (Var "i") $+ mkIntLit 1)
                    ] $ mkCall "fn3" []
                , mkIntLit 0
                ]
            ] $ mkCall "fn2" []
        ] $ mkCall "fn1" []
    , assertEscapeAnalysisML
        [ "for i := 7 to 10 do"
        , "let var i := 33"
        , "in print(chr(i))"
        , "end"
        ] $ mkFor "i" Remaining (mkIntLit 7) (mkIntLit 10) $ mkLet
        [ mkTypedVar "i" TInt Remaining (mkIntLit 33)
        ] $ mkCall "print" [mkCall "chr" [mkLVal (Var "i")]]
    , assertEscapeAnalysisML
        [ "let var x := 0"
        , "in"
        , "for i := 0 to 10 do x := x + i;"
        , "x"
        , "end"
        ] $ mkLet
        [ mkTypedVar "x" TInt Remaining (mkIntLit 0)
        ] $ mkSeq
        [ mkFor "i" Remaining (mkIntLit 0) (mkIntLit 10)
            (mkAssign (Var "x") (mkLVal (Var "x") $+ mkLVal (Var "i")))
        , mkLVal (Var "x")
        ]
    , assertEscapeAnalysisML
        [ "let var i := 0"
        , "in"
        , "for i := 0 to 10 do"
        , "let function fn(): int = i + 1"
        , "in print(chr(fn()))"
        , "end;"
        , "i"
        , "end"
        ] $ mkLet
        [ mkTypedVar "i" TInt Remaining (mkIntLit 0)
        ] $ mkSeq
        [ mkFor "i" Escaping (mkIntLit 0) (mkIntLit 10) $ mkLet
            [ mkTypedFun "fn" [] TInt (mkLVal (Var "i") $+ mkIntLit 1)
            ] $ mkCall "print" [mkCall "chr" [mkCall "fn" []]]
        , mkLVal (Var "i")
        ]
    , assertEscapeAnalysisML
        [ "let var x := 0"
        , "in"
        , "for i := 0 to 10 do"
        , "let function fn():int = (x := x + i; x)"
        , "in print(chr(fn()))"
        , "end;"
        , "x"
        , "end"
        ] $ mkLet
        [ mkTypedVar "x" TInt Escaping (mkIntLit 0)
        ] $ mkSeq
        [ mkFor "i" Escaping (mkIntLit 0) (mkIntLit 10) $ mkLet
            [ mkTypedFun "fn" [] TInt $ mkSeq
                [ mkAssign (Var "x") (mkLVal (Var "x") $+ mkLVal (Var "i"))
                , mkLVal (Var "x")
                ]
            ] $ mkCall "print" [mkCall "chr" [mkCall "fn" []]]
        , mkLVal (Var "x")
        ]
    , assertEscapeAnalysisML
        [ "let var i := 7"
        , "in (while i < 10 do i := i + 1; i)"
        , "end"
        ] $ mkLet
        [ mkTypedVar "i" TInt Remaining (mkIntLit 7)
        ] $ mkSeq
        [ mkWhile (mkLVal (Var "i") $< mkIntLit 10) $
            mkAssign (Var "i") (mkLVal (Var "i") $+ mkIntLit 1)
        , mkLVal (Var "i")
        ]
    , assertEscapeAnalysisML
        [ "let var i := 7"
        , "in (while i < 10 do"
        , "let function fn() = i := i + 1"
        , "in fn()"
        , "end; i)"
        , "end"
        ] $ mkLet
        [ mkTypedVar "i" TInt Escaping (mkIntLit 7)
        ] $ mkSeq
        [ mkWhile (mkLVal (Var "i") $< mkIntLit 10) $ mkLet
            [ mkTypedFun "fn" [] TUnit $
                mkAssign (Var "i") (mkLVal (Var "i") $+ mkIntLit 1)
            ] $ mkCall "fn" []
        , mkLVal (Var "i")
        ]
    , assertEscapeAnalysisML
        [ "let var i := 8"
        , "function fn() = i := 7"
        , "in fn()"
        , "end"
        ] $ mkLet
        [ mkTypedVar "i" TInt Escaping (mkIntLit 8)
        , mkTypedFun "fn" [] TUnit (mkAssign (Var "i") (mkIntLit 7))
        ] $ mkCall "fn" []
    , assertEscapeAnalysisML
        [ "let function fn1(i: int) ="
        , "(print(chr(i));"
        , "let var i := 7"
        , "function fn2() = i := 1"
        , "in fn2()"
        , "end)"
        , "in fn1(2)"
        , "end"
        ] $ mkLet
        [ mkTypedFun "fn1" [TypedFunArg "i" TInt Remaining] TUnit $ mkSeq
            [ mkCall "print" [mkCall "chr" [mkLVal $ Var "i"]]
            , mkLet
                [ mkTypedVar "i" TInt Escaping (mkIntLit 7)
                , mkTypedFun "fn2" [] TUnit (mkAssign (Var "i") (mkIntLit 1))
                ] $ mkCall "fn2" []
            ]
        ] $ mkCall "fn1" [mkIntLit 2]
    , assertEscapeAnalysisML
        [ "let function fn1(i: int) ="
        , "let function fn2() = i := 1"
        , "var i := 7"
        , "in fn2()"
        , "end"
        , "in fn1(2)"
        , "end"
        ] $ mkLet
        [ mkTypedFun "fn1" [TypedFunArg "i" TInt Escaping] TUnit $ mkLet
            [ mkTypedFun "fn2" [] TUnit $ mkAssign (Var "i") (mkIntLit 1)
            , mkTypedVar "i" TInt Remaining $ mkIntLit 7
            ] $ mkCall "fn2" []
        ] $ mkCall "fn1" [mkIntLit 2]
    , assertEscapeAnalysisML
        [ "let function fn1(i: int) ="
        , "let function fn2() = i := 7"
        , "var i := 5"
        , "function fn3() = i := 1"
        , "in fn3()"
        , "end"
        , "in fn1(2)"
        , "end"
        ] $ mkLet
        [ mkTypedFun "fn1" [TypedFunArg "i" TInt Escaping] TUnit $ mkLet
            [ mkTypedFun "fn2" [] TUnit $ mkAssign (Var "i") (mkIntLit 7)
            , mkTypedVar "i" TInt Escaping (mkIntLit 5)
            , mkTypedFun "fn3" [] TUnit $ mkAssign (Var "i") (mkIntLit 1)
            ] $ mkCall "fn3" []
        ] $ mkCall "fn1" [mkIntLit 2]
    ]

assertEscapeAnalysisML :: [Text] -> Expr TypedDec -> Assertion
assertEscapeAnalysisML src = assertEscapeAnalysis (unlines src)

assertEscapeAnalysis :: Text -> Expr TypedDec -> Assertion
assertEscapeAnalysis src expected = case parseFromText "<string>" src of
    Left err ->
        assertFailure $ "Unexpected error parsing `" ++ unpack src ++ "`:\n" ++ err
    Right untyped -> case typeCheck src untyped of
        Left err ->
            assertFailure $ "Unexpected error type checking `" ++ unpack src ++ "`:\n"
                ++ unpack err
        Right typed -> do
            let actual = escapeAnalysis typed
            assertEqual ("When escape analysis " ++ unpack src)
                (ShowTypedExpr expected) (ShowTypedExpr actual)
