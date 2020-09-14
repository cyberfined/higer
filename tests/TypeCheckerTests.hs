{-# LANGUAGE OverloadedStrings #-}
module TypeCheckerTests (tests) where

import Test.HUnit
import Common
import Data.Text(Text, unpack, unlines)

import Tiger.Expr
import Tiger.Parser
import Tiger.TypeCheck

import Prelude hiding(unlines)

tests :: Test
tests = TestLabel "type checker tests" $
    TestList
      [ caseConstants
      , caseLVal
      , caseOps
      , caseAssignment
      , caseIf
      , caseCall
      , caseWhileFor
      , caseLet
      ]

caseConstants :: Test
caseConstants = mkTestLabel "constants type checking"
    [ assertTypeCheck "1234" $ mkIntLit 1234
    , assertTypeCheck "\"abcd\"" $ mkStrLit "abcd"
    , assertTypeCheck "nil" mkNil
    ]

caseLVal :: Test
caseLVal = mkTestLabel "type checking lvals"
    [ assertTypeCheck "let var v := 5 in v + 5 end" $
        mkLet [ TypedVar "v" TInt False (mkIntLit 5) ] (mkLVal (Var "v") $+ mkIntLit 5)
    , assertTypeCheck "let var v: int := 5 in v + 5 end" $
        mkLet [ TypedVar "v" TInt False (mkIntLit 5) ] (mkLVal (Var "v") $+ mkIntLit 5)
    , assertTypeCheck "let var v := \"abcd\" in v end" $
        mkLet [ TypedVar "v" TString False (mkStrLit "abcd") ] (mkLVal (Var "v"))
    , assertTypeCheck "let var v : string := \"abcd\" in v end" $
        mkLet [ TypedVar "v" TString False (mkStrLit "abcd") ] (mkLVal (Var "v"))
    , assertTypeCheckML
        [ "let type intarr = array of int"
        , "var arr := intarr [12] of 0"
        , "in arr[1] + 5"
        , "end"
        ] $ mkLet
        [ TypedType "intarr" (TArray TInt)
        , TypedVar "arr" (TArray TInt) False (mkArray "intarr" (mkIntLit 12) (mkIntLit 0))
        ] (mkLVal (mkIndex (Var "arr") (mkIntLit 1)) $+ mkIntLit 5)
    , assertTypeCheckML
        [ "let type intarr = array of int"
        , "var arr: intarr := intarr [12] of 0"
        , "in arr[1] + 5"
        , "end"
        ] $ mkLet
        [ TypedType "intarr" (TArray TInt)
        , TypedVar "arr" (TArray TInt) False (mkArray "intarr" (mkIntLit 12) (mkIntLit 0))
        ] (mkLVal (mkIndex (Var "arr") (mkIntLit 1)) $+ mkIntLit 5)
    , assertTypeCheckML
        [ "let type intarr = array of int"
        , "var arr := intarr [12] of 0"
        , "var x := 7"
        , "in arr[x * 5] := 7"
        , "end"
        ] $ mkLet
        [ TypedType "intarr" (TArray TInt)
        , TypedVar "arr" (TArray TInt) False (mkArray "intarr" (mkIntLit 12) (mkIntLit 0))
        , TypedVar "x" TInt False (mkIntLit 7)
        ] (mkAssign (mkIndex (Var "arr") (mkLVal (Var "x") $* mkIntLit 5)) (mkIntLit 7))
    , assertTypeCheckML
        [ "let type rec = {x: int}"
        , "var v := rec {x = 5}"
        , "in v.x + 5"
        , "end"
        ] $ mkLet
        [ TypedType "rec" (TRecord "rec" [("x", TInt)])
        , TypedVar "v" (TRecord "rec" [("x", TInt)]) False (mkRecord "rec" [("x", mkIntLit 5)])
        ] (mkLVal (mkDot (Var "v") "x" 0) $+ mkIntLit 5)
    , assertTypeCheckML
        [ "let type rec = {x: int}"
        , "var v : rec := rec {x = 5}"
        , "in v.x + 5"
        , "end"
        ] $ mkLet
        [ TypedType "rec" (TRecord "rec" [("x", TInt)])
        , TypedVar "v" (TRecord "rec" [("x", TInt)]) False (mkRecord "rec" [("x", mkIntLit 5)])
        ] (mkLVal (mkDot (Var "v") "x" 0) $+ mkIntLit 5)
    , assertTypeCheckML
        [ "let type rec = {x: int, y: string}"
        , "var v := rec{x = 5, y = \"abcd\"}"
        , "in v.x := 7; v.y := \"dcba\""
        , "end"
        ] $ mkLet
        [ TypedType "rec" (TRecord "rec" [("x", TInt), ("y", TString)])
        , TypedVar "v" (TRecord "rec" [("x", TInt), ("y", TString)]) False
            (mkRecord "rec" [("x", mkIntLit 5), ("y", mkStrLit "abcd")])
        ] $ mkSeq
        [ mkAssign (mkDot (Var "v") "x" 0) (mkIntLit 7)
        , mkAssign (mkDot (Var "v") "y" 1) (mkStrLit "dcba")
        ]
    , assertTypeCheckML
        [ "let type intlist = {val: int, rest: intlist}"
        , "var lst := intlist{val = 4, rest = intlist{val = 5, rest = nil}}"
        , "in lst.rest.val + 5"
        , "end"
        ] $ mkLet
        [ TypedType "intlist" (TRecord "intlist" [("val", TInt), ("rest", TSelf)])
        , TypedVar "lst" (TRecord "intlist" [("val", TInt), ("rest", TSelf)]) False
            (mkRecord "intlist" [ ("val", mkIntLit 4)
                                , ("rest", mkRecord "intlist" [ ("val", mkIntLit 5)
                                                              , ("rest", mkNil)
                                                              ]
                                  )
                                ]
            )
        ] (mkLVal (mkDot (mkDot (Var "lst") "rest" 1) "val" 0) $+ mkIntLit 5)
    , assertTypeCheckFailML
        [ "let var v := 5"
        , "in v := \"abcd\""
        , "end"
        ]
    , assertTypeCheckFailML
        [ "let var v := \"abcd\""
        , "in v := 4"
        , "end"
        ]
    , assertTypeCheckFailML
        [ "let type intarr = array of int"
        , "var v := intarr [12] of 0"
        , "in v[0] := \"abcd\""
        , "end"
        ]
    , assertTypeCheckFailML
        [ "let type strarr = array of string"
        , "var v := strarr [5] of \"abcd\""
        , "in v[1] := 7"
        , "end"
        ]
    , assertTypeCheckFailML
        [ "let type intarr = array of int"
        , "var v := intarr [5] of \"abcd\""
        , "in v[0] = 4"
        , "end"
        ]
    , assertTypeCheckFailML
        [ "let type intarr = array of int"
        , "type intmat = array of intarr"
        , "in 5"
        , "end"
        ]
    , assertTypeCheckFailML
        [ "let type rec = {x: int, y: string}"
        , "var v := rec{x = 5, y = \"abcd\"}"
        , "in v.y := 7"
        , "end"
        ]
    , assertTypeCheckFailML
        [ "let type rec = {x: int, x: string}"
        , "in 5"
        , "end"
        ]
    , assertTypeCheckFailML
        [ "let type rec = {x:int, y:string}"
        , "var v := rec {x = 5}"
        , "in v"
        , "end"
        ]
    , assertTypeCheckFailML
        [ "let type rec = {x:int, y:string}"
        , "var v := rec{x = 5, y = \"abcd\"}"
        , "in v.z"
        , "end"
        ]
    ]

caseOps :: Test
caseOps = mkTestLabel "operations type checking"
    [ assertTypeCheck "-2 * -3" $ mkNeg (mkIntLit 2) $* mkNeg (mkIntLit 3)
    , assertTypeCheck "2 + 3" $ (mkIntLit 2) $+ (mkIntLit 3)
    , assertTypeCheck "2 - 3" $ (mkIntLit 2) $- (mkIntLit 3)
    , assertTypeCheck "2 * 3" $ (mkIntLit 2) $* (mkIntLit 3)
    , assertTypeCheck "2 / 3" $ (mkIntLit 2) $/ (mkIntLit 3)
    , assertTypeCheck "2 > 3" $ (mkIntLit 2) $> (mkIntLit 3)
    , assertTypeCheck "2 >= 3" $ (mkIntLit 2) $>= (mkIntLit 3)
    , assertTypeCheck "2 < 3" $ (mkIntLit 2) $< (mkIntLit 3)
    , assertTypeCheck "2 <= 3" $ (mkIntLit 2) $<= (mkIntLit 3)
    , assertTypeCheckFail "-\"abcd\""
    , assertTypeCheckFail "2 + \"abcd\""
    , assertTypeCheckFail "\"abcd\" + 2"
    , assertTypeCheckFail "2 - \"abcd\""
    , assertTypeCheckFail "\"abcd\" - 2"
    , assertTypeCheckFail "2 * \"abcd\""
    , assertTypeCheckFail "\"abcd\" * 2"
    , assertTypeCheckFail "2 / \"abcd\""
    , assertTypeCheckFail "\"abcd\" / 2"
    , assertTypeCheckFail "2 > \"abcd\""
    , assertTypeCheckFail "\"abcd\" > 2"
    , assertTypeCheckFail "2 >= \"abcd\""
    , assertTypeCheckFail "\"abcd\" >= 2"
    , assertTypeCheckFail "2 < \"abcd\""
    , assertTypeCheckFail "\"abcd\" < 2"
    , assertTypeCheckFail "2 <= \"abcd\""
    , assertTypeCheckFail "\"abcd\" <= 2"
    ]

caseAssignment :: Test
caseAssignment = mkTestLabel "type checking assignment"
    [ assertTypeCheckML
        [ "let type list = {x:int}"
        , "var v:list := nil"
        , "in v"
        , "end"
        ] $ mkLet
        [ TypedType "list" (TRecord "list" [("x", TInt)])
        , TypedVar "v" (TRecord "list" [("x", TInt)]) False mkNil
        ] $ mkLVal (Var "v")
    , assertTypeCheckML
        [ "let type list = {x:int, next:list}"
        , "var v := list{x=1,next=nil}"
        , "in v.next = nil"
        , "end"
        ] $ mkLet
        [ TypedType "list" (TRecord "list" [("x", TInt), ("next", TSelf)])
        , TypedVar "v" (TRecord "list" [("x", TInt), ("next", TSelf)]) False
            (mkRecord "list" [("x", mkIntLit 1), ("next", mkNil)])
        ] (mkLVal (mkDot (Var "v") "next" 1) $== mkNil)
    , assertTypeCheckML
        [ "let type list = {x:int, next:list}"
        , "var v := list{x=1,next=nil}"
        , "in v.next <> nil"
        , "end"
        ] $ mkLet
        [ TypedType "list" (TRecord "list" [("x", TInt), ("next", TSelf)])
        , TypedVar "v" (TRecord "list" [("x", TInt), ("next", TSelf)]) False
            (mkRecord "list" [("x", mkIntLit 1), ("next", mkNil)])
        ] (mkLVal (mkDot (Var "v") "next" 1) $!= mkNil)
    , assertTypeCheckFailML
        [ "let var v := nil"
        , "in v"
        , "end"
        ]
    , assertTypeCheckFailML
        [ "let var v := print(4)"
        , "in v"
        , "end"
        ]
    , assertTypeCheckFail "nil = nil"
    , assertTypeCheckFail "nil <> nil"
    , assertTypeCheckFail "flush() <> flush()"
    , assertTypeCheckFail "flush() = flush()"
    , assertTypeCheckFailML
        [ "let var x := 12"
        , "var y := 13"
        , "in x + y := 7"
        , "end"
        ]
    , assertTypeCheckFailML
        [ "let var x := 12"
        , "in -x := 7"
        , "end"
        ]
    , assertTypeCheckFailML
        [ "let type rec1 = {x:int, y:int}"
        , "type rec2 = {x:int, y:int}"
        , "var v1 := rec1{x=1,y=2}"
        , "in v1 := rec2{x=2,y=3}"
        , "end"
        ]
    , assertTypeCheckFailML
        [ "let type list1 = {x:int, next:list1}"
        , "type list2 = {x:int, next:list2}"
        , "var v := list1{x=1, next=nil}"
        , "in v.next := list2{x=2, next=nil}"
        , "end"
        ]
    ]

caseIf :: Test
caseIf = mkTestLabel "type checking if expression"
    [ assertTypeCheck "if 2 < 5 then print(\"5\")" $
        mkIf [ (mkIntLit 2) $< (mkIntLit 5)
             , mkCall "print" [mkStrLit "5"]
             ]
    , assertTypeCheck "if 2 <> 4 then print(\"2\") else print(\"4\")" $
        mkIf [ (mkIntLit 2) $!= (mkIntLit 4)
             , mkCall "print" [mkStrLit "2"]
             , mkCall "print" [mkStrLit "4"]
             ]
    , assertTypeCheckML
        [ "let type rec = {x:int}"
        , "var v:rec := if 2 > 5 then nil else rec{x=1}"
        , "in v"
        , "end"
        ] $ mkLet
        [ TypedType "rec" (TRecord "rec" [("x", TInt)])
        , TypedVar "v" (TRecord "rec" [("x", TInt)]) False $
            mkIf [ (mkIntLit 2) $> (mkIntLit 5)
                 , mkNil
                 , mkRecord "rec" [("x", mkIntLit 1)]
                 ]
        ] $ mkLVal (Var "v")
    , assertTypeCheckML
        [ "let type rec = {x:int}"
        , "var v := if 2 > 5 then nil else rec{x=1}"
        , "in v"
        , "end"
        ] $ mkLet
        [ TypedType "rec" (TRecord "rec" [("x", TInt)])
        , TypedVar "v" (TRecord "rec" [("x", TInt)]) False $
            mkIf [ (mkIntLit 2) $> (mkIntLit 5)
                 , mkNil
                 , mkRecord "rec" [("x", mkIntLit 1)]
                 ]
        ] $ mkLVal (Var "v")
    , assertTypeCheckML
        [ "let type rec = {x:int}"
        , "var v := if 2 > 5 then rec{x=1} else nil"
        , "in v"
        , "end"
        ] $ mkLet
        [ TypedType "rec" (TRecord "rec" [("x", TInt)])
        , TypedVar "v" (TRecord "rec" [("x", TInt)]) False $
            mkIf [ (mkIntLit 2) $> (mkIntLit 5)
                 , mkRecord "rec" [("x", mkIntLit 1)]
                 , mkNil
                 ]
        ] $ mkLVal (Var "v")
    , assertTypeCheckFail "if 2 > 3 then 2 else \"abcd\""
    , assertTypeCheckFail "if 2 > 3 then \"abcd\" else 2"
    ]

caseCall :: Test
caseCall = mkTestLabel "type checking function calls"
    [ assertTypeCheckML
        [ "let function f(x:int, y:string):int = x"
        , "in f(3, \"abcd\")"
        , "end"
        ] $ mkLet
        [ TypedFun "f" [("x", TInt, False), ("y", TString, False)] TInt (mkLVal (Var "x")) ]
        (mkCall "f" [mkIntLit 3, mkStrLit "abcd"])
    , assertTypeCheckML
        [ "let function f() = print(\"abcd\")"
        , "in f()"
        , "end"
        ] $ mkLet
        [ TypedFun "f" [] TUnit (mkCall "print" [mkStrLit "abcd"]) ] $ mkCall "f" []
    , assertTypeCheckML
        [ "let function fib(n:int):int = if n <= 2 then 1 else fib(n-1) + fib(n-2)"
        , "in fib(55)"
        , "end"
        ] $ mkLet
        [ TypedFun "fib" [("n", TInt, False)] TInt $ mkIf
            [ mkLVal (Var "n") $<= mkIntLit 2
            , mkIntLit 1
            ,  mkCall "fib" [mkLVal (Var "n") $- mkIntLit 1]
            $+ mkCall "fib" [mkLVal (Var "n") $- mkIntLit 2]
            ]
        ] $ mkCall "fib" [mkIntLit 55]
    , assertTypeCheckML
        [ "let function is_even(n:int):int ="
        , "if n < 0 then is_even(-n) else if n=0 then 1 else is_odd(n-1)"
        , "function is_odd(n:int):int ="
        , "if n < 0 then is_odd(-n) else if n=0 then 0 else is_even(n-1)"
        , "in is_odd(5)"
        , "end"
        ] $ mkLet
        [ TypedFun "is_even" [("n", TInt, False)] TInt $
            mkIf [ mkLVal (Var "n") $< mkIntLit 0
                 , mkCall "is_even" [mkNeg (mkLVal (Var "n"))]
                 , mkIf [ mkLVal (Var "n") $== mkIntLit 0
                        , mkIntLit 1
                        , mkCall "is_odd" [mkLVal (Var "n") $- mkIntLit 1]
                        ]
                 ]
        , TypedFun "is_odd" [("n", TInt, False)] TInt $
            mkIf [ mkLVal (Var "n") $< mkIntLit 0
                 , mkCall "is_odd" [mkNeg (mkLVal (Var "n"))]
                 , mkIf [ mkLVal (Var "n") $== mkIntLit 0
                        , mkIntLit 0
                        , mkCall "is_even" [mkLVal (Var "n") $- mkIntLit 1]
                        ]
                 ]
        ] $ mkCall "is_odd" [mkIntLit 5]
    , assertTypeCheckML
        [ "let type rec = {x:int}"
        , "function f(): rec = rec { x = 1 }"
        , "var v := f()"
        , "in v"
        , "end"
        ] $ mkLet
        [ TypedType "rec" (TRecord "rec" [("x", TInt)])
        , TypedFun "f" [] (TRecord "rec" [("x", TInt)]) (mkRecord "rec" [("x", mkIntLit 1)])
        , TypedVar "v" (TRecord "rec" [("x", TInt)]) False (mkCall "f" [])
        ] $ mkLVal (Var "v")
    , assertTypeCheckML
        [ "let type rec = {x:int}"
        , "function f(): rec = nil"
        , "var v := f()"
        , "in v"
        , "end"
        ] $ mkLet
        [ TypedType "rec" (TRecord "rec" [("x", TInt)])
        , TypedFun "f" [] (TRecord "rec" [("x", TInt)]) mkNil
        , TypedVar "v" (TRecord "rec" [("x", TInt)]) False (mkCall "f" [])
        ] $ mkLVal (Var "v")
    , assertTypeCheckML
        [ "let type rec = {x:int}"
        , "function f(y:rec):int = y.x"
        , "in f(nil)"
        , "end"
        ] $ mkLet
        [ TypedType "rec" (TRecord "rec" [("x", TInt)])
        , TypedFun "f" [("y", TRecord "rec" [("x", TInt)], False)] TInt
            (mkLVal (mkDot (Var "y") "x" 0))
        ] $ mkCall "f" [mkNil]
    , assertTypeCheckFailML
        [ "let function f(x:int,y:string):int = x"
        , "in f(\"abcd\", \"test\")"
        , "end"
        ]
    , assertTypeCheckFailML
        [ "let function f():int = 5"
        , "in f(1)"
        , "end"
        ]
    ]

caseWhileFor :: Test
caseWhileFor = mkTestLabel "while, for type checking"
    [ assertTypeCheckML
        [ "let var i := 49"
        , "in while i < 54 do (i := i+1; print(chr(i)))"
        , "end"
        ] $ mkLet
        [ TypedVar "i" TInt False (mkIntLit 49) ] $
            mkWhile (mkLVal (Var "i") $< mkIntLit 54) $
                mkSeq [ mkAssign (Var "i") (mkLVal (Var "i") $+ mkIntLit 1)
                      , mkCall "print" [mkCall "chr" [mkLVal (Var "i")]]
                      ]
    , assertTypeCheckML
        [ "let var i := 49"
        , "in while i < 54 do (if i > 50 then break; print(chr(i)))"
        , "end"
        ] $ mkLet
        [ TypedVar "i" TInt False (mkIntLit 49) ] $
            mkWhile (mkLVal (Var "i") $< mkIntLit 54) $
                mkSeq [ mkIf [ mkLVal (Var "i") $> mkIntLit 50
                             , mkBreak
                             ]
                      , mkCall "print" [mkCall "chr" [mkLVal (Var "i")]]
                      ]
    , assertTypeCheck "for i := 49 to 54 do print(chr(i))" $
        mkFor "i" False (mkIntLit 49) (mkIntLit 54)
            (mkCall "print" [mkCall "chr" [mkLVal (Var "i")]])
    , assertTypeCheck "for i := 49 to 54 do (if i > 50 then break; print(chr(i)))" $
        mkFor "i" False (mkIntLit 49) (mkIntLit 54) $
            mkSeq [ mkIf [ mkLVal (Var "i") $> mkIntLit 50
                         , mkBreak
                         ]
                  , mkCall "print" [mkCall "chr" [mkLVal (Var "i")]]
                  ]
    , assertTypeCheckFailML
        [ "let var x := 4"
        , "in (break; x)"
        , "end"
        ]
    ]

caseLet :: Test
caseLet = mkTestLabel "let type checking"
    [ assertTypeCheckFailML
        [ "let type a = b"
        , "type b = c"
        , "type c = d"
        , "type d = a"
        , "in 5"
        , "end"
        ]
    ]

assertTypeCheckML :: [Text] -> Expr TypedDec -> Assertion
assertTypeCheckML src = assertTypeCheck (unlines src)

assertTypeCheck :: Text -> Expr TypedDec -> Assertion
assertTypeCheck src expected = case parseFromText "<string>" src of
    Left err ->
        assertFailure $ "Unexpected error parsing `" ++ unpack src ++ "`:\n" ++ err
    Right untyped -> case typeCheck src untyped of
        Left err ->
            assertFailure $ "Unexpected error type checking `" ++ unpack src ++ "`:\n"
                ++ unpack err
        Right actual ->
            assertEqual ("When type checking " ++ unpack src)
                (ShowTypedExpr expected) (ShowTypedExpr actual)

assertTypeCheckFailML :: [Text] -> Assertion
assertTypeCheckFailML = assertTypeCheckFail . unlines

assertTypeCheckFail :: Text -> Assertion
assertTypeCheckFail src = case parseFromText "<string>" src of
    Left err ->
        assertFailure $ "Unexpected error parsing `" ++ unpack src ++ "`:\n" ++ err
    Right untyped -> case typeCheck src untyped of
        Left _ -> return ()
        Right res ->
            assertFailure $ "Unexpected success type checking `"
                ++ unpack src ++ "`:\ntype checked value "
                    ++ show (ShowTypedExpr res)
