{-# LANGUAGE OverloadedStrings #-}

module ParserTests (tests) where

import Data.Text (Text, unpack)
import Test.HUnit

import Common
import Tiger.Expr
import Tiger.Parser

tests :: Test
tests = TestLabel "parser tests" $
    TestList
      [ caseConstants
      , caseStrings
      , caseLVal
      , caseOps
      , caseRecArrCall
      , caseAssignment
      , caseIfWhileFor
      , caseLet
      ]

caseConstants :: Test
caseConstants = mkTestLabel "constants parsing"
    [ assertParse "1234" $ mkIntLit 1234
    , assertParse "nil" mkNil
    ]

caseStrings :: Test
caseStrings = mkTestLabel "strings parsing"
    [ assertParse "\"\"" $ mkStrLit ""
    , assertParse "\"abcd\"" $ mkStrLit "abcd"
    , assertParse "\"abcd abcd\"" $ mkStrLit "abcd abcd"
    , assertParse "\"abcd\\nabcd\"" $ mkStrLit "abcd\nabcd"
    , assertParse "\"abcd\\tabcd\"" $ mkStrLit "abcd\tabcd"
    , assertParse "\"abcd\\\"abcd\\\"\"" $ mkStrLit "abcd\"abcd\""
    , assertParse "\"abcd\\\\abcd\"" $ mkStrLit "abcd\\abcd"
    , assertParse "\"abcd\\033abcd\"" $ mkStrLit "abcd!abcd"
    , assertParse "\"abcd\\\"abcd\\\"\"" $ mkStrLit "abcd\"abcd\""
    , assertParse "\"abcd\\   \n\r   \\abcd\"" $ mkStrLit "abcdabcd"
    , assertParseFail "\"abcd"
    , assertParseFail "\"abcd\\rabcd\""
    , assertParseFail "\"abcd\\128\""
    , assertParseFail "\"abcd\\   illegal   \\abcd\""
    ]

caseLVal :: Test
caseLVal = mkTestLabel "lvals parsing"
    [ assertParse "x" $ mkLVal (Var "x")
    , assertParse "x1" $ mkLVal (Var "x1")
    , assertParse "x_1" $ mkLVal (Var "x_1")
    , assertParse "x1y2" $ mkLVal (Var "x1y2")
    , assertParse "x.y" $ mkLVal (mkDot (Var "x") "y" 0)
    , assertParse "x.y.z" $ mkLVal (mkDot (mkDot (Var "x") "y" 0) "z" 0)
    , assertParse "x[0]" $ mkLVal (mkIndex (Var "x") (mkIntLit 0))
    , assertParse "x[y+z]" $
        mkLVal (mkIndex (Var "x") (mkLVal (Var "y") $+ mkLVal (Var "z")))
    , assertParse "x[0].y[1].z[2]" $
        mkLVal $ mkIndex
            (mkDot
                (mkIndex (mkDot (mkIndex (Var "x") (mkIntLit 0)) "y" 0) (mkIntLit 1))
                "z"
                0)
            (mkIntLit 2)
    , assertParse "x[y.z[0] * 2]" $
        mkLVal $
            mkIndex (Var "x")
            (mkLVal (mkIndex (mkDot (Var "y") "z" 0) (mkIntLit 0)) $* mkIntLit 2)
    , assertParse "x[0] + y[1]" $
        mkLVal (mkIndex (Var "x") (mkIntLit 0)) $+
        mkLVal (mkIndex (Var "y") (mkIntLit 1))
    , assertParseFail "_test"
    , assertParseFail "_123"
    , assertParseFail "123test"
    , assertParseFail "x[0][1]"
    , assertParseFail "if"
    , assertParseFail "if.x"
    , assertParseFail "for[1]"
    , assertParseFail "x.while[1]"
    , assertParseFail "x[0].break"
    , assertParseFail "if + to"
    ]

caseOps :: Test
caseOps = mkTestLabel "operations parsing"
    [ assertParse "-x" $ mkNeg (mkLVal (Var "x"))
    , assertParse "x + y" $ mkLVal (Var "x") $+ mkLVal (Var "y")
    , assertParse "x - y" $ mkLVal (Var "x") $- mkLVal (Var "y")
    , assertParse "x * y" $ mkLVal (Var "x") $* mkLVal (Var "y")
    , assertParse "x / y" $ mkLVal (Var "x") $/ mkLVal (Var "y")
    , assertParse "x = y" $ mkLVal (Var "x") $== mkLVal (Var "y")
    , assertParse "x <> y" $ mkLVal (Var "x") $!= mkLVal (Var "y")
    , assertParse "x > y" $ mkLVal (Var "x") $> mkLVal (Var "y")
    , assertParse "x >= y" $ mkLVal (Var "x") $>= mkLVal (Var "y")
    , assertParse "x < y" $ mkLVal (Var "x") $< mkLVal (Var "y")
    , assertParse "x <= y" $ mkLVal (Var "x") $<= mkLVal (Var "y")
    , assertParse "x & y" $ mkLVal (Var "x") $&& mkLVal (Var "y")
    , assertParse "x | y" $ mkLVal (Var "x") $|| mkLVal (Var "y")
    , assertParse "-x + -y*z" $
        mkNeg (mkLVal (Var "x")) $+ (mkNeg (mkLVal (Var "y")) $* mkLVal (Var "z"))
    , assertParse "x + y*z/5" $
        mkLVal (Var "x") $+ ((mkLVal (Var "y") $* mkLVal (Var "z")) $/ mkIntLit 5)
    , assertParse "-(x - y) * z" $
        mkNeg (mkLVal (Var "x") $- mkLVal (Var "y")) $* mkLVal (Var "z")
    , assertParse "x >= y & y >= z | z < 5" $
        (mkLVal (Var "x") $>= mkLVal (Var "y")   $&&
        (mkLVal (Var "y") $>= mkLVal (Var "z"))) $||
        (mkLVal (Var "z") $< mkIntLit 5)
    , assertParse "x + 12 <> y + 11 | z = 2" $
        ((mkLVal (Var "x") $+ mkIntLit 12) $!=
        (mkLVal (Var "y") $+ mkIntLit 11)) $||
        (mkLVal (Var "z") $== mkIntLit 2)
    ]

caseRecArrCall :: Test
caseRecArrCall = mkTestLabel "record, array, call parsing"
    [ assertParse "rec {}" $ mkRecord "rec" []
    , assertParse "rec {x=1}" $ mkRecord "rec" [RecordField "x" (mkIntLit 1)]
    , assertParse "rec { x = 1, y = 2 }" $
        mkRecord "rec" [RecordField "x" (mkIntLit 1), RecordField "y" (mkIntLit 2)]
    , assertParse "arr [ 12 ] of 0" $ mkArray "arr" (mkIntLit 12) (mkIntLit 0)
    , assertParse "test()" $ mkCall "test" []
    , assertParse "test(x+y, 12)" $
        mkCall "test" [mkLVal (Var "x") $+ mkLVal (Var "y"), mkIntLit 12]
    , assertParse "test1() + test2()" $ mkCall "test1" [] $+ mkCall "test2" []
    , assertParseFail "if {}"
    , assertParseFail "for [12] of 1"
    , assertParseFail "break [11] of for"
    , assertParseFail "if()"
    , assertParseFail "break(x,y)"
    ]

caseAssignment :: Test
caseAssignment = mkTestLabel "assignment parsing"
    [ assertParse "x := y" $ mkAssign (Var "x") (mkLVal (Var "y"))
    , assertParse "x.y := z + w" $
        mkAssign (mkDot (Var "x") "y" 0) (mkLVal (Var "z") $+ mkLVal (Var "w"))
    , assertParse "x[0].y[1] := z.w" $ mkAssign
        (mkIndex (mkDot (mkIndex (Var "x") (mkIntLit 0)) "y" 0) (mkIntLit 1))
        (mkLVal $ mkDot (Var "z") "w" 0)
    , assertParseFail "12 := 7"
    , assertParseFail "\"hello\" := \"world\""
    , assertParseFail "test() := x"
    , assertParseFail "nil := x"
    , assertParseFail "array [ 12 ] of 0 := x"
    , assertParseFail "rec {} := test()"
    ]

caseIfWhileFor :: Test
caseIfWhileFor = mkTestLabel "if, while, for parsing"
    [ assertParse "if x >= y then print(z)" $
        mkIf [ mkLVal (Var "x") $>= mkLVal (Var "y")
             , mkCall "print" [mkLVal (Var "z")]
             ]
    , assertParse "if x >= y & y = z then y else z" $
        mkIf [ (mkLVal (Var "x") $>= mkLVal (Var "y")) $&&
               (mkLVal (Var "y") $== mkLVal (Var "z"))
             , mkLVal (Var "y")
             , mkLVal (Var "z")
             ]
    , assertParse "if x > y then if z < y then z else y" $
        mkIf [ mkLVal (Var "x") $> mkLVal (Var "y")
             , mkIf [ mkLVal (Var "z") $< mkLVal (Var "y")
                    , mkLVal (Var "z")
                    , mkLVal (Var "y")
                    ]
             ]
    , assertParse "if x > y then x else if y > z then y else z" $
        mkIf [ mkLVal (Var "x") $> mkLVal (Var "y")
             , mkLVal (Var "x")
             , mkIf [ mkLVal (Var "y") $> mkLVal (Var "z")
                    , mkLVal (Var "y")
                    , mkLVal (Var "z")
                    ]
             ]
    , assertParse "if x > y then if x > z then x else z else if y > z then y else z" $
        mkIf [ mkLVal (Var "x") $> mkLVal (Var "y")
             , mkIf [ mkLVal (Var "x") $> mkLVal (Var "z")
                    , mkLVal (Var "x")
                    , mkLVal (Var "z")
                    ]
             , mkIf [ mkLVal (Var "y") $> mkLVal (Var "z")
                    , mkLVal (Var "y")
                    , mkLVal (Var "z")
                    ]
             ]
    , assertParse "while i > 0 do i := i-1" $
        mkWhile (mkLVal (Var "i") $> mkIntLit 0)
                (mkAssign (Var "i") (mkLVal (Var "i") $- mkIntLit 1))
    , assertParse "for i:=1 to 7 do (print(i-1); print(i))" $
        mkFor "i" Remaining (mkIntLit 1) (mkIntLit 7) $
            mkSeq [ mkCall "print" [mkLVal (Var "i") $- mkIntLit 1]
                  , mkCall "print" [mkLVal (Var "i")]
                  ]
    , assertParse "while i < 7 do (if i = 6 then break; i := i + 1)" $
        mkWhile (mkLVal (Var "i") $< mkIntLit 7) $
            mkSeq [ mkIf [ mkLVal (Var "i") $== mkIntLit 6
                         , mkBreak
                         ]
                  , mkAssign (Var "i") (mkLVal (Var "i") $+ mkIntLit 1)
                  ]
    ]

caseLet :: Test
caseLet = mkTestLabel "let parsing"
    [ assertParse "let type t = int in 0 end" $
        mkLet [ mkUntypedType "t" (TypeId "int") ] (mkIntLit 0)
    , assertParse "let type t = {x: string, y: int} in 5 end" $ mkLet
        [ mkUntypedType "t"
            (TypeRecord [RecordField "x" "string", RecordField "y" "int"])
        ]
        (mkIntLit 5)
    , assertParse "let type t = array of int in 5 end" $
        mkLet [ mkUntypedType "t" (TypeArray "int") ] (mkIntLit 5)
    , assertParse "let var x := 12 in print(x) end" $ mkLet
        [ mkUntypedVar "x" Nothing (mkIntLit 12) ] (mkCall "print" [mkLVal (Var "x")])
    , assertParse "let var x : int := 12 in x end" $
        mkLet [ mkUntypedVar "x" (Just "int") (mkIntLit 12) ] (mkLVal (Var "x"))
    , assertParse "let function p5() = print(5) in p5() end" $ mkLet
        [ mkUntypedFun "p5" [] Nothing (mkCall "print" [mkIntLit 5]) ] (mkCall "p5" [])
    , assertParse "let function id (x: int) = x in id(5) end" $
        mkLet [ mkUntypedFun "id" [UntypedFunArg "x" "int"] Nothing (mkLVal (Var "x")) ] $
            mkCall "id" [mkIntLit 5]
    , assertParse "let function add5(x:int): int = x+5 in add5(1) end" $ mkLet
        [ mkUntypedFun "add5"
            [UntypedFunArg "x" "int"]
            (Just "int")
            (mkLVal (Var "x") $+ mkIntLit 5)
        ]
        (mkCall "add5" [mkIntLit 1])
    , assertParse "let type t = int var x : t := 12 in x := x + 1; print(x) end" $
        mkLet [ mkUntypedType "t" (TypeId "int")
              , mkUntypedVar "x" (Just "t") (mkIntLit 12)
              ] $ mkSeq
              [ mkAssign (Var "x") (mkLVal (Var "x") $+ mkIntLit 1)
              , mkCall "print" [mkLVal (Var "x")]
              ]
    , assertParseFail "let type rec = {} in 5 end"
    ]

assertParse :: Text -> Expr UntypedDec -> Assertion
assertParse src expected = case parseFromText "<string>" src of
    Left err ->
        assertFailure $ "Unexpected error parsing `" ++ unpack src ++ "`:\n" ++ err
    Right actual ->
        assertEqual ("When parsing " ++ unpack src)
            (ShowUntypedExpr expected) (ShowUntypedExpr $ stripAnnotation actual)

assertParseFail :: Text -> Assertion
assertParseFail src = case parseFromText "<string>" src of
    Left _ -> return ()
    Right res ->
        assertFailure $ "Unexpected success parsing `"
            ++ unpack src ++ "`:\nParsed value "
            ++ show (ShowUntypedExpr $ stripAnnotation res)
