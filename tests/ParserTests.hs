module ParserTests (tests) where

import           Data.Text    (Text)
import           Prelude      hiding (and, break, div, exp, or, rem, seq, span)
import           Test.HUnit   hiding (path)
import           Tiger.Parser (parse)

import           Common

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

tests :: Test
tests = TestLabel "parser tests" $ TestList [ nilTest
                                            , lValueTests
                                            , intLitTest
                                            , strLitTests
                                            , operationTests
                                            , recordTests
                                            , arrayTest
                                            , assignTest
                                            , ifTests
                                            , whileTest
                                            , forTest
                                            , seqTests
                                            , callTests
                                            , breakTest
                                            , letTests
                                            , commentTests
                                            , fileCases
                                            ]

lValueTests :: Test
lValueTests = mkTestLabel "parses string" [ varTest
                                          , indexTest
                                          , dotTest
                                          , complexLValueTest
                                          ]
  where varTest = success "test" $ lval (var "test")
        indexTest = success "test[123]" $ lval $ index (var "test") (int 123)
        dotTest = success "test1.test2" $ lval $ dot (var "test1") "test2"
        complexLValueTest = success "t1[123].t2[1][2].t3" $
            lval $ dot (index (index (dot (index (var "t1") (int 123)) "t2") (int 1))
                (int 2)) "t3"

nilTest :: Test
nilTest = mkTestLabel "parses nil value" [success "nil" nil]

intLitTest :: Test
intLitTest = mkTestLabel "parses int value" [success "123" $ int 123]

strLitTests :: Test
strLitTests = mkTestLabel "parses string" [ escapeSequenceTest
                                          , controlCharTest
                                          , asciiCodeTest
                                          , multilineStringTest
                                          , wrongAsciiCodeTest
                                          , wrongEscapeSequenceTest
                                          , wrongControlCharacterTest
                                          , withoutQuoteTest
                                          ]
  where escapeSequenceTest = success "\"Hello\\n\\t\\\"\\\\\"" $ str "Hello\n\t\"\\"
        controlCharTest = success "\"Hello \\^@\\^G\\^H\\^I\\^J\\^K\\^L\\^M\\^Z\
                                  \\\^g\\^h\\^i\\^j\\^k\\^l\\^m\\^z\"" $
                                      str "Hello \0\a\b\t\n\v\f\r\032\a\b\t\n\v\f\r\032"
        asciiCodeTest = success "\"\\084\\101\\115\\116\"" $ str "Test"
        multilineStringTest = success "\"Hello\\\n      \\ World\"" $ str "Hello World"
        wrongAsciiCodeTest = failure "\"\\128\""
        wrongEscapeSequenceTest = failure "\"\\r\""
        wrongControlCharacterTest = failure "\"\\^D\""
        withoutQuoteTest = failure "\"Hello"

operationTests :: Test
operationTests = mkTestLabel "parses operations"
    [ success "-12" $ neg (int 12)
    , success "-(12 + 45)" $ neg (add (int 12) (int 45))
    , success "a + b * c" $ add (lval $ var "a") (mul (lval $ var "b") (lval $ var "c"))
    , success "(a + b) * c" $ mul (add (lval $ var "a") (lval $ var "b")) (lval $ var "c")
    , success "c * a - 12" $ sub (mul (lval $ var "c") (lval $ var "a")) (int 12)
    , success "c * (a - 12)" $ mul (lval $ var "c") (sub (lval $ var "a") (int 12))
    , success "a[1] / b.s / c" $ div (div (lval $ index (var "a") (int 1))
                                          (lval $ dot (var "b") "s"))
                                     (lval $ var "c")
    , success "12 / 3 * 4" $ mul (div (int 12) (int 3)) (int 4)
    , success "a > b" $ gt (lval $ var "a") (lval $ var "b")
    , success "a > (b > 12)" $ gt (lval $ var "a") (gt (lval $ var "b") (int 12))
    , success "a >= b" $ ge (lval $ var "a") (lval $ var "b")
    , success "(a >= b) >= nil" $ ge (ge (lval $ var "a") (lval $ var "b"))
                                               nil
    , success "a < b.s[1]" $ lt (lval $ var "a") (lval $ index (dot (var "b") "s")
                                                               (int 1))
    , success "(a < 12) < 13" $ lt (lt (lval $ var "a") (int 12)) (int 13)
    , success "a[1] <= nil" $ le (lval $ index (var "a") (int 1)) nil
    , success "11 <= (12 <= 13)" $ le (int 11) (le (int 12) (int 13))
    , success "a <> 2" $ ne (lval $ var "a") (int 2)
    , success "a < b <> b > c" $ ne (lt (lval $ var "a") (lval $ var "b"))
                                    (gt (lval $ var "b") (lval $ var "c"))
    , success "a = 2" $ eq (lval $ var "a") (int 2)
    , success "a < b = b <= c" $ eq (lt (lval $ var "a") (lval $ var "b"))
                                    (le (lval $ var "b") (lval $ var "c"))
    , failure "a > b > 12"
    , failure "a > b >= 12"
    , failure "a <> b <> c"
    , failure "a = b = c"
    , failure "a = b <> c"
    , failure "a <> b = c"
    , success "a = 2 & b <> 3 | c <= d" $ or (and (eq (lval $ var "a") (int 2))
                                                   (ne (lval $ var "b") (int 3)))
                                             (le (lval $ var "c") (lval $ var "d"))
    , success "a = 2 & (b <> 3 | c <= d)" $ and (eq (lval $ var "a") (int 2))
                                                (or (ne (lval $ var "b") (int 3))
                                                    (le (lval $ var "c")
                                                    (lval $ var "d")))
    ]

recordTests :: Test
recordTests = mkTestLabel "record creation tests"
    [ success "a := kek {}" $ assign (var "a") (record "kek" [])
    , success "a := kek { lol = 123, rofl = \"Hello\" }" $
        assign (var "a") (record "kek" [ field "lol" (int 123)
                                       , field "rofl" (str "Hello")
                                       ])
    ]

arrayTest :: Test
arrayTest = mkTestLabel "array creation test"
    [ success "a := kek [b] of c.s" $ assign (var "a")
                                             (array "kek" (lval $ var "b")
                                                          (lval $ dot (var "c") "s"))
    ]

assignTest :: Test
assignTest = mkTestLabel "assignment test"
    [ success "a := b[123].c" $ assign (var "a") (lval $ dot (index (var "b") (int 123))
                                                              "c")
    ]

ifTests :: Test
ifTests = mkTestLabel "if expression tests"
    [ success "if a <> b then print(\"hello\")" $
        if' [ ne (lval $ var "a") (lval $ var "b")
            , call "print" [str "hello"]
            ]
    , success "if a = b then print(\"hello\") else print(\"world\")" $
        if' [ eq (lval $ var "a") (lval $ var "b")
            , call "print" [str "hello"]
            , call "print" [str "world"]
            ]
    , success "print(if a = 1 then \"one\" else if a = 2 then \"two\" else \"three\")" $
        call "print" [ if' [ eq (lval $ var "a") (int 1)
                           , str "one"
                           , if' [ eq (lval $ var "a") (int 2)
                                 , str "two"
                                 , str "three"
                                 ]
                           ]
                     ]
    ]

whileTest :: Test
whileTest = mkTestLabel "while expression test"
    [ success "while a <= b * 12 do\n (print(a); a := a + 1)" $
        while (le (lval $ var "a") (mul (lval $ var "b") (int 12))) $
            seq [ call "print" [ lval $ var "a" ]
                , assign (var "a") (add (lval $ var "a") (int 1))
                ]
    ]

forTest :: Test
forTest = mkTestLabel "for expression test"
    [ success "for i := 1 to a do\n b := b * i" $
        for "i" rem (int 1) (lval $ var "a") $
            assign (var "b") (mul (lval $ var "b") (lval $ var "i"))
    ]

seqTests :: Test
seqTests = mkTestLabel "expression sequence tests"
    [ success "(12 + 45)" $ add (int 12) (int 45)
    , success "(print(toStr(a)); a := a / 5)" $
        seq [ call "print" [ call "toStr" [ lval $ var "a" ] ]
            , assign (var "a") (div (lval $ var "a") (int 5))
            ]
    ]

callTests :: Test
callTests = mkTestLabel "function call tests"
    [ success "kek()" $ call "kek" []
    , success "kek(123, b.c[1])" $ call "kek" [ int 123
                                              , lval $ index (dot (var "b") "c") (int 1)
                                              ]
    ]

breakTest :: Test
breakTest = mkTestLabel "break expression test"
    [ success "for i := 0 to 10 do\n(if i >= 9 then break; print(i))" $
        for "i" rem (int 0) (int 10) $
            seq $ [ if' [ ge (lval $ var "i") (int 9)
                        , break
                        ]
                  , call "print" [lval $ var "i"]
                  ]
    ]

letTests :: Test
letTests = mkTestLabel "let expression tests"
    [ success "let\n  type kek = string\n  var i: kek := \"hello\"\nin i\nend" $
        let' [ typeDecs [typeAlias "kek" "string"]
             , varDec "i" (Just "kek") rem $ str "hello"
             ]
             (lval $ var "i")
    , success "let\n  type lol = array of int\n  var kek := lol [123] of b.c\n\
              \in kek\nend" $
        let' [ typeDecs [arrayType "lol" "int"]
             , varDec "kek" Nothing rem $ array "lol" (int 123)
                (lval $ dot (var "b") "c")
             ]
             (lval $ var "kek")
    , success "let\n  type kek = string\n  type lol = { rofl: kek }\n\
              \  var i := lol { rofl = \"hello\\n\" }\nin i\nend" $
        let' [ typeDecs [ typeAlias "kek" "string"
                        , recordType "lol" [decField "rofl" rem "kek"]
                        ]
             , varDec "i" Nothing rem $ record "lol" [field "rofl" $ str "hello\n"]
             ]
             (lval $ var "i")
    , success "let\n  function hello() = print(\"hello\")\nin hello()\nend" $
        let' [ funDecs [funDec "hello" [] Nothing $ call "print" [str "hello"]]
             ]
             (call "hello" [])
    , success "let\n  function add5(a: int): int = a + 5\nin add5(6)\nend" $
        let' [ funDecs [funDec "add5" [decField "a" rem "int"] (Just "int") $
                            add (lval $ var "a") (int 5)
                       ]
             ]
             (call "add5" [int 6])
    , success "let\n  function ping(i: int, lim: int) = if i < lim \
              \then (print(\"ping\"); pong(i + 1, lim))\n  \
              \function pong(i: int, lim: int) = if i < lim \
              \then (print(\"pong\"); ping(i + 1, lim))\n  \
              \in ping(12)\nend" $
        let' [ funDecs [ funDec "ping"
                           [ decField "i" rem "int"
                           , decField "lim" rem "int"
                           ]
                           Nothing
                           (if' [ lt (lval $ var "i") (lval $ var "lim")
                                , seq [ call "print" [str "ping"]
                                      , call "pong"
                                          [ add (lval $ var "i") (int 1)
                                          , lval $ var "lim"
                                          ]
                                      ]
                                ]
                           )
                       , funDec "pong"
                           [ decField "i" rem "int"
                           , decField "lim" rem "int"
                           ]
                           Nothing
                           (if' [ lt (lval $ var "i") (lval $ var "lim")
                                , seq [ call "print" [str "pong"]
                                      , call "ping"
                                          [ add (lval $ var "i") (int 1)
                                          , lval $ var "lim"
                                          ]
                                      ]
                                ]
                           )
                       ]
             ]
             (call "ping" [int 12])
    ]

commentTests :: Test
commentTests = mkTestLabel "comment tests"
    [ success "/* comment */\nlet\n  var i: int := 123\nin i\nend" $
        let' [varDec "i" (Just "int") rem $ int 123] (lval $ var "i")
    , success "let\n  /* counter */\n  var i := 123\nin i\nend" $
        let' [varDec "i" Nothing rem $ int 123] (lval $ var "i")
    ]

fileCases :: Test
fileCases = mkTestLabel "successful test cases"
    [ successFile "testcases/test1.tig"
    , successFile "testcases/test2.tig"
    , successFile "testcases/test3.tig"
    , successFile "testcases/test4.tig"
    , successFile "testcases/test5.tig"
    , successFile "testcases/test6.tig"
    , successFile "testcases/test7.tig"
    , successFile "testcases/test8.tig"
    , successFile "testcases/test9.tig"
    , successFile "testcases/test10.tig"
    , successFile "testcases/test11.tig"
    , successFile "testcases/test12.tig"
    , successFile "testcases/test13.tig"
    , successFile "testcases/test14.tig"
    , successFile "testcases/test15.tig"
    , successFile "testcases/test16.tig"
    , successFile "testcases/test17.tig"
    , successFile "testcases/test18.tig"
    , successFile "testcases/test19.tig"
    , successFile "testcases/test20.tig"
    , successFile "testcases/test21.tig"
    , successFile "testcases/test22.tig"
    , successFile "testcases/test23.tig"
    , successFile "testcases/test24.tig"
    , successFile "testcases/test25.tig"
    , successFile "testcases/test26.tig"
    , successFile "testcases/test27.tig"
    , successFile "testcases/test28.tig"
    , successFile "testcases/test29.tig"
    , successFile "testcases/test30.tig"
    , successFile "testcases/test31.tig"
    , successFile "testcases/test32.tig"
    , successFile "testcases/test33.tig"
    , successFile "testcases/test34.tig"
    , successFile "testcases/test35.tig"
    , successFile "testcases/test36.tig"
    , successFile "testcases/test37.tig"
    , successFile "testcases/test38.tig"
    , successFile "testcases/test39.tig"
    , successFile "testcases/test40.tig"
    , successFile "testcases/test41.tig"
    , successFile "testcases/test42.tig"
    , successFile "testcases/test43.tig"
    , successFile "testcases/test44.tig"
    , successFile "testcases/test45.tig"
    , successFile "testcases/test46.tig"
    , successFile "testcases/test47.tig"
    , successFile "testcases/test48.tig"
    , failureFile "testcases/test49.tig"
    , successFile "testcases/test50.tig"
    , successFile "testcases/test51.tig"
    , successFile "testcases/test52.tig"
    , successFile "testcases/test53.tig"
    , successFile "testcases/test54.tig"
    , successFile "testcases/test55.tig"
    , successFile "testcases/test56.tig"
    , successFile "testcases/test57.tig"
    ]

success :: Text -> EqExpr -> Assertion
success src exp = case parse "test.tig" src of
    Left err  -> assertFailure $  "unexpected parsing error `"
                               ++ Text.unpack src
                               ++ "`:\n"
                               ++ Text.unpack err
    Right act -> assertEqual ("when parsing `" ++ Text.unpack src ++ "`") exp (EqExpr act)

failure :: Text -> Assertion
failure src = case parse "test.tig" src of
    Left _     -> pure ()
    Right expr -> assertFailure $ "unexpected success when parsing `"
                                ++ Text.unpack src
                                ++ "`\n"
                                ++ show (EqExpr expr)

successFile :: FilePath -> Assertion
successFile path = do
    src <- Text.readFile path
    case parse path src of
        Left err -> assertFailure $  "unexpected parsing error `"
                                  ++ path
                                  ++ "`:\n"
                                  ++ Text.unpack err
        Right _  -> pure ()

failureFile :: FilePath -> Assertion
failureFile path = do
    src <- Text.readFile path
    case parse path src of
        Left _     -> pure ()
        Right expr -> assertFailure $  "unexpected success when parsing `"
                                    ++ path
                                    ++ "`:\n"
                                    ++ show (EqExpr expr)
