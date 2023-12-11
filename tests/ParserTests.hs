module ParserTests (tests) where

import           Data.Text        (Text)
import           Prelude          hiding (and, break, div, exp, or, rem, seq, span)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Common
import           Tiger.Parser     (parse)

import qualified Data.Text        as Text
import qualified Data.Text.IO     as Text

tests :: TestTree
tests = testGroup "Parser tests" [ nilTest
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

nilTest :: TestTree
nilTest = success "parses nil value" "nil" nil

lValueTests :: TestTree
lValueTests = testGroup "parses lvalue" [ varTest
                                        , indexTest
                                        , dotTest
                                        , complexLValueTest
                                        ]
  where varTest = success "parses variable" "test" $ lval (var "test")
        indexTest = success "parses array indexing" "test[123]" $
            lval $ index (var "test") (int 123)
        dotTest = success "parses record field access" "test1.test2" $
            lval $ dot (var "test1") "test2"
        complexLValueTest = success "parses complex lvalue" "t1[123].t2[1][2].t3" $
            lval $ dot (index (index (dot (index (var "t1") (int 123)) "t2") (int 1))
                (int 2)) "t3"

intLitTest :: TestTree
intLitTest = success "parses int value" "123" $ int 123

strLitTests :: TestTree
strLitTests = testGroup "parses string" [ escapeSequenceTest
                                        , controlCharTest
                                        , asciiCodeTest
                                        , multilineStringTest
                                        , wrongAsciiCodeTest
                                        , wrongEscapeSequenceTest
                                        , wrongControlCharacterTest
                                        , withoutQuoteTest
                                        ]
  where escapeSequenceTest = success "parses string with escape sequences"
            "\"Hello\\n\\t\\\"\\\\\"" $ str "Hello\n\t\"\\"
        controlCharTest = success "parses string with control characters"
            "\"Hello \\^@\\^G\\^H\\^I\\^J\\^K\\^L\\^M\\^Z\
            \\\^g\\^h\\^i\\^j\\^k\\^l\\^m\\^z\"" $
                str "Hello \0\a\b\t\n\v\f\r\032\a\b\t\n\v\f\r\032"
        asciiCodeTest = success "parses string with ascii codes"
            "\"\\084\\101\\115\\116\"" $ str "Test"
        multilineStringTest = success "parses multiline string"
            "\"Hello\\\n      \\ World\"" $ str "Hello World"
        wrongAsciiCodeTest = failure "failed to parse string with wrong ascii code"
            "\"\\128\""
        wrongEscapeSequenceTest =
            failure "failed to parse string with wrong escape sequence" "\"\\r\""
        wrongControlCharacterTest =
            failure "failed to parse string with wrong control characters" "\"\\^D\""
        withoutQuoteTest = failure "failed to parse string without closing quote"
            "\"Hello"

operationTests :: TestTree
operationTests = testGroup "parses operations"
    [ success "parses negation of int" "-12" $ neg (int 12)
    , success "parses negation of expression" "-(12 + 45)" $ neg (add (int 12) (int 45))
    , success "parses add and mult with correct priorities" "a + b * c" $
        add (lval $ var "a") (mul (lval $ var "b") (lval $ var "c"))
    , success "parses expression with parens with correct priorities 1" "(a + b) * c" $
        mul (add (lval $ var "a") (lval $ var "b")) (lval $ var "c")
    , success "parses sub and mult with correct priorities" "c * a - 12" $
        sub (mul (lval $ var "c") (lval $ var "a")) (int 12)
    , success "parses expression with parens with correct priorities 2" "c * (a - 12)" $
        mul (lval $ var "c") (sub (lval $ var "a") (int 12))
    , success "parses div with correct priorities" "a[1] / b.s / c" $
        div (div (lval $ index (var "a") (int 1)) (lval $ dot (var "b") "s"))
            (lval $ var "c")
    , success "parses mul and div with correct priorities" "12 / 3 * 4" $
        mul (div (int 12) (int 3)) (int 4)
    , success "parses gt" "a > b" $ gt (lval $ var "a") (lval $ var "b")
    , success "parses two gt with parens" "a > (b > 12)" $
        gt (lval $ var "a") (gt (lval $ var "b") (int 12))
    , success "parses ge" "a >= b" $ ge (lval $ var "a") (lval $ var "b")
    , success "parse two ge with parens" "(a >= b) >= nil" $
        ge (ge (lval $ var "a") (lval $ var "b")) nil
    , success "parses lt" "a < b.s[1]" $ lt (lval $ var "a")
                                            (lval $ index (dot (var "b") "s") (int 1))
    , success "parses two lt with parens" "(a < 12) < 13" $
        lt (lt (lval $ var "a") (int 12)) (int 13)
    , success "parses le" "a[1] <= nil" $ le (lval $ index (var "a") (int 1)) nil
    , success "parses two le with brackets" "11 <= (12 <= 13)" $
        le (int 11) (le (int 12) (int 13))
    , success "parses ne" "a <> 2" $ ne (lval $ var "a") (int 2)
    , success "parses ne, lt, gt with correct priorities" "a < b <> b > c" $
        ne (lt (lval $ var "a") (lval $ var "b"))
           (gt (lval $ var "b") (lval $ var "c"))
    , success "parses eq" "a = 2" $ eq (lval $ var "a") (int 2)
    , success "parses lt, eq and le with correct priorities" "a < b = b <= c" $
        eq (lt (lval $ var "a") (lval $ var "b"))
           (le (lval $ var "b") (lval $ var "c"))
    , failure "failed to parse two conditionals without parens 1" "a > b > 12"
    , failure "failed to parse two conditionals without parens 2" "a > b >= 12"
    , failure "failed to parse two conditionals without parens 3" "a <> b <> c"
    , failure "failed to parse two conditionals without parens 4" "a = b = c"
    , failure "failed to parse two conditionals without parens 5" "a = b <> c"
    , failure "failed to parse two conditionals without parens 6" "a <> b = c"
    , success "parses condition with and and or 1" "a = 2 & b <> 3 | c <= d" $
        or (and (eq (lval $ var "a") (int 2)) (ne (lval $ var "b") (int 3)))
           (le (lval $ var "c") (lval $ var "d"))
    , success "parses condition with and and or 2" "a = 2 & (b <> 3 | c <= d)" $
        and (eq (lval $ var "a") (int 2))
            (or (ne (lval $ var "b") (int 3)) (le (lval $ var "c") (lval $ var "d")))
    ]

recordTests :: TestTree
recordTests = testGroup "record creation tests"
    [ success "parses record creation without fields" "a := kek {}" $
        assign (var "a") (record "kek" [])
    , success "parses record creation with fields"
        "a := kek { lol = 123, rofl = \"Hello\" }" $
            assign (var "a") (record "kek" [ field "lol" (int 123)
                                           , field "rofl" (str "Hello")
                                           ])
    ]

arrayTest :: TestTree
arrayTest = success "array creation test" "a := kek [b] of c.s" $
    assign (var "a") (array "kek" (lval $ var "b") (lval $ dot (var "c") "s"))

assignTest :: TestTree
assignTest = success "assignment test" "a := b[123].c" $
    assign (var "a") (lval $ dot (index (var "b") (int 123)) "c")

ifTests :: TestTree
ifTests = testGroup "if expression tests"
    [ success "parses if without else" "if a <> b then print(\"hello\")" $
        if' [ ne (lval $ var "a") (lval $ var "b")
            , call "print" [str "hello"]
            ]
    , success "parses if with else"
        "if a = b then print(\"hello\") else print(\"world\")" $
            if' [ eq (lval $ var "a") (lval $ var "b")
                , call "print" [str "hello"]
                , call "print" [str "world"]
                ]
    , success "parses if that returns value"
        "print(if a = 1 then \"one\" else if a = 2 then \"two\" else \"three\")" $
            call "print" [ if' [ eq (lval $ var "a") (int 1)
                               , str "one"
                               , if' [ eq (lval $ var "a") (int 2)
                                     , str "two"
                                     , str "three"
                                     ]
                               ]
                         ]
    ]

whileTest :: TestTree
whileTest = success "while expression test"
    "while a <= b * 12 do\n (print(a); a := a + 1)" $
        while (le (lval $ var "a") (mul (lval $ var "b") (int 12))) $
            seq [ call "print" [ lval $ var "a" ]
                , assign (var "a") (add (lval $ var "a") (int 1))
                ]

forTest :: TestTree
forTest = success "for expression test" "for i := 1 to a do\n b := b * i" $
    for "i" rem (int 1) (lval $ var "a") $
        assign (var "b") (mul (lval $ var "b") (lval $ var "i"))

seqTests :: TestTree
seqTests = testGroup "expression sequence tests"
    [ success "parses expression without semicolon as binop" "(12 + 45)" $
        add (int 12) (int 45)
    , success "parses expression with simicolon as sequence"
        "(print(toStr(a)); a := a / 5)" $
            seq [ call "print" [ call "toStr" [ lval $ var "a" ] ]
                , assign (var "a") (div (lval $ var "a") (int 5))
                ]
    ]

callTests :: TestTree
callTests = testGroup "function call tests"
    [ success "parses call without parameters" "kek()" $ call "kek" []
    , success "parses call with parameters" "kek(123, b.c[1])" $
        call "kek" [ int 123
                   , lval $ index (dot (var "b") "c") (int 1)
                   ]
    ]

breakTest :: TestTree
breakTest = success "break expression test"
    "for i := 0 to 10 do\n(if i >= 9 then break; print(i))" $
        for "i" rem (int 0) (int 10) $
            seq $ [ if' [ ge (lval $ var "i") (int 9)
                        , break
                        ]
                  , call "print" [lval $ var "i"]
                  ]

letTests :: TestTree
letTests = testGroup "let expression tests"
    [ success "parses let with one variable"
        "let\n  type kek = string\n  var i: kek := \"hello\"\nin i\nend" $
            let' [ typeDecs [typeAlias "kek" "string"]
                 , varDec "i" (Just "kek") rem $ str "hello"
                 ]
                 (lval $ var "i")
    , success "parses let with type and variable definitions"
        "let\n  type lol = array of int\n  var kek := lol [123] of b.c\n\
        \in kek\nend" $
            let' [ typeDecs [arrayType "lol" "int"]
                 , varDec "kek" Nothing rem $ array "lol" (int 123)
                    (lval $ dot (var "b") "c")
                 ]
                 (lval $ var "kek")
    , success "parses let with record cration"
        "let\n  type kek = string\n  type lol = { rofl: kek }\n\
        \  var i := lol { rofl = \"hello\\n\" }\nin i\nend" $
            let' [ typeDecs [ typeAlias "kek" "string"
                            , recordType "lol" [recField "rofl" "kek"]
                            ]
                 , varDec "i" Nothing rem $ record "lol" [field "rofl" $ str "hello\n"]
                 ]
                 (lval $ var "i")
    , success "parses let with function definition without parameters"
        "let\n  function hello() = print(\"hello\")\nin hello()\nend" $
            let' [ funDecs [funDec "hello" [] Nothing $ call "print" [str "hello"]]
                 ]
                 (call "hello" [])
    , success "parses let with function definition with parameters"
        "let\n  function add5(a: int): int = a + 5\nin add5(6)\nend" $
            let' [ funDecs [funDec "add5" [decField "a" rem "int"] (Just "int") $
                            add (lval $ var "a") (int 5)
                           ]
                 ]
                 (call "add5" [int 6])
    , success "parses let with functions block"
              "let\n  function ping(i: int, lim: int) = if i < lim \
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

commentTests :: TestTree
commentTests = testGroup "comment tests"
    [ success "parses line comment"
        "/* comment */\nlet\n  var i: int := 123\nin i\nend" $
            let' [varDec "i" (Just "int") rem $ int 123] (lval $ var "i")
    , success "parses comment in middle of expression"
        "let\n  /* counter */\n  var i := 123\nin i\nend" $
            let' [varDec "i" Nothing rem $ int 123] (lval $ var "i")
    , failure "failed to parse not closed comment" "let\n/*  var i := 1\nin i\nend"
    ]

fileCases :: TestTree
fileCases = testGroup "parses files"
    [ successFile "parses array creation" "testcases/test1.tig"
    , successFile "parses array creation of type alias" "testcases/test2.tig"
    , successFile "parses record creation" "testcases/test3.tig"
    , successFile "parses recursive function" "testcases/test4.tig"
    , successFile "parses recursive types" "testcases/test5.tig"
    , successFile "parses mutually recursive procedures" "testcases/test6.tig"
    , successFile "parses mutually recursive functions" "testcases/test7.tig"
    , successFile "parses correct if" "testcases/test8.tig"
    , successFile "parses if with wrong types" "testcases/test9.tig"
    , successFile "parses while with non-unit body" "testcases/test10.tig"
    , successFile "parses for with wrong types" "testcases/test11.tig"
    , successFile "parses for and let" "testcases/test12.tig"
    , successFile "parses comparison with wrong types 1" "testcases/test13.tig"
    , successFile "parses comparison with wrong types 2" "testcases/test14.tig"
    , successFile "parses if without else" "testcases/test15.tig"
    , successFile "parses cycle types definitions" "testcases/test16.tig"
    , successFile "parses wrong mutually recursive types definitions"
        "testcases/test17.tig"
    , successFile "parses wrong mutually recursive functions definitions"
        "testcases/test18.tig"
    , successFile "parses function with undefined variable" "testcases/test19.tig"
    , successFile "parses while with undefined variable" "testcases/test20.tig"
    , successFile "parses recursive function" "testcases/test21.tig"
    , successFile "parses record creation with wrong field" "testcases/test22.tig"
    , successFile "parses record field assignment" "testcases/test23.tig"
    , successFile "parses int variable indexing" "testcases/test24.tig"
    , successFile "parses int variable dot" "testcases/test25.tig"
    , successFile "parses int and string addition" "testcases/test26.tig"
    , successFile "parses function with parameter name shadowing" "testcases/test27.tig"
    , successFile "parses different record types assignment" "testcases/test28.tig"
    , successFile "parses different array types assignment" "testcases/test29.tig"
    , successFile "parses synonym array assignment" "testcases/test30.tig"
    , successFile "parses initialization of integer variable with string"
        "testcases/test31.tig"
    , successFile "parses initialization of integer array with string"
        "testcases/test32.tig"
    , successFile "parses creation of unknown record" "testcases/test33.tig"
    , successFile "parses function call with wrong parameters types"
        "testcases/test34.tig"
    , successFile "parses function call with less parameters" "testcases/test35.tig"
    , successFile "parses function call with more parameters" "testcases/test36.tig"
    , successFile "parses variable redeclaration" "testcases/test37.tig"
    , successFile "parses type redeclaration" "testcases/test38.tig"
    , successFile "parses function redeclaration in the same block" "testcases/test39.tig"
    , successFile "parses procedure that returns value" "testcases/test40.tig"
    , successFile "parses local type redeclaration" "testcases/test41.tig"
    , successFile "parses program with all supported definitions" "testcases/test42.tig"
    , successFile "parses unit type assignment" "testcases/test43.tig"
    , successFile "parses nil record initialization and assignment" "testcases/test44.tig"
    , successFile "parses nil initialization without type annotation"
        "testcases/test45.tig"
    , successFile "parses record with nil comparison" "testcases/test46.tig"
    , successFile "parses type redeclaration in different blocks" "testcases/test47.tig"
    , successFile "parses function redeclaration in different blocks"
        "testcases/test48.tig"
    , failureFile "failed to parse wrong record initialization" "testcases/test49.tig"
    , successFile "parses record type with duplicated field" "testcases/test50.tig"
    , successFile "parses record creation with duplicated field initialization"
        "testcases/test51.tig"
    , successFile "parses record creation with unitialized field" "testcases/test52.tig"
    , successFile "parses array creation from record type" "testcases/test53.tig"
    , successFile "parses break outside of loop" "testcases/test54.tig"
    , successFile "parses break inside of loop" "testcases/test55.tig"
    , successFile "parses comparison of different array types" "testcases/test56.tig"
    , successFile "parses comparison of same array types" "testcases/test57.tig"
    , successFile "parses program with merge sort implementation" "testcases/merge.tig"
    , successFile "parses program with 8-queens problem solution" "testcases/queens.tig"
    , successFile "parses program with atoi and itoa implementation" "testcases/atoi.tig"
    ]

success :: TestName -> Text -> EqExpr -> TestTree
success name src exp = testCase name $ do
    case parse "test.tig" src of
        Left err  -> assertFailure $  "unexpected parsing error `"
                                   ++ Text.unpack src
                                   ++ "`:\n"
                                   ++ Text.unpack err
        Right act -> assertEqual ("when parsing `" ++ Text.unpack src ++ "`")
                                 exp
                                 (EqExpr act)

failure :: TestName -> Text -> TestTree
failure name src = testCase name $ do
    case parse "test.tig" src of
        Left _     -> pure ()
        Right expr -> assertFailure $ "unexpected success when parsing `"
                                    ++ Text.unpack src
                                    ++ "`\n"
                                    ++ show (EqExpr expr)

successFile :: TestName -> FilePath -> TestTree
successFile name path = testCase name $ do
    src <- Text.readFile path
    case parse path src of
        Left err -> assertFailure $  "unexpected parsing error `"
                                  ++ path
                                  ++ "`:\n"
                                  ++ Text.unpack err
        Right _  -> pure ()

failureFile :: TestName -> FilePath -> TestTree
failureFile name path = testCase name $ do
    src <- Text.readFile path
    case parse path src of
        Left _     -> pure ()
        Right expr -> assertFailure $  "unexpected success when parsing `"
                                    ++ path
                                    ++ "`:\n"
                                    ++ show (EqExpr expr)
