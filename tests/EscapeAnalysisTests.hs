module EscapeAnalysisTests (tests) where

import           Data.Text            (Text)
import           Prelude              hiding (div, exp, rem, seq)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Common
import           Tiger.EscapeAnalysis (escapeAnalyze, getEscapeAnalysisResult)

import qualified Data.Text            as Text

tests :: TestTree
tests = testGroup "Escape analysis tests"
    [ test "variable escapes in one level nesting"
        [ "let"
        , "  var x := 12"
        , "  var y := 13"
        , "  function f(): int = - y"
        , "in f()"
        , "end"
        ] $
        let' [ varDec "x" Nothing rem $ int 12
             , varDec "y" Nothing esc $ int 13
             , funDecs [ funDec "f" [] (Just "int") $ neg (lval $ var "y") ]
             ]
             (call "f" [])
    , test "function parameter escapes"
        [ "let"
        , "  function f(x: int, y: int): int ="
        , "    let"
        , "      function g(): int = x * 20"
        , "    in g() + y"
        , "    end"
        , "in f(12, 13)"
        , "end"
        ] $
        let' [ funDecs [ funDec "f" [ decField "x" esc "int"
                                    , decField "y" rem "int"
                                    ]
                                    (Just "int") $
                                    let' [ funDecs [ funDec "g" [] (Just "int") $
                                                     mul (lval $ var "x") (int 20)
                                                   ]
                                         ]
                                         (add (call "g" []) (lval $ var "y"))
                       ]
             ]
             (call "f" [int 12, int 13])
    , test "for loop variable escapes"
        [ "for i := 12 to 15 do"
        , "  for j := 1 to i do"
        , "    let"
        , "      function f(j: int): int = 15 / j * i"
        , "    in f(j)"
        , "    end"
        ] $
        for "i" esc (int 12) (int 15) $
            for "j" rem (int 1) (lval $ var "i") $
                let' [ funDecs [ funDec "f" [ decField "j" rem "int" ] (Just "int") $
                                                 mul (div (int 15) (lval $ var "j"))
                                                     (lval $ var "i")
                               ]
                     ]
                     (call "f" [lval $ var "j"])
    , test "record variable escapes in one level nesting"
        [ "let"
        , "  type rec = { x: int, y: string }"
        , "  var x := 12"
        , "  function f(): rec = rec { x = x, y = \"hello\" }"
        , "in f()"
        , "end"
        ] $
        let' [ typeDecs [ recordType "rec" [ recField "x" "int"
                                           , recField "y" "string"
                                           ]
                        ]
             , varDec "x" Nothing esc (int 12)
             , funDecs [ funDec "f" [] (Just "rec") $
                             record "rec" [ field "x" (lval $ var "x")
                                          , field "y" (str "hello")
                                          ]
                       ]
             ]
             (call "f" [])
    , test "variable escapes when used for array creation"
        [ "let"
        , "  type intarr = array of int"
        , "  function f(x: int, y: int): intarr ="
        , "    let"
        , "      function g(): intarr = intarr [ x ] of y"
        , "    in g()"
        , "    end"
        , "in f(12, 13)"
        , "end"
        ] $
        let' [ typeDecs [ arrayType "intarr" "int" ]
             , funDecs [ funDec "f" [ decField "x" esc "int"
                                    , decField "y" esc "int"
                                    ]
                                    (Just "intarr") $
                                    let' [ funDecs [ funDec "g" [] (Just "intarr") $
                                                         array "intarr"
                                                               (lval $ var "x")
                                                               (lval $ var "y")
                                                   ]
                                         ]
                                         (call "g" [])
                       ]
             ]
             (call "f" [int 12, int 13])
    , test "for loop escaping variable assignment"
        [ "for i := 1 to 10 do"
        , "  let"
        , "    function f(x: int) = i := x"
        , "  in f(12)"
        , "  end"
        ] $
        for "i" esc (int 1) (int 10) $
            let' [ funDecs [ funDec "f" [ decField "x" rem "int" ] Nothing $
                                 assign (var "i") (lval $ var "x")
                           ]
                 ]
                 (call "f" [int 12])
    , test "complex lval variable escapes"
        [ "let"
        , "  type intarr = array of int"
        , "  type rec = { x: intarr }"
        , "  var r: rec := rec { x = intarr [ 123 ] of 12 }"
        , "  function f() = r.x[11] := 0"
        , "in f()"
        , "end"
        ] $
        let' [ typeDecs [ arrayType "intarr" "int"
                        , recordType "rec" [ recField "x" "intarr" ]
                        ]
             , varDec "r" (Just "rec") esc (record "rec" [ field "x" $ array "intarr"
                                                                             (int 123)
                                                                             (int 12)
                                                         ]
                                           )
             , funDecs [ funDec "f" [] Nothing $
                             assign (index (dot (var "r") "x") (int 11)) (int 0)
                       ]
             ]
             (call "f" [])
    , test "variable escapes when used for indexing array"
        [ "let"
        , "  function f(x: int): string ="
        , "    let"
        , "      type strarr = array of string"
        , "      function g(): string ="
        , "        let"
        , "          var arr := strarr [ 11 ] of \"hello\""
        , "        in arr[x]"
        , "        end"
        , "    in g()"
        , "    end"
        , "in f(2)"
        , "end"
        ] $
        let' [ funDecs [ funDec "f" [ decField "x" esc "int" ] (Just "string") $
                             let' [ typeDecs [ arrayType "strarr" "string" ]
                                  , funDecs [ funDec "g" [] (Just "string") $
                                                 let' [ varDec "arr" Nothing rem $
                                                             array "strarr"
                                                                   (int 11)
                                                                   (str "hello")
                                                      ]
                                                      (lval $ index (var "arr")
                                                                    (lval $ var "x")
                                                      )
                                            ]
                                  ]
                                  (call "g" [])
                       ]
             ]
             (call "f" [int 2])
    , test "variable escapes when used after then"
        [ "let"
        , "  var x := 12"
        , "  var y := 13"
        , "  function f() ="
        , "    if 11 <> 1"
        , "       then x := 13"
        , "in f(); x"
        , "end"
        ] $
        let' [ varDec "x" Nothing esc (int 12)
             , varDec "y" Nothing rem (int 13)
             , funDecs [ funDec "f" [] Nothing $
                             if' [ ne (int 11) (int 1)
                                 , assign (var "x") (int 13)
                                 ]
                       ]
             ]
             (seq [ call "f" [], lval $ var "x" ])
    , test "variable escapes when used after else"
        [ "let"
        , "  var x := 12"
        , "  var y := 13"
        , "  function f(z: int): int ="
        , "    if z = 1"
        , "       then x"
        , "       else y"
        , "in f(2)"
        , "end"
        ] $
        let' [ varDec "x" Nothing esc (int 12)
             , varDec "y" Nothing esc (int 13)
             , funDecs [ funDec "f" [ decField "z" rem "int" ] (Just "int") $
                             if' [ eq (lval $ var "z") (int 1)
                                 , lval $ var "x"
                                 , lval $ var "y"
                                 ]
                       ]
             ]
             (call "f" [int 2])
    , test "variables escape when used in while loop"
        [ "let"
        , "  function f(x: int, y: int): int ="
        , "    let"
        , "      function g() ="
        , "        while x < y do x := x + 2"
        , "    in g(); x"
        , "    end"
        , "in f(12, 34)"
        , "end"
        ] $
        let' [ funDecs [ funDec "f" [ decField "x" esc "int"
                                    , decField "y" esc "int"
                                    ]
                                    (Just "int") $
                                    let' [ funDecs [ funDec "g" [] Nothing $
                                                         while (lt (lval $ var "x")
                                                                   (lval $ var "y")
                                                               )
                                                               (assign (var "x") $
                                                                   add (lval $ var "x")
                                                                       (int 2)
                                                               )
                                                   ]
                                         ]
                                         (seq [ call "g" [], lval $ var "x" ])
                       ]
             ]
             (call "f" [int 12, int 34])
    , test "variables escape when used in for loop"
        [ "let"
        , "  var x := 12"
        , "  var y := 13"
        , "  var z := 14"
        , "  function f(): int ="
        , "    (for i := x + 1 to y do z := z + 3; z)"
        , "in f()"
        , "end"
        ] $
        let' [ varDec "x" Nothing esc (int 12)
             , varDec "y" Nothing esc (int 13)
             , varDec "z" Nothing esc (int 14)
             , funDecs [ funDec "f" [] (Just "int") $
                             seq [ for "i" rem (add (lval $ var "x") (int 1))
                                               (lval $ var "y")
                                               (assign (var "z") (add (lval $ var "z")
                                                                      (int 3)
                                                                 )
                                               )
                                 , lval $ var "z"
                                 ]
                       ]
             ]
             (call "f" [])
    , test "variables escape when used in expressions sequence"
        [ "let"
        , "  var x := 12"
        , "  var y := 13"
        , "  function f(): int = (12; x; y)"
        , "in f()"
        , "end"
        ] $
        let' [ varDec "x" Nothing esc (int 12)
             , varDec "y" Nothing esc (int 13)
             , funDecs [ funDec "f" [] (Just "int") $
                             seq [ int 12
                                 , lval $ var "x"
                                 , lval $ var "y"
                                 ]
                       ]
             ]
             (call "f" [])
    , test "variables escape when used in for loop's nested function"
        [ "let"
        , "  function f(x: int, y: int): int = x + 5 + y"
        , "in for i := 1 to 5 do"
        , "     let"
        , "       var y := i + 3"
        , "       function g(): int = f(i, y)"
        , "     in g()"
        , "     end"
        , "end"
        ] $
        let' [ funDecs [ funDec "f" [ decField "x" rem "int"
                                    , decField "y" rem "int"
                                    ]
                                    (Just "int")
                                    (add (add (lval $ var "x") (int 5))
                                         (lval $ var "y")
                                    )
                       ]
             ] $
             for "i" esc (int 1) (int 5) $
                 let' [ varDec "y" Nothing esc (add (lval $ var "i") (int 3))
                      , funDecs [ funDec "g" [] (Just "int") $
                                     call "f" [ lval $ var "i"
                                              , lval $ var "y"
                                              ]
                                ]
                      ]
                      (call "g" [])
    , test "variable not escape if its name was shadowed 1"
        [ "let"
        , "  var x := 12"
        , "  var y := 13"
        , "  function f(y: int): int ="
        , "    (let"
        , "       var x := 11"
        , "     in x"
        , "     end; y + x)"
        , "in f(5)"
        , "end"
        ] $
        let' [ varDec "x" Nothing esc (int 12)
             , varDec "y" Nothing rem (int 13)
             , funDecs [ funDec "f" [ decField "y" rem "int" ] (Just "int") $
                             seq [ let' [ varDec "x" Nothing rem (int 11)
                                        ]
                                        (lval $ var "x")
                                 , add (lval $ var "y") (lval $ var "x")
                                 ]
                       ]
             ]
             (call "f" [int 5])
    , test "variable not escape if its name was shadowed 2"
        [ "let"
        , "  var x := 12"
        , "  var y := 13"
        , "  function f(): int ="
        , "    let"
        , "      var x := 12"
        , "    in x"
        , "    end"
        , "  function g(): int = y"
        , "in f() + g()"
        , "end"
        ] $
        let' [ varDec "x" Nothing rem (int 12)
             , varDec "y" Nothing esc (int 13)
             , funDecs [ funDec "f" [] (Just "int") $
                             let' [ varDec "x" Nothing rem (int 12)
                                  ]
                                  (lval $ var "x")
                       , funDec "g" [] (Just "int") (lval $ var "y")
                       ]
             ]
             (add (call "f" []) (call "g" []))
    , test "parameter not escape if its name was shadowed"
        [ "let"
        , "  function f(x: int, y: string): int ="
        , "    let"
        , "      function g(x: int, y: string): int = x"
        , "    in g(x, y)"
        , "    end"
        , "in f(12, \"hello\")"
        , "end"
        ] $
        let' [ funDecs [ funDec "f" [ decField "x" rem "int"
                                    , decField "y" rem "string"
                                    ]
                                    (Just "int") $
                                    let' [ funDecs [ funDec "g"
                                                         [ decField "x" rem "int"
                                                         , decField "y" rem "string"
                                                         ]
                                                         (Just "int")
                                                         (lval $ var "x")
                                                   ]
                                         ]
                                         (call "g" [lval $ var "x", lval $ var "y"])
                       ]
             ]
             (call "f" [int 12, str "hello"])
    , test "variable escapes in second functions block"
        [ "let"
        , "  var x := 12"
        , "  function f(): int = 12"
        , "  var y := \"hello\""
        , "  function g(): int = x"
        , "in g()"
        , "end"
        ] $
        let' [ varDec "x" Nothing esc (int 12)
             , funDecs [ funDec "f" [] (Just "int") (int 12) ]
             , varDec "y" Nothing rem (str "hello")
             , funDecs [ funDec "g" [] (Just "int") (lval $ var "x") ]
             ]
             (call "g" [])
    ]

test :: TestName -> [Text] -> EqExpr -> TestTree
test name srcLines exp = testCase name $ do
    act <- genericParser "test.tig" src
    escAct <- escapeAnalyze act
    assertEqual ("when analyze `" ++ Text.unpack src ++ "`")
                exp
                (EqExpr (getEscapeAnalysisResult escAct))
  where src = Text.unlines srcLines
