module BackendTestCases
    ( TestCase(..)
    , Input(..)
    , testCases
    , readLineSrc
    , atoiSrc
    , itoaSrc
    , unwordsSrc
    , test
    ) where

import           Data.Text      (Text)
import           Prelude        hiding (lines)
import           Test.Tasty     (TestName)

import qualified Data.Text      as Text
import qualified Data.Text.Lazy as LazyText

data TestCase = TestCase
    { testName   :: !TestName
    , testSrc    :: !Text
    , testInputs :: ![Input]
    }

data Input = Input
    { inputStdin    :: !Text
    , inputStdout   :: !LazyText.Text
    , inputExitCode :: !Int
    }

testCases :: [TestCase]
testCases =
    [ test "simple binary expression"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var x := atoi(readLine())"
        , "  var y := atoi(readLine())"
        , "  var z := (x + y) * (x - y)"
        , "in"
        , "  print(itoa(z))"
        , "end"
        ]
        [ ("30\n20", "500", 0)
        , ("20\n30", "-500", 0)
        , ("-20\n-11", "279", 0)
        , ("-11\n-34", "-1035", 0)
        ]
    , test "binary expression with boolean operators"
        [ "let"
        , readLineSrc
        , atoiSrc
        , "  var x := atoi(readLine())"
        , "  var y := atoi(readLine())"
        , "  var z := atoi(readLine())"
        , "  var w := atoi(readLine())"
        , "in print(chr(((x | y) & (z | w)) + 48))"
        , "end"
        ]
        [ ("0\n0\n0\n0", "0", 0)
        , ("0\n0\n2\n0", "0", 0)
        , ("0\n0\n0\n4", "0", 0)
        , ("0\n0\n2\n4", "0", 0)
        , ("23\n0\n0\n0", "0", 0)
        , ("0\n24\n0\n0", "0", 0)
        , ("23\n24\n0\n0", "0", 0)
        , ("90\n0\n11\n0", "1", 0)
        , ("90\n0\n0\n11", "1", 0)
        , ("0\n80\n13\n0", "1", 0)
        , ("0\n80\n0\n-11", "1", 0)
        , ("90\n80\n-11\n11", "1", 0)
        ]
    , test "complex binary expression with boolean operators"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var x := atoi(readLine())"
        , "  var y := atoi(readLine())"
        , "  var z := atoi(readLine())"
        , "  var res := (((x <= y & (y > z | y = 7)) >= 1) + 9) * 5"
        , "in print(itoa(res))"
        , "end"
        ]
        [ ("-20\n-30\n-25", "45", 0)
        , ("30\n20\n-10", "45", 0)
        , ("30\n7\n8", "45", 0)
        , ("0\n1\n1", "45", 0)
        , ("1\n1\n1", "45", 0)
        , ("0\n1\n0", "50", 0)
        , ("1\n1\n0", "50", 0)
        , ("6\n7\n10", "50", 0)
        , ("7\n7\n10", "50", 0)
        ]
    , test "simple if assignment"
        [ "let"
        , readLineSrc
        , atoiSrc
        , "  var x := atoi(readLine())"
        , "  var y := atoi(readLine())"
        , "  var z := if x > y then 3 else 4"
        , "in print(chr(z + 48));"
        , "exit(20)"
        , "end"
        ]
        [ ("30\n20", "3", 20)
        , ("20\n30", "4", 20)
        , ("-30\n20", "4", 20)
        , ("20\n-30", "3", 20)
        , ("-30\n-20", "4", 20)
        , ("-20\n-30", "3", 20)
        ]
    , test "complex if assignment"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var a := if atoi(readLine()) < 10"
        , "           then if atoi(readLine()) > 20"
        , "                then 10"
        , "                else if atoi(readLine()) * 2 < 11"
        , "                     then 11"
        , "                     else 12"
        , "           else 13"
        , "in"
        , "  print(itoa(a))"
        , "end"
        ]
        [ ("12", "13", 0)
        , ("9\n21", "10", 0)
        , ("8\n19\n2", "11", 0)
        , ("8\n19\n6", "12", 0)
        ]
    , test "nested ifs without else"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var a := atoi(readLine())"
        , "  var b := atoi(readLine())"
        , "  var c := atoi(readLine())"
        , "in"
        , "  if a < b"
        , "  then if b < c"
        , "       then if c > a"
        , "            then print(itoa(a + b * c))"
        , "end"
        ]
        [ ("12\n13\n14", "194", 0)
        , ("12\n13\n12", "", 0)
        , ("15\n11\n20", "", 0)
        ]
    , test "nested ifs with else 1"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var a := atoi(readLine())"
        , "  var b := atoi(readLine())"
        , "  var c := atoi(readLine())"
        , "in"
        , "  if a < b"
        , "  then if b < c"
        , "       then (if c > a"
        , "             then print(itoa(a + b * c)))"
        , "       else print(itoa(c * a - b))"
        , "end"
        ]
        [ ("12\n13\n14", "194", 0)
        , ("12\n13\n11", "119", 0)
        , ("15\n11\n20", "", 0)
        ]
    , test "nested ifs with else 2"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var a := atoi(readLine())"
        , "  var b := atoi(readLine())"
        , "  var c := atoi(readLine())"
        , "in"
        , "  if a < b"
        , "  then if b < c"
        , "       then (if c > a"
        , "             then print(itoa(a + b * c)))"
        , "       else print(itoa(c * a - b))"
        , "  else print(itoa(a))"
        , "end"
        ]
        [ ("12\n13\n14", "194", 0)
        , ("12\n13\n11", "119", 0)
        , ("15\n11\n20", "15", 0)
        ]
    , test "while loop"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var min := atoi(readLine())"
        , "  var max := atoi(readLine())"
        , "  var num := 0"
        , "  var prod := 1"
        , "in"
        , "  while (num := atoi(readLine()); min <= num & num <= max) do"
        , "    prod := prod * num;"
        , "  print(itoa(prod))"
        , "end"
        ]
        [ ("5\n10\n6\n5\n5\n7\n10\n-1", "10500", 0)
        , ("4\n6\n4\n4\n5\n3", "80", 0)
        , ("7\n8\n7\n8\n7\n7\n8\n9", "21952", 0)
        ]
    , test "while loop with complex nested ifs 1"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var a := atoi(readLine())"
        , "  var b := atoi(readLine())"
        , "  var c := atoi(readLine())"
        , "in"
        , "  while a < c do ("
        , "    if a < b"
        , "    then if b < c"
        , "         then if c > a"
        , "              then print(concat(itoa(a + b * c), \"\\n\"));"
        , "    a := a + 1"
        , "  )"
        , "end"
        ]
        [ ("12\n15\n18", "282\n283\n284\n", 0)
        , ("12\n13\n12", "", 0)
        , ("15\n11\n20", "", 0)
        , ("10\n9\n9", "", 0)
        ]
    , test "while loop with complex nested ifs 2"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var a := atoi(readLine())"
        , "  var b := atoi(readLine())"
        , "  var c := atoi(readLine())"
        , "in"
        , "  while a < c do ("
        , "    if a < b"
        , "    then if b < c"
        , "         then if c > a"
        , "              then (a := a * 2; break);"
        , "    a := a + 1"
        , "  );"
        , "  print(itoa(a))"
        , "end"
        ]
        [ ("12\n15\n18", "24", 0)
        , ("12\n13\n12", "12", 0)
        , ("15\n11\n20", "20", 0)
        , ("10\n9\n9", "10", 0)
        ]
    , test "while loop with complex nested ifs 3"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var a := atoi(readLine())"
        , "  var b := atoi(readLine())"
        , "  var c := atoi(readLine())"
        , "in"
        , "  while a < c do ("
        , "    if a < b"
        , "    then if b < c"
        , "         then (if c > a"
        , "               then (print(itoa(a + b * c))); break)"
        , "         else (print(itoa(c * a - b)); break);"
        , "    a := a + 1"
        , "  );"
        , "  print(concat(\"\\n\", itoa(a)))"
        , "end"
        ]
        [ ("12\n20\n14", "148\n12", 0)
        , ("10\n12\n15", "190\n10", 0)
        , ("10\n9\n15", "\n15", 0)
        , ("10\n9\n9", "\n10", 0)
        ]
    , test "while loop with complex nested ifs 4"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var a := atoi(readLine())"
        , "  var b := atoi(readLine())"
        , "  var c := atoi(readLine())"
        , "in"
        , "  while a < c do ("
        , "    if a < b"
        , "    then if b < c"
        , "         then (if c > a"
        , "               then (print(itoa(a + b * c))); break)"
        , "         else (print(itoa(c * a - b)); break)"
        , "    else print(itoa(b));"
        , "    a := a + 1"
        , "  );"
        , "  print(concat(\"\\n\", itoa(a)))"
        , "end"
        ]
        [ ("12\n20\n14", "148\n12", 0)
        , ("10\n12\n15", "190\n10", 0)
        , ("10\n9\n15", "99999\n15", 0)
        , ("10\n9\n9", "\n10", 0)
        ]
    , test "while loop with escaping variable"
        [ "let"
        , readLineSrc
        , itoaSrc
        , "  var i := 0"
        , "in"
        , "  while i <= 10 do"
        , "    let"
        , "      function step() ="
        , "        let"
        , "          var cmd := readLine()"
        , "        in"
        , "          if cmd = \"one\""
        , "          then i := i + 1"
        , "          else if cmd = \"two\""
        , "               then i := i + 2"
        , "               else if cmd = \"three\""
        , "                    then i := i + 3"
        , "        end"
        , "    in"
        , "      print(concat(itoa(i), \"\\n\"));"
        , "      step();"
        , "      i := i + 1"
        , "    end"
        , "end"
        ]
        [ ("one\none\none\none\none", "0\n2\n4\n6\n8\n10\n", 0)
        , ("three\none\ntwo\ntwo\none", "0\n4\n6\n9\n", 0)
        , ("three\nthree\nthree\nthree\n", "0\n4\n8\n", 0)
        , ("two\ntwo\none\none\n", "0\n3\n6\n8\n10\n", 0)
        ]
    , test "for loop with complex nested ifs 1"
        [ "let"
           , readLineSrc
           , atoiSrc
           , itoaSrc
           , "  var a := atoi(readLine())"
           , "  var b := atoi(readLine())"
           , "  var c := atoi(readLine())"
           , "in"
           , "  for i := a to c do ("
           , "    if i < b"
           , "    then if b < c"
           , "         then if c > 1"
           , "              then print(concat(itoa(i + b * c), \"\\n\"));"
           , "    a := a + 2"
           , "  )"
           , "end"
           ]
           [ ("12\n15\n18", "282\n283\n284\n", 0)
           , ("12\n13\n12", "", 0)
           , ("15\n11\n20", "", 0)
           , ("10\n9\n9", "", 0)
           ]
    , test "for loop with complex nested ifs 2"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var a := atoi(readLine())"
        , "  var b := atoi(readLine())"
        , "  var c := atoi(readLine())"
        , "in"
        , "  for i := a to c do ("
        , "    if a < b"
        , "    then if b < c"
        , "         then if c > a"
        , "              then (a := a * 2; break);"
        , "    a := a + 1"
        , "  );"
        , "  print(itoa(a))"
        , "end"
        ]
        [ ("12\n15\n18", "24", 0)
        , ("12\n13\n11", "12", 0)
        , ("15\n11\n20", "21", 0)
        , ("10\n9\n9", "10", 0)
        ]
    , test "for loop with complex nested ifs 3"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var a := atoi(readLine())"
        , "  var b := atoi(readLine())"
        , "  var c := atoi(readLine())"
        , "in"
        , "  for i := a to c do ("
        , "    if i < b"
        , "    then if b < c"
        , "         then (if c > i"
        , "               then (print(itoa(i + b * c))); break)"
        , "         else (print(itoa(c * i - b)); break);"
        , "    a := a + 1"
        , "  );"
        , "  print(concat(\"\\n\", itoa(a)))"
        , "end"
        ]
        [ ("12\n20\n14", "148\n12", 0)
        , ("10\n12\n15", "190\n10", 0)
        , ("10\n9\n15", "\n16", 0)
        , ("10\n9\n9", "\n10", 0)
        ]
    , test "for loop with complex nested ifs 4"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var a := atoi(readLine())"
        , "  var b := atoi(readLine())"
        , "  var c := atoi(readLine())"
        , "in"
        , "  for i := a to c do ("
        , "    if i < b"
        , "    then if b < c"
        , "         then (if c > i"
        , "               then (print(itoa(i + b * c))); break)"
        , "         else (print(itoa(c * i - b)); break)"
        , "    else print(itoa(b));"
        , "    a := a + 1"
        , "  );"
        , "  print(concat(\"\\n\", itoa(a)))"
        , "end"
        ]
        [ ("12\n20\n14", "148\n12", 0)
        , ("10\n12\n15", "190\n10", 0)
        , ("10\n9\n15", "999999\n16", 0)
        , ("10\n9\n9", "\n10", 0)
        ]
    , test "for loop with escaping variable"
        [ "let"
        , readLineSrc
        , itoaSrc
        , "in"
        , "  for i := 0 to 10 do"
        , "    let"
        , "      function step() ="
        , "        let"
        , "          var cmd := readLine()"
        , "        in"
        , "          if cmd = \"one\""
        , "          then i := i + 1"
        , "          else if cmd = \"two\""
        , "               then i := i + 2"
        , "               else if cmd = \"three\""
        , "                    then i := i + 3"
        , "        end"
        , "    in"
        , "      print(concat(itoa(i), \"\\n\"));"
        , "      step()"
        , "    end"
        , "end"
        ]
        [ ("one\none\none\none\none", "0\n2\n4\n6\n8\n10\n", 0)
        , ("three\none\ntwo\ntwo\none", "0\n4\n6\n9\n", 0)
        , ("three\nthree\nthree\nthree\n", "0\n4\n8\n", 0)
        , ("two\ntwo\none\none\n", "0\n3\n6\n8\n10\n", 0)
        ]
    , test "simple array test"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  type intarray = array of int"
        , "  var arrSize := atoi(readLine())"
        , "  var arr1 := intarray [ arrSize ] of 0"
        , "  var arr2 := intarray [ arrSize ] of 0"
        , "  var sumArr := intarray [ arrSize ] of 0"
        , "  var sum := 0"
        , "in"
        , "  for i := 0 to arrSize - 1 do"
        , "    arr1[i] := atoi(readLine());"
        , "  for i := 0 to arrSize - 1 do"
        , "    arr2[i] := atoi(readLine());"
        , "  for i := 0 to arrSize - 1 do"
        , "    sumArr[i] := arr1[i] + arr2[i];"
        , "  for i := 0 to arrSize - 1 do"
        , "    if i <> arrSize - 1"
        , "    then print(concat(itoa(sumArr[i]), \", \"))"
        , "    else print(itoa(sumArr[i]))"
        , "end"
        ]
        [ ("5\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10", "7, 9, 11, 13, 15", 0)
        , ("2\n-1\n4\n6\n-9", "5, -5", 0)
        ]
    , test "array as parameter test (bubble sort)"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  type intarray = array of int"
        , "  var arrSize := atoi(readLine())"
        , "  var arr := intarray [ arrSize ] of 0"
        , "  function readArray(rarr: intarray, rarrSize: int) ="
        , "    for i := 0 to rarrSize - 1 do"
        , "      rarr[i] := atoi(readLine())"
        , "  function sortArray(sarr: intarray, sarrSize: int) ="
        , "    let"
        , "      var isSorted := 0"
        , "    in"
        , "      while not(isSorted) do ("
        , "        isSorted := 1;"
        , "        for i := 0 to sarrSize - 2 do"
        , "          if sarr[i] > sarr[i + 1]"
        , "          then"
        , "            let"
        , "              var tmp := sarr[i]"
        , "            in"
        , "              sarr[i] := sarr[i + 1];"
        , "              sarr[i + 1] := tmp;"
        , "              isSorted := 0"
        , "            end"
        , "      )"
        , "    end"
        , "  function printArray(parr: intarray, parrSize: int) ="
        , "    for i := 0 to parrSize - 1 do"
        , "      if i <> parrSize - 1"
        , "      then print(concat(itoa(parr[i]), \", \"))"
        , "      else print(itoa(parr[i]))"
        , "in"
        , "  readArray(arr, arrSize);"
        , "  sortArray(arr, arrSize);"
        , "  printArray(arr, arrSize)"
        , "end"
        ]
        [ ("5\n5\n4\n3\n2\n1", "1, 2, 3, 4, 5", 0)
        , ("5\n4\n1\n5\n3\n2", "1, 2, 3, 4, 5", 0)
        , ("5\n3\n1\n2\n5\n4", "1, 2, 3, 4, 5", 0)
        , ("5\n5\n1\n3\n4\n2", "1, 2, 3, 4, 5", 0)
        , ("5\n5\n1\n2\n3\n4", "1, 2, 3, 4, 5", 0)
        , ("5\n5\n3\n2\n4\n1", "1, 2, 3, 4, 5", 0)
        , ("10\n10\n9\n8\n7\n6\n5\n4\n3\n2\n1", "1, 2, 3, 4, 5, 6, 7, 8, 9, 10", 0)
        ]
    , test "array variable escaping (bubble sort)"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  type intarray = array of int"
        , "  var arrSize := atoi(readLine())"
        , "  var arr := intarray [ arrSize ] of 0"
        , "  function readArray() ="
        , "    for i := 0 to arrSize - 1 do"
        , "      arr[i] := atoi(readLine())"
        , "  function sortArray() ="
        , "    let"
        , "      var isSorted := 0"
        , "    in"
        , "      while not(isSorted) do ("
        , "        isSorted := 1;"
        , "        for i := 0 to arrSize - 2 do"
        , "          if arr[i] > arr[i + 1]"
        , "          then"
        , "            let"
        , "              var tmp := arr[i]"
        , "            in"
        , "              arr[i] := arr[i + 1];"
        , "              arr[i + 1] := tmp;"
        , "              isSorted := 0"
        , "            end"
        , "      )"
        , "    end"
        , "  function printArray() ="
        , "    for i := 0 to arrSize - 1 do"
        , "      if i <> arrSize - 1"
        , "      then print(concat(itoa(arr[i]), \", \"))"
        , "      else print(itoa(arr[i]))"
        , "in"
        , "  readArray();"
        , "  sortArray();"
        , "  printArray()"
        , "end"
        ]
        [ ("5\n5\n4\n3\n2\n1", "1, 2, 3, 4, 5", 0)
        , ("5\n4\n1\n5\n3\n2", "1, 2, 3, 4, 5", 0)
        , ("5\n3\n1\n2\n5\n4", "1, 2, 3, 4, 5", 0)
        , ("5\n5\n1\n3\n4\n2", "1, 2, 3, 4, 5", 0)
        , ("5\n5\n1\n2\n3\n4", "1, 2, 3, 4, 5", 0)
        , ("5\n5\n3\n2\n4\n1", "1, 2, 3, 4, 5", 0)
        , ("10\n10\n9\n8\n7\n6\n5\n4\n3\n2\n1", "1, 2, 3, 4, 5, 6, 7, 8, 9, 10", 0)
        ]
    , test "reassign array escaping variable"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  type intarray = array of int"
        , "  var arr := intarray [ 5 ] of 0"
        , "  var arrSize := atoi(readLine())"
        , "  function replArray(arrSize: int) = ("
        , "    arr := intarray [ arrSize ] of 1;"
        , "    for i := 0 to arrSize - 1 do"
        , "      arr[i] := atoi(readLine())"
        , "  )"
        , "  function printArray(arr: intarray, arrSize: int) ="
        , "    for i := 0 to arrSize - 1 do"
        , "      if i <> arrSize - 1"
        , "      then print(concat(itoa(arr[i]), \", \"))"
        , "      else print(itoa(arr[i]))"
        , "in"
        , "  replArray(arrSize);"
        , "  printArray(arr, arrSize)"
        , "end"
        ]
        [ ("4\n1\n2\n3\n4", "1, 2, 3, 4", 0)
        , ("5\n1\n2\n3\n4\n5", "1, 2, 3, 4, 5", 0)
        , ("6\n1\n2\n3\n4\n5\n6", "1, 2, 3, 4, 5, 6", 0)
        , ("7\n1\n2\n3\n4\n5\n6\n7", "1, 2, 3, 4, 5, 6, 7", 0)
        ]
    , test "string array"
        [ "let"
        , readLineSrc
        , atoiSrc
        , "  type strarray = array of string"
        , "  var arrSize := atoi(readLine())"
        , "  var arr := strarray [ arrSize ] of \"\""
        , "  function readArray(rarr: strarray, rarrSize: int) ="
        , "    for i := 0 to rarrSize - 1 do"
        , "      rarr[i] := readLine()"
        , "  function wordsArray(warr: strarray, warrSize: int): string ="
        , "    let"
        , "      var result := \"\""
        , "      var lastIdx := warrSize - 1"
        , "    in"
        , "      for i := 0 to lastIdx do"
        , "        result := concat(result, if i <> lastIdx"
        , "                                 then concat(warr[i], \" \")"
        , "                                 else warr[i]);"
        , "      result"
        , "    end"
        , "in"
        , "  readArray(arr, arrSize);"
        , "  print(wordsArray(arr, arrSize))"
        , "end"
        ]
        [ ("2\nHey\nbabe", "Hey babe", 0)
        , ("3\nApocalypse\nnow\nlol", "Apocalypse now lol", 0)
        , ("1\nKek", "Kek", 0)
        , ("5\nI\nwanna\nbe\nthe\nboshy", "I wanna be the boshy", 0)
        ]
    , test "string escaping array"
        [ "let"
        , readLineSrc
        , atoiSrc
        , "  type strarray = array of string"
        , "  var arrSize := atoi(readLine())"
        , "  var arr := strarray [ arrSize ] of \"\""
        , "  function readArray() ="
        , "    for i := 0 to arrSize - 1 do"
        , "      arr[i] := readLine()"
        , "  function wordsArray(): string ="
        , "    let"
        , "      var result := \"\""
        , "      var lastIdx := arrSize - 1"
        , "    in"
        , "      for i := 0 to lastIdx do"
        , "        result := concat(result, if i <> lastIdx"
        , "                                 then concat(arr[i], \" \")"
        , "                                 else arr[i]);"
        , "      result"
        , "    end"
        , "in"
        , "  readArray();"
        , "  print(wordsArray())"
        , "end"
        ]
        [ ("2\nHey\nbabe", "Hey babe", 0)
        , ("3\nApocalypse\nnow\nlol", "Apocalypse now lol", 0)
        , ("1\nKek", "Kek", 0)
        , ("5\nI\nwanna\nbe\nthe\nboshy", "I wanna be the boshy", 0)
        ]
    , test "simple record with field assignment in direct order"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  type film = { title: string, minAge: int }"
        , "  function filmToString(f: film): string ="
        , "    concat("
        , "      concat(\"film { title = \", f.title),"
        , "      concat(concat(\", minAge = \", itoa(f.minAge)), \" }\")"
        , "    )"
        , "  var f := film { title = readLine(), minAge = atoi(readLine()) }"
        , "in"
        , "  print(filmToString(f))"
        , "end"
        ]
        [ ("Залупа Иваныча\n18", "film { title = Залупа Иваныча, minAge = 18 }", 0) ]
    , test "simple record with field assignment in random order"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  type film = { title: string, minAge: int }"
        , "  function filmToString(f: film): string ="
        , "    concat("
        , "      concat(\"film { title = \", f.title),"
        , "      concat(concat(\", minAge = \", itoa(f.minAge)), \" }\")"
        , "    )"
        , "  var title := readLine()"
        , "  var minAge := atoi(readLine())"
        , "  var f := film { minAge = minAge, title = title }"
        , "in"
        , "  print(filmToString(f))"
        , "end"
        ]
        [ ("Залупа Иваныча\n18", "film { title = Залупа Иваныча, minAge = 18 }", 0) ]
    , test "record with array field"
        [ "let"
        , unwordsSrc
        , readLineSrc
        , "  var line := readLine()"
        , "  var lineWords := unwords(line)"
        , "  var result := \"\""
        , "in"
        , "  for i := 0 to lineWords.numWords - 1 do"
        , "    if i <> lineWords.numWords - 1"
        , "    then print(concat(lineWords.words[i], \"\\n\"))"
        , "    else print(lineWords.words[i])"
        , "end"
        ]
        [ ("I wanna be the boshy", "I\nwanna\nbe\nthe\nboshy", 0)
        , ("Kill me please", "Kill\nme\nplease", 0)
        , ("Total annihilation", "Total\nannihilation", 0)
        , ("Заяц + белочка = заебелочка", "Заяц\n+\nбелочка\n=\nзаебелочка", 0)
        ]
    , test "complex lvalue"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  type mark = { title: string, mark: int }"
        , "  type markarray = array of mark"
        , "  type marks = { marks: markarray, numMarks: int }"
        , "  type student = { name: string, age: int, marks: marks }"
        , "  type studentarray = array of student"
        , "  var numStudents := atoi(readLine())"
        , "  var students := studentarray [ numStudents ] of nil"
        , "  function readMark(): mark ="
        , "    let"
        , "      var title := readLine()"
        , "      var mark := atoi(readLine())"
        , "    in mark { title = title, mark = mark }"
        , "    end"
        , "  function markToStr(m: mark): string ="
        , "    concat(concat(m.title, \": \"), itoa(m.mark))"
        , "  function readStudent(): student ="
        , "    let"
        , "      var name := readLine()"
        , "      var age := atoi(readLine())"
        , "      var numMarks := atoi(readLine())"
        , "      var marksArr := markarray [ numMarks ] of nil"
        , "    in"
        , "      for i := 0 to numMarks - 1 do"
        , "        marksArr[i] := readMark();"
        , "      student { name = name"
        , "              , age = age"
        , "              , marks = marks { marks = marksArr, numMarks = numMarks }"
        , "              }"
        , "    end"
        , "  function studentToString(s: student): string ="
        , "    let"
        , "      var ageStr := concat(concat(\" \", itoa(s.age)), \" - \")"
        , "      var nameAge := concat(s.name, ageStr)"
        , "      var marksStr := \"\""
        , "    in"
        , "      for i := 0 to s.marks.numMarks - 1 do"
        , "        let"
        , "          var markStr := markToStr(s.marks.marks[i])"
        , "        in"
        , "          marksStr := concat("
        , "            marksStr,"
        , "            if i <> 0 then concat(\", \", markStr) else markStr"
        , "          )"
        , "        end;"
        , "      concat(nameAge, marksStr)"
        , "    end"
        , "in"
        , "  for i := 0 to numStudents - 1 do"
        , "    students[i] := readStudent();"
        , "  for i := 0 to numStudents - 1 do"
        , "    print(concat(studentToString(students[i]), \"\\n\"))"
        , "end"
        ]
        [ ("2\nHenry\n20\n3\ncalculus\n5\ndifferential equations\n4\nphilosophy\n3\n\
           \Jill\n21\n2\nsoftware architecture\n5\nPE\n2",
           "Henry 20 - calculus: 5, differential equations: 4, philosophy: 3\n\
           \Jill 21 - software architecture: 5, PE: 2\n",
           0
          )
        ]
    , test "recursive record"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  type intlist = { val: int, rest: intlist }"
        , "  var ints: intlist := nil"
        , "  var revints: intlist := nil"
        , "  function reverseList(l: intlist, r: intlist): intlist ="
        , "    if l = nil"
        , "    then r"
        , "    else (r := intlist { val = l.val, rest = r };"
        , "         reverseList(l.rest, r))"
        , "  function printList(l: intlist) ="
        , "    while l <> nil do ("
        , "      if l.rest <> nil"
        , "      then print(concat(itoa(l.val), \", \"))"
        , "      else print(itoa(l.val));"
        , "      l := l.rest"
        , "    )"
        , "in"
        , "  while 1 do"
        , "    let"
        , "      var line := readLine()"
        , "    in"
        , "      if line = \"stop\""
        , "      then break"
        , "      else ints := intlist { val = atoi(line), rest = ints }"
        , "    end;"
        , "  revints := reverseList(ints, nil);"
        , "  printList(revints)"
        , "end"
        ]
        [ ("1\n2\n3\n4\nstop", "1, 2, 3, 4", 0)
        , ("12\n10\n11\n7\n1111\n3\nstop", "12, 10, 11, 7, 1111, 3", 0)
        ]
    , test "recusive escaping record"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  type intlist = { val: int, rest: intlist }"
        , "  var ints: intlist := nil"
        , "  var revints: intlist := nil"
        , "  function reverseList(l: intlist) ="
        , "    if l <> nil"
        , "    then (revints := intlist { val = l.val, rest = revints };"
        , "         reverseList(l.rest))"
        , "  function printList(l: intlist) ="
        , "    while l <> nil do ("
        , "      if l.rest <> nil"
        , "      then print(concat(itoa(l.val), \", \"))"
        , "      else print(itoa(l.val));"
        , "      l := l.rest"
        , "    )"
        , "in"
        , "  while 1 do"
        , "    let"
        , "      var line := readLine()"
        , "    in"
        , "      if line = \"stop\""
        , "      then break"
        , "      else ints := intlist { val = atoi(line), rest = ints }"
        , "    end;"
        , "  reverseList(ints);"
        , "  printList(revints)"
        , "end"
        ]
        [ ("1\n2\n3\n4\nstop", "1, 2, 3, 4", 0)
        , ("12\n10\n11\n7\n1111\n3\nstop", "12, 10, 11, 7, 1111, 3", 0)
        ]
    , test "array of records"
        [ "let"
        , readLineSrc
        , atoiSrc
        , "  type film = { title: string, minAge: int }"
        , "  type filmArray = array of film"
        , "  var films := filmArray [ 5 ] of nil"
        , "  var findTitle := readLine()"
        , "  var currentAge := atoi(readLine())"
        , "  function findFilmByTitle("
        , "    title: string, films: filmArray, numFilms: int"
        , "  ): film ="
        , "    let"
        , "      var result: film := nil"
        , "    in"
        , "      for i := 0 to numFilms - 1 do"
        , "        if films[i].title = title"
        , "        then (result := films[i]; break);"
        , "      result"
        , "    end"
        , "in"
        , "  films[0] := film { title = \"Залупа Иваныча\", minAge = 18 };"
        , "  films[1] := film { title = \"Анальное путешествие\", minAge = 12 };"
        , "  films[2] := film { title = \"Сумраки\", minAge = 16 };"
        , "  films[3] := film { title = \"Гот с ЗИЛа\", minAge = 21 };"
        , "  films[4] := film { title = \"Карма & удон\", minAge = 6 };"
        , "  let"
        , "    var film := findFilmByTitle(findTitle, films, 5)"
        , "  in"
        , "    if film = nil"
        , "    then ("
        , "      print(concat(concat(\"Film \", findTitle), \" was not found\"));"
        , "      exit(1)"
        , "    );"
        , "    if film.minAge > currentAge"
        , "    then ("
        , "      print(concat(\"You are too young to watch \", findTitle));"
        , "      exit(1)"
        , "    );"
        , "    print(\"Yes, you can watch it\")"
        , "  end"
        , "end"
        ]
        [ ("Залупа Иваныча\n18", "Yes, you can watch it", 0)
        , ("Залупа Иваныча\n14", "You are too young to watch Залупа Иваныча", 1)
        , ("Залупа Иваныча\n21", "Yes, you can watch it", 0)
        , ("Анальное путешествие\n12", "Yes, you can watch it", 0)
        , ("Анальное путешествие\n45", "Yes, you can watch it", 0)
        , ( "Анальное путешествие\n9"
          , "You are too young to watch Анальное путешествие"
          , 1
          )
        , ("Сумраки\n16", "Yes, you can watch it", 0)
        , ("Сумраки\n17", "Yes, you can watch it", 0)
        , ("Сумраки\n15", "You are too young to watch Сумраки", 1)
        , ("Гот с ЗИЛа\n21", "Yes, you can watch it", 0)
        , ("Гот с ЗИЛа\n35", "Yes, you can watch it", 0)
        , ("Гот с ЗИЛа\n10", "You are too young to watch Гот с ЗИЛа", 1)
        , ("Карма & удон\n6", "Yes, you can watch it", 0)
        , ("Карма & удон\n85", "Yes, you can watch it", 0)
        , ("Карма & удон\n1", "You are too young to watch Карма & удон", 1)
        , ("Малыш и Ларсон\n25", "Film Малыш и Ларсон was not found", 1)
        ]
    , test "escaping array of record"
        [ "let"
        , readLineSrc
        , atoiSrc
        , "  type film = { title: string, minAge: int }"
        , "  type filmArray = array of film"
        , "  var films := filmArray [ 5 ] of nil"
        , "  var findTitle := readLine()"
        , "  var currentAge := atoi(readLine())"
        , "  function findFilmByTitle(title: string): film ="
        , "    let"
        , "      var result: film := nil"
        , "    in"
        , "      for i := 0 to 4 do"
        , "        if films[i].title = title"
        , "        then (result := films[i]; break);"
        , "      result"
        , "    end"
        , "in"
        , "  films[0] := film { title = \"Залупа Иваныча\", minAge = 18 };"
        , "  films[1] := film { title = \"Анальное путешествие\", minAge = 12 };"
        , "  films[2] := film { title = \"Сумраки\", minAge = 16 };"
        , "  films[3] := film { title = \"Гот с ЗИЛа\", minAge = 21 };"
        , "  films[4] := film { title = \"Карма & удон\", minAge = 6 };"
        , "  let"
        , "    var film := findFilmByTitle(findTitle)"
        , "  in"
        , "    if film = nil"
        , "    then ("
        , "      print(concat(concat(\"Film \", findTitle), \" was not found\"));"
        , "      exit(1)"
        , "    );"
        , "    if film.minAge > currentAge"
        , "    then ("
        , "      print(concat(\"You are too young to watch \", findTitle));"
        , "      exit(1)"
        , "    );"
        , "    print(\"Yes, you can watch it\")"
        , "  end"
        , "end"
        ]
        [ ("Залупа Иваныча\n18", "Yes, you can watch it", 0)
        , ("Залупа Иваныча\n14", "You are too young to watch Залупа Иваныча", 1)
        , ("Залупа Иваныча\n21", "Yes, you can watch it", 0)
        , ("Анальное путешествие\n12", "Yes, you can watch it", 0)
        , ("Анальное путешествие\n45", "Yes, you can watch it", 0)
        , ( "Анальное путешествие\n9"
          , "You are too young to watch Анальное путешествие"
          , 1
          )
        , ("Сумраки\n16", "Yes, you can watch it", 0)
        , ("Сумраки\n17", "Yes, you can watch it", 0)
        , ("Сумраки\n15", "You are too young to watch Сумраки", 1)
        , ("Гот с ЗИЛа\n21", "Yes, you can watch it", 0)
        , ("Гот с ЗИЛа\n35", "Yes, you can watch it", 0)
        , ("Гот с ЗИЛа\n10", "You are too young to watch Гот с ЗИЛа", 1)
        , ("Карма & удон\n6", "Yes, you can watch it", 0)
        , ("Карма & удон\n85", "Yes, you can watch it", 0)
        , ("Карма & удон\n1", "You are too young to watch Карма & удон", 1)
        , ("Малыш и Ларсон\n25", "Film Малыш и Ларсон was not found", 1)
        ]
    , test "escaping variables in nested functions"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  function first(p1: int): int ="
        , "    let"
        , "      function second(p2: int) ="
        , "        let"
        , "          function third(p3: int) = p1 := p1 + p2 + p3"
        , "        in"
        , "          third(p2 * p2 + 4);"
        , "          p1 := p1 - p2"
        , "        end"
        , "    in"
        , "      second(11);"
        , "      p1"
        , "    end"
        , "  var p1 := first(atoi(readLine()))"
        , "in"
        , "  print(itoa(p1))"
        , "end"
        ]
        [ ("2", "127", 0)
        , ("3", "128", 0)
        , ("4", "129", 0)
        , ("5", "130", 0)
        ]
    , test "escaping array variable in nested functions"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  type intarray = array of int"
        , "  var arrSize := atoi(readLine())"
        , "  var arr := intarray [ arrSize ] of 0"
        , "  function first(m: int) ="
        , "    let"
        , "      function second(a: int) ="
        , "        let"
        , "          function third() ="
        , "            for i := 0 to arrSize - 1 do"
        , "              arr[i] := arr[i] * m + a"
        , "        in"
        , "          for i := 0 to arrSize - 1 do"
        , "            arr[i] := atoi(readLine());"
        , "          third()"
        , "        end"
        , "    in"
        , "      second(m * 2 + 7);"
        , "      for i := 0 to arrSize - 1 do"
        , "        let"
        , "          var nthStr := itoa(arr[i])"
        , "          var str := if i < arrSize - 1"
        , "                     then concat(nthStr, \", \")"
        , "                     else concat(nthStr, \"\\n\")"
        , "        in print(str)"
        , "        end"
        , "    end"
        , "in first(atoi(readLine()))"
        , "end"
        ]
        [ ("5\n10\n9\n8\n7\n6\n5", "117, 107, 97, 87, 77\n", 0)
        , ("3\n7\n108\n31\n11", "777, 238, 98\n", 0)
        ]
    , test "escaping record variable in nested functions"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  type rec = { val1: int, val2: string }"
        , "  var data := rec { val1 = atoi(readLine()), val2 = readLine() }"
        , "  function first(): string ="
        , "    let"
        , "      function second(): string ="
        , "        let"
        , "          function rec2string(): string ="
        , "            let"
        , "              var hdr := \"rec { \""
        , "              var val1Str := concat(\"val1 = \", itoa(data.val1))"
        , "              var val2Str := concat(\", val2 = \", data.val2)"
        , "              var foot := \" }\""
        , "              var res := concat("
        , "                hdr,"
        , "                concat(val1Str, concat(val2Str, foot))"
        , "              )"
        , "            in"
        , "              data.val2 := \"Hehe\";"
        , "              res"
        , "            end"
        , "        in"
        , "          data.val1 := data.val1 + 18;"
        , "          data.val2 := concat(data.val2, \" Yoha\");"
        , "          rec2string()"
        , "        end"
        , "      var res := second()"
        , "    in"
        , "      data.val2 := concat(data.val2, \" Lil\");"
        , "      res"
        , "    end"
        , "in"
        , "  print(concat(first(), \"\\n\"));"
        , "  print(concat(itoa(data.val1), \"\\n\"));"
        , "  print(concat(data.val2, \"\\n\"))"
        , "end"
        ]
        [ ("2\nKill", "rec { val1 = 20, val2 = Kill Yoha }\n20\nHehe Lil\n", 0)
        , ("11\nNya", "rec { val1 = 29, val2 = Nya Yoha }\n29\nHehe Lil\n", 0)
        ]
    , test "escaping array of records variable in nested functions"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , unwordsSrc
        , "  type point = { x: int, y: int }"
        , "  type pointarray = array of point"
        , "  var numPoints := atoi(readLine())"
        , "  var points := pointarray [ numPoints ] of nil"
        , "  function first(p: point) ="
        , "    let"
        , "      function readPoints() ="
        , "        let"
        , "          function readPoint(): point ="
        , "            let"
        , "              var words := unwords(readLine())"
        , "            in"
        , "              point { x = atoi(words.words[0])"
        , "                    , y = atoi(words.words[1])"
        , "                    }"
        , "            end"
        , "        in"
        , "          for i := 0 to numPoints - 1 do"
        , "            points[i] := readPoint()"
        , "        end"
        , "      function second() ="
        , "        let"
        , "          function third() = ("
        , "            points[2].x := points[3].x + 1;"
        , "            points[points[2].y].x := points[4].x;"
        , "            points[5] := p)"
        , "        in"
        , "          points[1].x := points[2].x + 1;"
        , "          points[points[1].y].x := points[3].x;"
        , "          third()"
        , "        end"
        , "    in"
        , "      readPoints();"
        , "      points[0].x := points[1].x + 1;"
        , "      points[points[0].y].x := points[2].x;"
        , "      second()"
        , "    end"
        , "  function printPoints(points: pointarray, numPoints: int) ="
        , "    let"
        , "      function point2str(p: point): string ="
        , "        concat("
        , "          concat(\"(\", itoa(p.x)),"
        , "          concat(concat(\", \", itoa(p.y)), \")\\n\")"
        , "        )"
        , "    in"
        , "      for i := 0 to numPoints - 1 do"
        , "        print(point2str(points[i]))"
        , "    end"
        , "in"
        , "  first(point { x = 1, y = 2 });"
        , "  printPoints(points, numPoints)"
        , "end"
        ]
        [ ( "6\n2 3\n4 5\n6 1\n11 45\n89 100\n345 787"
          , "(5, 3)\n(89, 5)\n(7, 1)\n(6, 45)\n(89, 100)\n(1, 2)\n"
          , 0
          )
        , ( "7\n100 0\n92 4\n1 6\n100 45\n11 43\n21 12\n333 666"
          , "(1, 0)\n(2, 4)\n(101, 6)\n(100, 45)\n(100, 43)\n(1, 2)\n(100, 666)\n"
          , 0
          )
        ]
    , test "escaping array variable reassign in nested functions"
        [ "let"
        , readLineSrc
        , itoaSrc
        , "  type strarray = array of string"
        , "  var numLines := 3"
        , "  var lines := strarray [ numLines ] of \"Ебать!\""
        , "  function first() ="
        , "    let"
        , "      function second() = ("
        , "        numLines := 4;"
        , "        lines := strarray [ numLines ] of \"\""
        , "      )"
        , "    in"
        , "      second();"
        , "      for i := 0 to numLines - 1 do"
        , "        lines[i] := readLine()"
        , "    end"
        , "  function printLines() ="
        , "    for i := 0 to numLines - 1 do"
        , "      print(concat(lines[i], \"\\n\"))"
        , "in"
        , "  for i := 0 to numLines - 1 do"
        , "    lines[i] := concat(lines[i], itoa(i));"
        , "  first();"
        , "  printLines()"
        , "end"
        ]
        [ ("Пика\nЧу\nПришёл\nК", "Пика\nЧу\nПришёл\nК\n", 0)
        , ("Врачу\nХирургу\nСдаваться\nНа", "Врачу\nХирургу\nСдаваться\nНа\n", 0)
        , ("Мясо\nПорубили\nПика\nЧу", "Мясо\nПорубили\nПика\nЧу\n", 0)
        ]
    , test "escaping record variable reassign in nested functions"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  type struct = { a: int, b: int, c: int }"
        , "  var s := struct { a = 12, b = 13, c = 15 }"
        , "  function first(cmd: int) ="
        , "    let"
        , "      function second() ="
        , "        let"
        , "          function third() ="
        , "            let"
        , "              var newS := struct { a = s.a + cmd"
        , "                                 , b = s.b - cmd"
        , "                                 , c = cmd"
        , "                                 }"
        , "            in"
        , "              s := newS"
        , "            end"
        , "        in"
        , "          cmd := cmd - 2;"
        , "          third()"
        , "        end"
        , "    in"
        , "      cmd := cmd * 2;"
        , "      second()"
        , "    end"
        , "  function printStruct(s: struct) ="
        , "    let"
        , "      var str := concat("
        , "        concat("
        , "          concat(\"(\", itoa(s.a)),"
        , "          concat(\", \", itoa(s.b))"
        , "        ),"
        , "        concat(concat(\", \", itoa(s.c)), \")\\n\")"
        , "      )"
        , "    in"
        , "      print(str)"
        , "    end"
        , "in"
        , "  first(atoi(readLine()));"
        , "  printStruct(s)"
        , "end"
        ]
        [ ("13", "(36, -11, 24)\n", 0)
        , ("456", "(922, -897, 910)\n", 0)
        , ("2", "(14, 11, 2)\n", 0)
        ]
    -- TODO: support recusive functions with params in interpreter
    , test "escaping variable in simple recursive function"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var resStr := \"\""
        , "  var i := atoi(readLine())"
        , "  function recFunc() ="
        , "    if i <> 0 then ("
        , "      resStr := concat(resStr, itoa(i));"
        , "      i := i - 1;"
        , "      recFunc()"
        , "    )"
        , "in"
        , "  recFunc();"
        , "  print(resStr)"
        , "end"
        ]
        [ ("5", "54321", 0)
        , ("4", "4321", 0)
        , ("3", "321", 0)
        , ("2", "21", 0)
        , ("1", "1", 0)
        ]
    , test "escaping variable in nested recursive function"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var resStr := \"\""
        , "  var i := atoi(readLine())"
        , "  function first() ="
        , "    let"
        , "      function recFunc() ="
        , "        if i <> 0 then ("
        , "          resStr := concat(resStr, itoa(i));"
        , "          i := i - 1;"
        , "          recFunc()"
        , "        )"
        , "    in recFunc()"
        , "    end"
        , "in"
        , "  first();"
        , "  print(resStr)"
        , "end"
        ]
        [ ("5", "54321", 0)
        , ("4", "4321", 0)
        , ("3", "321", 0)
        , ("2", "21", 0)
        , ("1", "1", 0)
        ]
    , test "call function that uses static link from recursive function of \
           \the same nesting level"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  var resStr := \"\""
        , "  var i := atoi(readLine())"
        , "  function first() ="
        , "    let"
        , "      function second() = i := i - 1"
        , "      function recFunc() ="
        , "        if i <> 0 then ("
        , "          resStr := concat(resStr, itoa(i));"
        , "          second();"
        , "          recFunc()"
        , "        )"
        , "    in recFunc()"
        , "    end"
        , "in"
        , "  first();"
        , "  print(resStr)"
        , "end"
        ]
        [ ("5", "54321", 0)
        , ("4", "4321", 0)
        , ("3", "321", 0)
        , ("2", "21", 0)
        , ("1", "1", 0)
        ]
    , test "call function that uses static link from function of the same nesting level"
        [ "let"
        , readLineSrc
        , itoaSrc
        , atoiSrc
        , "  var i := atoi(readLine())"
        , "  function first() = i := i - 1"
        , "  function second() = first()"
        , "in"
        , "  second();"
        , "  print(itoa(i))"
        , "end"
        ]
        [ ("5", "4", 0)
        , ("4", "3", 0)
        , ("3", "2", 0)
        , ("1", "0", 0)
        ]
    , test "call function that uses static link from function with bigger nesting"
        [ "let"
        , readLineSrc
        , itoaSrc
        , atoiSrc
        , "  var i := atoi(readLine())"
        , "  function first() = i := i - 1"
        , "  function second() ="
        , "    let"
        , "      function third() = first()"
        , "    in third()"
        , "    end"
        , "in"
        , "  second();"
        , "  print(itoa(i))"
        , "end"
        ]
        [ ("5", "4", 0)
        , ("4", "3", 0)
        , ("3", "2", 0)
        , ("1", "0", 0)
        ]
    , test "variable name shadowing"
        [ "let"
        , readLineSrc
        , itoaSrc
        , atoiSrc
        , "  var i := atoi(readLine())"
        , "  function first(i: int) = ("
        , "    i := i + 1;"
        , "    print(concat(itoa(i), \"\\n\"))"
        , "  )"
        , "in"
        , "  first(1);"
        , "  print(itoa(i))"
        , "end"
        ]
        [ ("3", "2\n3", 0)
        , ("4", "2\n4", 0)
        , ("5", "2\n5", 0)
        , ("6", "2\n6", 0)
        , ("7", "2\n7", 0)
        ]
    , test "function name shadowing"
        [ "let"
        , readLineSrc
        , atoiSrc
        , itoaSrc
        , "  function cool(i: int): int ="
        , "    let"
        , "      function second(x: int): int ="
        , "        let"
        , "          function cool(y: int): int = y * 2"
        , "        in cool(x + 5)"
        , "        end"
        , "    in second(i / 10)"
        , "    end"
        , "  var res := cool(atoi(readLine()))"
        , "in"
        , "  print(itoa(res))"
        , "end"
        ]
        [ ("200", "50", 0)
        , ("340", "78", 0)
        , ("565", "122", 0)
        , ("111", "32", 0)
        ]
    ]

readLineSrc :: Text
readLineSrc = Text.unlines
    [ "function readLine(): string ="
    , "  let"
    , "    var result := \"\""
    , "  in"
    , "    while 1 do"
    , "      let"
    , "        var char := getchar()"
    , "      in"
    , "        if char = \"\\n\" | char = \"\""
    , "        then break;"
    , "        result := concat(result, char)"
    , "      end;"
    , "    result"
    , "  end"
    ]

atoiSrc :: Text
atoiSrc = Text.unlines
    [ "function atoi(s: string): int ="
    , "  let"
    , "    var isNeg := substring(s, 0, 1) = \"-\""
    , "    var i := size(s) - 1"
    , "    var pow := 1"
    , "    var result := 0"
    , "  in"
    , "    while i >= isNeg do ("
    , "      result := result + (ord(substring(s, i, 1)) - 48) * pow;"
    , "      pow := pow * 10;"
    , "      i := i - 1"
    , "    );"
    , "    if isNeg then result := -result;"
    , "    result"
    , "  end"
    ]

itoaSrc :: Text
itoaSrc = Text.unlines
    [ "function itoa(n: int): string ="
    , "  let"
    , "    var isNeg := n < 0"
    , "    var result := \"\""
    , "  in"
    , "    if isNeg then n := -n;"
    , "    while 1 do"
    , "      let"
    , "        var quot := n / 10"
    , "        var rem := n - quot * 10"
    , "      in"
    , "        result := concat(chr(rem + 48), result);"
    , "        if quot = 0 then break;"
    , "        n := quot"
    , "      end;"
    , "    if isNeg then concat(\"-\", result) else result"
    , "  end"
    ]

unwordsSrc :: Text
unwordsSrc = Text.unlines
    [ "type strarray = array of string"
    , "type words = { words: strarray, numWords: int }"
    , "function unwords(s: string): words ="
    , "  let"
    , "    var numWords := 0"
    , "    var isWord := 0"
    , "  in"
    , "    for i := 0 to size(s) - 1 do"
    , "      let"
    , "        var char := substring(s, i, 1)"
    , "      in"
    , "        if char = \" \""
    , "        then isWord := 0"
    , "        else if char <> \" \" & not(isWord)"
    , "             then (numWords := numWords + 1; isWord := 1)"
    , "      end;"
    , "    let"
    , "      var words := strarray [ numWords ] of \"\""
    , "      var curWord := 0"
    , "    in"
    , "      isWord := 0;"
    , "      for i := 0 to size(s) - 1 do"
    , "        let"
    , "          var char := substring(s, i, 1)"
    , "        in"
    , "          if char = \" \" & isWord"
    , "          then (isWord := 0; curWord := curWord + 1)"
    , "          else if char <> \" \""
    , "               then (isWord := 1; words[curWord] := concat(words[curWord], char))"
    , "        end;"
    , "      words { words = words, numWords = numWords }"
    , "    end"
    , "  end"
    ]

test :: TestName -> [Text] -> [(Text, LazyText.Text, Int)] -> TestCase
test testName lines inputs = TestCase{..}
  where testSrc = Text.unlines lines
        testInputs = map (\(stdin, stdout, code) -> Input stdin stdout code) inputs
