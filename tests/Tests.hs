import Test.HUnit

import qualified ParserTests

main :: IO ()
main = runTestTTAndExit $ TestList $ [ ParserTests.tests ]
