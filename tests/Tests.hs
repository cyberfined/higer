import           Test.HUnit

import qualified ParserTests
import qualified SemantTests

main :: IO ()
main = runTestTTAndExit $ TestList [ ParserTests.tests
                                   , SemantTests.tests
                                   ]
