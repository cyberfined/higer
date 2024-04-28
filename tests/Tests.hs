import           Test.HUnit

import qualified ParserTests
import qualified SemantTests
import qualified EscapeAnalysisTests

main :: IO ()
main = runTestTTAndExit $ TestList [ ParserTests.tests
                                   , SemantTests.tests
                                   , EscapeAnalysisTests.tests
                                   ]
