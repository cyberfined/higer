import           Test.HUnit

import qualified ParserTests
import qualified TypeCheckerTests
import qualified EscapeAnalysisTests

main :: IO ()
main = runTestTTAndExit $ TestList [ ParserTests.tests
                                   , EscapeAnalysisTests.tests
                                   , TypeCheckerTests.tests
                                   ]
