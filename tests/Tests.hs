import           Test.HUnit

import qualified EscapeAnalysisTests
import qualified IRGenTests
import qualified ParserTests
import qualified TypeCheckerTests

main :: IO ()
main = runTestTTAndExit $ TestList [ ParserTests.tests
                                   , EscapeAnalysisTests.tests
                                   , TypeCheckerTests.tests
                                   , IRGenTests.tests
                                   ]
