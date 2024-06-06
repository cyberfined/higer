import           Test.Tasty

import qualified EscapeAnalysisTests
import qualified IRGenTests
import qualified ParserTests
import qualified TypeCheckerTests

main :: IO ()
main = defaultMain $ testGroup "Tiger compiler tests"
    [ ParserTests.tests
    , EscapeAnalysisTests.tests
    , TypeCheckerTests.tests
    , IRGenTests.tests
    ]
