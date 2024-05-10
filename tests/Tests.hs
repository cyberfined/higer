import           Test.Tasty

import qualified EscapeAnalysisTests
import qualified ParserTests
import qualified TypeCheckerTests

main :: IO ()
main = defaultMain $ testGroup "Tiger compiler tests"
    [ ParserTests.tests
    , EscapeAnalysisTests.tests
    , TypeCheckerTests.tests
    ]
