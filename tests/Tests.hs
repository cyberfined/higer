import           Test.Tasty

import qualified Codegen.Tests       as CodegenTests
import qualified EscapeAnalysisTests
import qualified IRCanonTests
import qualified IRGenTests
import qualified ParserTests
import qualified TypeCheckerTests

main :: IO ()
main = defaultMain $ testGroup "Tiger compiler tests"
    [ ParserTests.tests
    , EscapeAnalysisTests.tests
    , TypeCheckerTests.tests
    , IRGenTests.tests
    , IRCanonTests.tests
    , CodegenTests.tests
    ]
