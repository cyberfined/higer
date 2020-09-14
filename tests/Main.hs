import Test.HUnit
import qualified ParserTests
import qualified TypeCheckerTests
import Control.Monad(void)

main :: IO ()
main = void $ runTestTT $ TestList
    [ ParserTests.tests
    , TypeCheckerTests.tests
    ]
