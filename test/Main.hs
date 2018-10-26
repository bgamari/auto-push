import Test.Tasty
import Test.Tasty.HUnit
import qualified Autopush.Tests.DB as DB
import qualified Autopush.Tests.Actions as Actions

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Autopush"
          [ DB.tests
          , Actions.tests
          ]
