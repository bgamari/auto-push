import Test.Tasty
import Test.Tasty.HUnit
import qualified Autopush.Tests.DB as DB

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Autopush"
          [ DB.tests
          ]
