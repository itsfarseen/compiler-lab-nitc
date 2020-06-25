import Test.Backend.Codegen
import Test.Backend.Simulator
import Test.Library
import Test.Grammar
import Test.Tasty
import qualified Test.Golden

main :: IO ()
main = do
  goldenTests <- Test.Golden.main
  defaultMain
    $ testGroup "Main"
    $ [ Test.Backend.Codegen.tests
      , Test.Backend.Simulator.tests
      , Test.Grammar.tests
      , Test.Library.tests
      , goldenTests
      ]

