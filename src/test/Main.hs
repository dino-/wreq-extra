import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $ testGroup " tests"
  [ unitTests
  ]


unitTests :: TestTree
unitTests = testGroup "unit tests"
  [ testCase "True is True" $
      True @=? True
  ]
