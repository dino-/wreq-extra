import Test.Network.Wreq.Extra ( tests )
import Test.Tasty


main :: IO ()
main = defaultMain $ testGroup " tests" [tests]
