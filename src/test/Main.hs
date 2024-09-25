{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Data.ByteString.Char8 qualified as BS
import Data.Char ( toUpper )
import Data.String.Conv ( toS )
import Lens.Micro ( (^.) )
import Network.Wreq ( responseStatus, statusCode, statusMessage )
import Network.Wreq.Extra ( getAny )
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $ testGroup " tests"
  [ unitTests
  ]


testServerUrl :: String
testServerUrl = "http://httpstat.us/"
-- testServerUrl = "http://httpbin.org/status/"


testEndpoint :: Int -> BS.ByteString -> TestTree
testEndpoint code msg = testCase (show code <> " " <> toS msg) $ do
  response <- getAny $ testServerUrl <> (show code)
  code @=? response ^. responseStatus . statusCode
  (BS.map toUpper msg) @=? (BS.map toUpper $ response ^. responseStatus . statusMessage)


-- NOTE: 3XX codes are for redirection and wreq follows the Location header to
-- the new URI, resulting in a 200 OK. So those codes are not a good test here.

unitTests :: TestTree
unitTests = testGroup "unit tests"
  [ testEndpoint 200 "OK"
  , testEndpoint 202 "Accepted"
  , testEndpoint 226 "IM Used"
  , testEndpoint 400 "Bad Request"
  , testEndpoint 404 "Not Found"
  , testEndpoint 407 "Proxy Authentication Required"
  , testEndpoint 418 "I'm a teapot"
  , testEndpoint 500 "Internal Server Error"
  , testEndpoint 503 "Service Unavailable"
  , testEndpoint 507 "Insufficient Storage"
  ]
