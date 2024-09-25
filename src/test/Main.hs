{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Control.Lens ( (^.) )
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char ( toUpper )
import Data.String.Conv ( toS )
import Network.HTTP.Client.Internal ( HttpException )
import Network.Wreq ( Response, responseStatus, statusCode, statusMessage )
import Network.Wreq.Extra ( getAny, getAnyEither, postAny, postAnyEither )
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $ testGroup " tests"
  [ testGroup "GET tests" $ testBadURL getAnyEither : map testGet testStatuses
  , testGroup "POST tests"
      $ testBadURL (flip postAnyEither ("foobar" :: BS.ByteString))
      : map testPost testStatuses
  ]


-- NOTE: 3XX codes are for redirection and wreq follows the Location header to
-- the new URI, resulting in a 200 OK. So those codes are not a good test here.

testStatuses :: [(Int, BS.ByteString)]
testStatuses =
  [ (200, "OK")
  , (202, "Accepted")
  , (226, "IM Used")
  , (400, "Bad Request")
  , (404, "Not Found")
  , (407, "Proxy Authentication Required")
  , (418, "I'm a teapot")
  , (500, "Internal Server Error")
  , (503, "Service Unavailable")
  , (507, "Insufficient Storage")
  ]


testServerURL :: String
-- This server seems to sometimes get overloaded
-- testServerURL = "http://httpbin.org/status/"
testServerURL = "http://httpstat.us/"


evaluateResponse :: Int -> BS.ByteString
  -> (String -> IO (Response BL.ByteString)) -> Assertion
evaluateResponse code msg requestFunc = do
  response <- requestFunc $ testServerURL <> show code
  code @=? response ^. responseStatus . statusCode
  -- Case is insignificant here and servers will respond with whatever case they feel like
  BS.map toUpper msg @=? BS.map toUpper (response ^. responseStatus . statusMessage)


testGet :: (Int, BS.ByteString) -> TestTree
testGet (code, msg) = testCase ("getAny " <> show code <> " " <> toS msg) $
  evaluateResponse code msg getAny


testPost :: (Int, BS.ByteString) -> TestTree
testPost (code, msg) = testCase ("postAny " <> show code <> " " <> toS msg) $
  evaluateResponse code msg $ flip postAny ("foobar" :: BS.ByteString)


testBadURL :: (String -> IO (Either HttpException (Response BL.ByteString))) -> TestTree
testBadURL requestFunc = testCase "Bad URL sent to getXXXEither" $ do
  eResponse <- requestFunc "notAValidURL/"
  case eResponse of
    Right _ -> fail "This should not have succeeded"
    Left _ -> pure ()
