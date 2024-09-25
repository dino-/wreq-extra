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
  [ getTests
  , postTests
  ]


-- NOTE: 3XX codes are for redirection and wreq follows the Location header to
-- the new URI, resulting in a 200 OK. So those codes are not a good test here.

getTests :: TestTree
getTests = testGroup "GET tests"
  [ testGet 200 "OK"
  , testGet 202 "Accepted"
  , testGet 226 "IM Used"
  , testGet 400 "Bad Request"
  , testGet 404 "Not Found"
  , testGet 407 "Proxy Authentication Required"
  , testGet 418 "I'm a teapot"
  , testGet 500 "Internal Server Error"
  , testGet 503 "Service Unavailable"
  , testGet 507 "Insufficient Storage"
  , testBadURL getAnyEither
  ]


postTests :: TestTree
postTests = testGroup "POST tests"
  [ testPost 200 "OK"
  , testPost 202 "Accepted"
  , testPost 226 "IM Used"
  , testPost 400 "Bad Request"
  , testPost 404 "Not Found"
  , testPost 407 "Proxy Authentication Required"
  , testPost 418 "I'm a teapot"
  , testPost 500 "Internal Server Error"
  , testPost 503 "Service Unavailable"
  , testPost 507 "Insufficient Storage"
  , testBadURL $ flip postAnyEither ("foobar" :: BS.ByteString)
  ]


evaluateResponse :: Int -> BS.ByteString -> (String -> IO (Response BL.ByteString)) -> Assertion
evaluateResponse code msg requestFunc = do
  response <- requestFunc $ "http://httpbin.org/status/" <> (show code)
  code @=? response ^. responseStatus . statusCode
  (BS.map toUpper msg) @=? (BS.map toUpper $ response ^. responseStatus . statusMessage)


testGet :: Int -> BS.ByteString -> TestTree
testGet code msg = testCase (show code <> " " <> toS msg) $
  evaluateResponse code msg getAny


testPost :: Int -> BS.ByteString -> TestTree
testPost code msg = testCase (show code <> " " <> toS msg) $
  evaluateResponse code msg $ flip postAny ("foobar" :: BS.ByteString)


testBadURL :: (String -> IO (Either HttpException (Response BL.ByteString))) -> TestTree
testBadURL requestFunc = testCase "Bad URL sent to getAnyEither" $ do
  eResponse <- requestFunc "notAValidURL/"
  case eResponse of
    Right _ -> fail "This should not have succeeded"
    Left _ -> pure ()
