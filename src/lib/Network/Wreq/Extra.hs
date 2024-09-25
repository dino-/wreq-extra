{-# LANGUAGE OverloadedStrings #-}

module Network.Wreq.Extra
  ( getAny
  , getAnyEither
  , postAny
  , postAnyEither
  )
  where

import Control.Lens ( set )
import Control.Exception ( try )
import Data.ByteString.Lazy ( ByteString )
import Network.HTTP.Client ( HttpException )
import Network.Wreq
  ( Options, Response
  , checkResponse, defaults, getWith, postWith
  )
import Network.Wreq.Types ( Postable )


lenientResponseOptions :: Options
lenientResponseOptions = set checkResponse (Just $ \_ _ -> return ()) defaults


-- | Issue a GET request and responding with whatever comes back, no throwing
--   exceptions for non-2XX status codes
getAny :: String -> IO (Response ByteString)
getAny = getWith lenientResponseOptions


-- | Issue a GET request and responding with whatever comes back, no throwing
--   exceptions for non-2XX status codes and expressing any HttpException as
--   the Left side of the Either
getAnyEither :: String -> IO (Either HttpException (Response ByteString))
getAnyEither = try . getWith lenientResponseOptions


-- | Issue a POST request and responding with whatever comes back, no throwing
--   exceptions for non-2XX status codes
postAny :: Postable a => String -> a -> IO (Response ByteString)
postAny = postWith lenientResponseOptions


-- | Issue a POST request and responding with whatever comes back, no throwing
--   exceptions for non-2XX status codes and expressing any HttpException as
--   the Left side of the Either
postAnyEither :: Postable a => String -> a -> IO (Either HttpException (Response ByteString))
postAnyEither url = try . postWith lenientResponseOptions url
