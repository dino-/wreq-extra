{-# LANGUAGE OverloadedStrings #-}

module Network.Wreq.Extra
  ( getAny
  , postAny
  )
  where

import Data.ByteString.Lazy ( ByteString )
import Lens.Micro ( set )
import Network.Wreq
  ( Options, Response
  , checkResponse, defaults, getWith, postWith
  )
import Network.Wreq.Types ( Postable )


lenientResponseOptions :: Options
lenientResponseOptions = set checkResponse (Just $ \_ _ -> return ()) defaults


-- | Issue a GET request and responding with whatever comes back, no throwing
--   exceptions
getAny :: String -> IO (Response ByteString)
getAny = getWith lenientResponseOptions


-- | Issue a POST request and responding with whatever comes back, no throwing
--   exceptions
postAny :: Postable a => String -> a -> IO (Response ByteString)
postAny = postWith lenientResponseOptions
