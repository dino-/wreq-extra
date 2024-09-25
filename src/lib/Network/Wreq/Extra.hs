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


getAny :: String -> IO (Response ByteString)
getAny = getWith lenientResponseOptions


postAny :: Postable a => String -> a -> IO (Response ByteString)
postAny = postWith lenientResponseOptions
