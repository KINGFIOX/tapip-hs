{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tapip.Phy.TuntapInterface (new) where

import GHC.IO.Handle (Handle)
import Network.TapUtils (allocTap)

data TuntapInterface = TuntapInterface
  { handle :: Handle,
    mtu :: Int
  }

new :: String -> IO TuntapInterface
new name = do
  (handle, _, _, mtu) <- allocTap name
  return TuntapInterface {handle, mtu}
