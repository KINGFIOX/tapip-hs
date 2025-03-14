{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Tapip.Phy (RxToken, rxConsume, TxToken, txProduce, Device, receive, transmit) where

import Data.ByteString (ByteString)

class RxToken tok where
  rxConsume :: forall r. tok -> (ByteString -> r) -> r

class TxToken tok where
  txProduce :: forall r. tok -> Int -> (ByteString -> r) -> IO r

class Device dev where
  type RxTokenType dev
  type TxTokenType dev
  receive :: dev -> Int -> IO (RxTokenType dev, TxTokenType dev)
  transmit :: dev -> Int -> IO (TxTokenType dev)
