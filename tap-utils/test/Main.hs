{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bits ((.&.))
import Data.ByteString (hGet)
import Data.List (intercalate)
import Data.Word (Word32, Word8)
import GHC.IO.Handle (Handle)
import Network.Socket (HostAddress)
import Network.TapUtils
import Text.Printf (printf)

-- | format a mac address to a string
formatMacAddress :: [Word8] -> String
formatMacAddress = intercalate ":" . map (printf "%02x")

-- | format an ip address to a string
formatIpAddress :: HostAddress -> String
formatIpAddress ip =
  let (a, b, c, d) = toIPv4 ip
   in printf "%d.%d.%d.%d" a b c d
  where
    toIPv4 :: Word32 -> (Word8, Word8, Word8, Word8)
    toIPv4 w =
      ( fromIntegral (w .&. 0xFF),
        fromIntegral ((w `div` 0x100) .&. 0xFF),
        fromIntegral ((w `div` 0x10000) .&. 0xFF),
        fromIntegral ((w `div` 0x1000000) .&. 0xFF)
      )

-- | parse an ip address from a string
parseIpAddress :: String -> Word32
parseIpAddress s =
  case map read $ words $ map (\c -> if c == '.' then ' ' else c) s of
    [a, b, c, d] ->
      fromIntegral $
        (d `mod` (256 :: Int)) * 0x1000000
          + (c `mod` 256) * 0x10000
          + (b `mod` 256) * 0x100
          + (a `mod` 256)
    _ -> error $ "Invalid IP address format: " ++ s

main :: IO ()
main = do
  putStrLn "creating tap device..."
  handle <- allocTap "tap0"
  case handle of
    Left err -> error $ show err
    Right tapFd -> testTapDevice tapFd

testTapDevice :: Handle -> IO ()
testTapDevice tapFd = do
  -- create a socket for network device operations
  skFd <- setTap

  hwAddr <- getHwaddrTap tapFd
  case hwAddr of
    Left err -> error $ show err
    Right hwAddr' -> putStrLn $ "hardware address: " ++ formatMacAddress hwAddr'

  -- get and show the device information
  name <- getNameTap tapFd
  putStrLn $ "device name: " ++ show name

  case name of
    Left err -> error $ show err
    Right nameStr -> do
      -- setting the ip address (192.168.7.1)
      let ipAddr = parseIpAddress "192.168.7.1"
      putStrLn $ "setting ip address: " ++ formatIpAddress ipAddr
      _ <- setIpaddrTap skFd nameStr ipAddr

      -- setting the netmask (255.255.255.0)
      let netmask = parseIpAddress "255.255.255.0"
      putStrLn $ "setting netmask: " ++ formatIpAddress netmask
      _ <- setnetmaskTap skFd nameStr netmask

      -- enable the device
      putStrLn "enabling device..."
      _ <- setupTap skFd nameStr

      -- verify the configuration
      configuredIp <- getIpaddrTap skFd tapFd
      case configuredIp of
        Left err -> error $ show err
        Right ip -> putStrLn $ "configured ip address: " ++ formatIpAddress ip

      -- get and show the MTU
      mtu <- getMtuTap skFd nameStr
      putStrLn $ "MTU: " ++ show mtu

      case mtu of
        Left err -> error $ show err
        Right mtu' -> do
          b1 <- hGet tapFd mtu'
          putStrLn $ "b1: " ++ show b1

  return ()
