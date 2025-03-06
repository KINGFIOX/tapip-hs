module Main where

import Data.Bits ((.&.))
import Data.ByteString (hGetSome)
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
  ret <- withTap "tap0" (parseIpAddress "192.168.7.1") (parseIpAddress "255.255.255.0") testTapDevice
  print $ "ret: " ++ show ret

testTapDevice :: Handle -> Int -> Word32 -> IO (Either IOError ())
testTapDevice fd mtu ipv4 = do
  putStrLn $ "mtu: " ++ show mtu
  putStrLn $ "ipv4: " ++ formatIpAddress ipv4
  b1 <- hGetSome fd mtu
  putStrLn $ "b1: " ++ show b1
  return (Right ())
