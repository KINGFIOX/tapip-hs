module Main where

import Data.Bits ((.&.))
import Data.List (intercalate)
import Data.Word (Word32, Word8)
import Foreign (allocaBytes)
import GHC.Conc.IO (threadDelay)
import GHC.IO.Handle (Handle, hIsOpen)
import Network.Socket (HostAddress)
import Network.TapUtils
import System.IO (hGetBuf)
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

-- ip tuntap add mode tun dev tap0
-- ip addr add 10.0.0.1/24 dev tap0
-- ip link set up dev tap0

main :: IO ()
main = do
  putStrLn "creating tap device..."
  ret <- withTap "tap0" testTapDevice
  print $ "ret: " ++ show ret

testTapDevice :: Handle -> IO (Either IOError ())
testTapDevice fd = do
  openStatus <- hIsOpen fd
  putStrLn $ "openStatus: " ++ show openStatus
  threadDelay 1000000000
  allocaBytes 1500 $ \buf -> do
    bytes <- hGetBuf fd buf 1500
    putStrLn $ "bytes: " ++ show bytes
  return (Right ())
