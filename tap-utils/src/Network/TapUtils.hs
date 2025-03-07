{-# LANGUAGE ForeignFunctionInterface #-}
{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Network.TapUtils
  ( withTap,
    getNameTap,
    getHwaddrTap,
    getIpaddrTap,
    getMtuTap,
  )
where

import Control.Monad (join)
import Foreign
import Foreign.C.Error (Errno (Errno), errnoToIOError)
import Foreign.C.String
import Foreign.C.Types
import GHC.IO.Handle (Handle)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Network.Socket (HostAddress)
import System.IO (withFile)
import System.Posix.IO (handleToFd)
import System.Process.Internals (fdToHandle)

withTap :: String -> (Handle -> IO (Either IOError ())) -> IO (Either IOError ())
withTap devName action = do
  withFile "/dev/net/tun" ReadWriteMode $ \fd -> do
    allocResult <- withCString devName $ \namePtr -> do
      ret <- join $ c_alloc_tap <$> (fromIntegral <$> handleToFd fd) <*> pure namePtr
      if ret < 0
        then return (Left (errnoToIOError "allocTap" (Errno (negate ret)) Nothing Nothing))
        else return (Right ())
    case allocResult of
      Left err -> return (Left err)
      Right () -> do action fd

-- | create a socket for configuration
setTap :: IO Handle
setTap = c_set_tap >>= fdToHandle

-- | get the name of a tap device
getNameTap :: Handle -> IO (Either IOError String)
getNameTap tapfd = allocaBytes 32 $ \namePtr -> do
  result <- join $ c_getname_tap <$> (fromIntegral <$> handleToFd tapfd) <*> pure namePtr
  if result < 0
    then return (Left (errnoToIOError "getNameTap" (Errno (negate result)) Nothing Nothing))
    else Right <$> peekCString (castPtr namePtr)

-- | get the hardware address of a tap device
getHwaddrTap :: Handle -> IO (Either IOError [Word8])
getHwaddrTap tapfd = allocaBytes 6 $ \haPtr -> do
  result <- join $ c_gethwaddr_tap <$> (fromIntegral <$> handleToFd tapfd) <*> pure haPtr
  if result < 0
    then return (Left (errnoToIOError "getHwaddrTap" (Errno (negate result)) Nothing Nothing))
    else Right <$> mapM (peekElemOff (castPtr haPtr :: Ptr Word8)) [0 .. 5]

-- | get the ip address of a tap device
getIpaddrTap :: Handle -> Handle -> IO (Either IOError HostAddress)
getIpaddrTap skfd tapfd = do
  getNameTap tapfd >>= \case
    Left err -> return (Left err)
    Right nameStr ->
      withCString nameStr $ \namePtr ->
        alloca @CUInt $ \ipPtr -> do
          result <- join $ c_getipaddr_tap <$> (fromIntegral <$> handleToFd skfd) <*> pure (castPtr namePtr) <*> pure (castPtr ipPtr)
          if result < 0
            then return (Left (errnoToIOError "getIpaddrTap" (Errno (negate result)) Nothing Nothing))
            else Right . fromIntegral <$> peek ipPtr

-- | get the mtu of a tap device
getMtuTap :: Handle -> String -> IO (Either IOError Int)
getMtuTap skfd name = withCString name $ \namePtr ->
  alloca $ \mtuPtr -> do
    result <- join $ c_getmtu_tap <$> (fromIntegral <$> handleToFd skfd) <*> pure (castPtr namePtr) <*> pure mtuPtr
    if result < 0
      then return (Left (errnoToIOError "getMtuTap" (Errno (negate result)) Nothing Nothing))
      else Right . fromIntegral <$> peek mtuPtr

foreign import ccall unsafe "alloc_tap"
  c_alloc_tap :: CInt -> CString -> IO CInt

foreign import ccall unsafe "getname_tap"
  c_getname_tap :: CInt -> Ptr CUChar -> IO CInt

foreign import ccall unsafe "gethwaddr_tap"
  c_gethwaddr_tap :: CInt -> Ptr CUChar -> IO CInt

foreign import ccall unsafe "getipaddr_tap"
  c_getipaddr_tap :: CInt -> Ptr CUChar -> Ptr CUInt -> IO CInt

foreign import ccall unsafe "getmtu_tap"
  c_getmtu_tap :: CInt -> Ptr CUChar -> Ptr CInt -> IO CInt

foreign import ccall unsafe "set_tap"
  c_set_tap :: IO CInt
