{-# LANGUAGE ForeignFunctionInterface #-}
{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Network.TapUtils
  ( withTap,
    setTap,
    getNameTap,
    getHwaddrTap,
    getIpaddrTap,
    setIpaddrTap,
    getMtuTap,
    setupTap,
    setnetmaskTap,
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
import System.Posix.IO (fdToHandle, handleToFd)
import System.Posix.Types (Fd (Fd))

withTap :: String -> Word32 -> Word32 -> (Handle -> Int -> Word32 -> IO (Either IOError ())) -> IO (Either IOError ())
withTap devName ipv4 netmask action =
  withFile "/dev/net/tun" ReadWriteMode $ \fd -> do
    allocResult <- withCString devName $ \namePtr -> do
      ret <- join $ c_alloc_tap <$> (fromIntegral <$> handleToFd fd) <*> pure namePtr
      if ret < 0
        then return (Left (errnoToIOError "allocTap" (Errno (negate ret)) Nothing Nothing))
        else return (Right ())
    case allocResult of
      Left err -> return (Left err)
      Right () -> do
        skfd <- setTap
        mtuRes <- getMtuTap skfd devName
        case mtuRes of
          Left err -> return (Left err)
          Right mtu -> do
            ipRes <- setIpaddrTap skfd devName ipv4
            case ipRes of
              Left err -> return (Left err)
              Right () -> do
                nmRes <- setnetmaskTap skfd devName netmask
                case nmRes of
                  Left err -> return (Left err)
                  Right () -> action fd mtu ipv4

-- | create a socket for configuration
setTap :: IO Handle
setTap = c_set_tap >>= (fdToHandle . Fd)

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

-- | set the ip address of a tap device
setIpaddrTap :: Handle -> String -> HostAddress -> IO (Either IOError ())
setIpaddrTap skfd name ipaddr = withCString name $ \namePtr -> do
  result <- join $ c_setipaddr_tap <$> (fromIntegral <$> handleToFd skfd) <*> pure (castPtr namePtr) <*> pure (fromIntegral ipaddr)
  if result < 0
    then return (Left (errnoToIOError "setIpaddrTap" (Errno (negate result)) Nothing Nothing))
    else return (Right ())

-- | get the mtu of a tap device
getMtuTap :: Handle -> String -> IO (Either IOError Int)
getMtuTap skfd name = withCString name $ \namePtr ->
  alloca $ \mtuPtr -> do
    result <- join $ c_getmtu_tap <$> (fromIntegral <$> handleToFd skfd) <*> pure (castPtr namePtr) <*> pure mtuPtr
    if result < 0
      then return (Left (errnoToIOError "getMtuTap" (Errno (negate result)) Nothing Nothing))
      else Right . fromIntegral <$> peek mtuPtr

-- | set the tap device to the available state
setupTap :: Handle -> String -> IO (Either IOError ())
setupTap skfd name = withCString name $ \namePtr -> do
  result <- fromIntegral <$> join (c_setup_tap <$> (fromIntegral <$> handleToFd skfd) <*> pure (castPtr namePtr))
  if result < 0
    then return (Left (errnoToIOError "setupTap" (Errno (negate result)) Nothing Nothing))
    else return (Right ())

-- | set the netmask of a tap device
setnetmaskTap :: Handle -> String -> HostAddress -> IO (Either IOError ())
setnetmaskTap skfd name netmask = withCString name $ \namePtr -> do
  result <- fromIntegral <$> join (c_setnetmask_tap <$> (fromIntegral <$> handleToFd skfd) <*> pure (castPtr namePtr) <*> pure (fromIntegral netmask))
  if result < 0
    then return (Left (errnoToIOError "setnetmaskTap" (Errno (negate result)) Nothing Nothing))
    else return (Right ())

foreign import ccall unsafe "alloc_tap"
  c_alloc_tap :: CInt -> CString -> IO CInt

foreign import ccall unsafe "set_tap"
  c_set_tap :: IO CInt

foreign import ccall unsafe "getname_tap"
  c_getname_tap :: CInt -> Ptr CUChar -> IO CInt

foreign import ccall unsafe "gethwaddr_tap"
  c_gethwaddr_tap :: CInt -> Ptr CUChar -> IO CInt

foreign import ccall unsafe "getipaddr_tap"
  c_getipaddr_tap :: CInt -> Ptr CUChar -> Ptr CUInt -> IO CInt

foreign import ccall unsafe "setipaddr_tap"
  c_setipaddr_tap :: CInt -> Ptr CUChar -> CUInt -> IO CInt

foreign import ccall unsafe "getmtu_tap"
  c_getmtu_tap :: CInt -> Ptr CUChar -> Ptr CInt -> IO CInt

foreign import ccall unsafe "setup_tap"
  c_setup_tap :: CInt -> Ptr CUChar -> IO CInt

foreign import ccall unsafe "setnetmask_tap"
  c_setnetmask_tap :: CInt -> Ptr CUChar -> CUInt -> IO CInt