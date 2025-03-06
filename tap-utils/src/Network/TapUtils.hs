{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}

module Network.TapUtils
  ( allocTap,
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
import System.IO (openFile)
import System.Posix.IO (fdToHandle, handleToFd)
import System.Posix.Types (Fd (Fd))

-- | alloc a tap device
allocTap :: String -> IO (Either IOError Handle)
allocTap devName = do
  handle <- openFile "/dev/net/tun" ReadWriteMode
  -- fd <- withCString devName (fmap fromIntegral . c_alloc_tap)
  ret <- join $ withCString devName $ \namePtr -> c_alloc_tap <$> (fromIntegral <$> handleToFd handle) <*> pure namePtr

  if ret < 0
    then return (Left (errnoToIOError "allocTap" (Errno (negate ret)) Nothing Nothing))
    else return (Right handle)

-- | create a socket for configuration
setTap :: IO Handle
setTap = c_set_tap >>= (fdToHandle . Fd)

-- | get the name of a tap device
getNameTap :: Handle -> IO String
getNameTap tapfd = allocaBytes 32 $ \namePtr -> do
  result <- join $ c_getname_tap <$> (fromIntegral <$> handleToFd tapfd) <*> pure namePtr
  if result < 0
    then return ""
    else peekCString (castPtr namePtr)

-- | get the hardware address of a tap device
getHwaddrTap :: Handle -> IO [Word8]
getHwaddrTap tapfd = allocaBytes 6 $ \haPtr -> do
  result <- join $ c_gethwaddr_tap <$> (fromIntegral <$> handleToFd tapfd) <*> pure haPtr
  if result < 0
    then return []
    else mapM (peekElemOff (castPtr haPtr :: Ptr Word8)) [0 .. 5]

-- | get the ip address of a tap device
getIpaddrTap :: Handle -> Handle -> IO HostAddress
getIpaddrTap skfd tapfd = do
  name <- getNameTap tapfd
  withCString name $ \namePtr ->
    alloca @CUInt $ \ipPtr -> do
      _ <- join $ c_getipaddr_tap <$> (fromIntegral <$> handleToFd skfd) <*> pure (castPtr namePtr) <*> pure (castPtr ipPtr)
      fromIntegral <$> peek ipPtr

-- | set the ip address of a tap device
setIpaddrTap :: Handle -> String -> HostAddress -> IO Int
setIpaddrTap skfd name ipaddr = withCString name $ \namePtr -> do
  fromIntegral <$> join (c_setipaddr_tap <$> (fromIntegral <$> handleToFd skfd) <*> pure (castPtr namePtr) <*> pure (fromIntegral ipaddr))

-- | get the mtu of a tap device
getMtuTap :: Handle -> String -> IO Int
getMtuTap skfd name = withCString name $ \namePtr ->
  alloca $ \mtuPtr -> do
    result <- join $ c_getmtu_tap <$> (fromIntegral <$> handleToFd skfd) <*> pure (castPtr namePtr) <*> pure mtuPtr
    if result < 0
      then return (-1)
      else fromIntegral <$> peek mtuPtr

-- | set the tap device to the available state
setupTap :: Handle -> String -> IO Int
setupTap skfd name = withCString name $ \namePtr -> do
  fromIntegral <$> join (c_setup_tap <$> (fromIntegral <$> handleToFd skfd) <*> pure (castPtr namePtr))

-- | set the netmask of a tap device
setnetmaskTap :: Handle -> String -> HostAddress -> IO Int
setnetmaskTap skfd name netmask = withCString name $ \namePtr -> do
  fromIntegral <$> join (c_setnetmask_tap <$> (fromIntegral <$> handleToFd skfd) <*> pure (castPtr namePtr) <*> pure (fromIntegral netmask))

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