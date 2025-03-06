{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

module Network.TapUtils
  ( allocTap,
    setTap,
    unsetTap,
    getNameTap,
    getHwaddrTap,
    getIpaddrTap,
    setIpaddrTap,
    getMtuTap,
    setupTap,
    setdownTap,
    setflagsTap,
    setnetmaskTap,
  )
where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.IO.Handle (Handle)
import Network.Socket (HostAddress)
import System.IO (hClose)
import System.Posix.IO (fdToHandle, handleToFd)
import System.Posix.Types (Fd (Fd))

-- | alloc a tap device
allocTap :: String -> IO (Maybe Handle)
allocTap devName = do
  fd <- withCString devName (fmap fromIntegral . c_alloc_tap)
  if fd < 0
    then return Nothing
    else Just <$> fdToHandle (Fd fd)

-- | create a socket for configuration
setTap :: IO Handle
setTap = c_set_tap >>= (fdToHandle . Fd)

-- | close the socket
unsetTap :: Handle -> IO ()
unsetTap = hClose

-- | get the name of a tap device
getNameTap :: Handle -> IO String
getNameTap tapfd = allocaBytes 32 $ \namePtr -> do
  Fd fd' <- fromIntegral <$> handleToFd tapfd
  result <- c_getname_tap fd' namePtr
  if result < 0
    then return ""
    else peekCString (castPtr namePtr)

-- | get the hardware address of a tap device
getHwaddrTap :: Handle -> IO [Word8]
getHwaddrTap tapfd = allocaBytes 6 $ \haPtr -> do
  Fd fd' <- fromIntegral <$> handleToFd tapfd
  result <- c_gethwaddr_tap fd' haPtr
  if result < 0
    then return []
    else mapM (peekElemOff (castPtr haPtr :: Ptr Word8)) [0 .. 5]

-- | get the ip address of a tap device
getIpaddrTap :: Handle -> Handle -> IO HostAddress
getIpaddrTap skfd tapfd = do
  name <- getNameTap tapfd
  withCString name $ \namePtr ->
    alloca @CUInt $ \ipPtr -> do
      Fd skfd' <- fromIntegral <$> handleToFd skfd
      _ <- c_getipaddr_tap skfd' (castPtr namePtr) (castPtr ipPtr)
      fromIntegral <$> peek ipPtr

-- | set the ip address of a tap device
setIpaddrTap :: Handle -> String -> HostAddress -> IO Int
setIpaddrTap skfd name ipaddr = withCString name $ \namePtr -> do
  Fd skfd' <- fromIntegral <$> handleToFd skfd
  fromIntegral <$> c_setipaddr_tap skfd' (castPtr namePtr) (fromIntegral ipaddr)

-- | get the mtu of a tap device
getMtuTap :: Handle -> String -> IO Int
getMtuTap skfd name = withCString name $ \namePtr ->
  alloca $ \mtuPtr -> do
    Fd skfd' <- fromIntegral <$> handleToFd skfd
    result <- c_getmtu_tap skfd' (castPtr namePtr) mtuPtr
    if result < 0
      then return (-1)
      else fromIntegral <$> peek mtuPtr

-- | set the tap device to the available state
setupTap :: Handle -> String -> IO Int
setupTap skfd name = withCString name $ \namePtr -> do
  Fd skfd' <- fromIntegral <$> handleToFd skfd
  fromIntegral <$> c_setup_tap skfd' (castPtr namePtr)

-- | set the tap device to the unavailable state
setdownTap :: Handle -> String -> IO Int
setdownTap skfd name = withCString name $ \namePtr -> do
  Fd skfd' <- fromIntegral <$> handleToFd skfd
  fromIntegral <$> c_setdown_tap skfd' (castPtr namePtr)

-- | set the flags of a tap device
setflagsTap :: Handle -> String -> Word16 -> Bool -> IO Int
setflagsTap skfd name flags set = withCString name $ \namePtr -> do
  Fd skfd' <- fromIntegral <$> handleToFd skfd
  fromIntegral <$> c_setflags_tap skfd' (castPtr namePtr) (fromIntegral flags) (if set then 1 else 0)

-- | set the netmask of a tap device
setnetmaskTap :: Handle -> String -> HostAddress -> IO Int
setnetmaskTap skfd name netmask = withCString name $ \namePtr -> do
  Fd skfd' <- fromIntegral <$> handleToFd skfd
  fromIntegral <$> c_setnetmask_tap skfd' (castPtr namePtr) (fromIntegral netmask)

foreign import ccall unsafe "alloc_tap"
  c_alloc_tap :: CString -> IO CInt

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

foreign import ccall unsafe "setdown_tap"
  c_setdown_tap :: CInt -> Ptr CUChar -> IO CInt

foreign import ccall unsafe "setflags_tap"
  c_setflags_tap :: CInt -> Ptr CUChar -> CUShort -> CInt -> IO CInt

foreign import ccall unsafe "setnetmask_tap"
  c_setnetmask_tap :: CInt -> Ptr CUChar -> CUInt -> IO CInt