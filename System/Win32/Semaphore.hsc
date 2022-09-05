#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Semaphore
-- Copyright   :  (c) Sam Derbyshire, 2022
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Sam Derbyshire
-- Stability   :  provisional
-- Portability :  portable
--
-- Windows Semaphore objects and operations
--
-----------------------------------------------------------------------------

module System.Win32.Semaphore
    ( -- * Access modes
      AccessMode
    , sEMAPHORE_ALL_ACCESS
    , sEMAPHORE_MODIFY_STATE

      -- * Managing semaphores
    , createSemaphore
    , openSemaphore
    , releaseSemaphore
    ) where

import System.Win32.File
import System.Win32.Types

import Foreign hiding (void)
import Foreign.C ( withCAString )

##include "windows_cconv.h"

#include <windows.h>

----------------------------------------------------------------
-- Access rights for semaphores
----------------------------------------------------------------

#{enum AccessMode,
    , sEMAPHORE_ALL_ACCESS   = SEMAPHORE_ALL_ACCESS
    , sEMAPHORE_MODIFY_STATE = SEMAPHORE_MODIFY_STATE
    }

----------------------------------------------------------------
-- Semaphores
----------------------------------------------------------------

-- | Open a semaphore with the given name, or create a new semaphore
-- if no such semaphore exists, with initial count @i@ and maximum count @m@.
--
-- The returned boolean indicates whether a semaphore already existed.
--
-- The counts must satisfy @i >= 0@, @m > 0@ and @i <= m@; they are ignored
-- if a semaphore with the given name already exists.
--
-- Use 'openSemaphore' if you don't want to create a new semaphore.
createSemaphore :: Maybe SECURITY_ATTRIBUTES
                -> LONG         -- ^ initial count @i@ with @0 <= i <= m@
                -> LONG         -- ^ maximum count @m > 0@
                -> Maybe String -- ^ (optional) semaphore name
                                -- (case-sensitive, limited to MAX_PATH characters)
                -> IO (HANDLE, Bool)
createSemaphore mb_sec initial_count max_count mb_name =
  maybeWith with mb_sec $ \ c_sec -> do
  maybeWith withCAString mb_name $ \ c_name -> do
  handle <- c_CreateSemaphore c_sec initial_count max_count c_name
  err_code <- getLastError
  already_exists <-
    case err_code of
      (# const ERROR_INVALID_HANDLE) ->
        errorWin "createSemaphore: name matches non-semaphore"
      (# const ERROR_ALREADY_EXISTS) ->
        return True
      _                              ->
        return False
  if handle == nullPtr
  then errorWin "createSemaphore"
  else return (handle, already_exists)

foreign import WINDOWS_CCONV unsafe "windows.h CreateSemaphoreA"
  c_CreateSemaphore :: LPSECURITY_ATTRIBUTES -> LONG -> LONG -> LPCSTR -> IO HANDLE

-- | Open an existing semaphore.
openSemaphore :: AccessMode -- ^ desired access mode
              -> Bool       -- ^ should child processes inherit the handle?
              -> String     -- ^ name of the semaphore to open (case-sensitive)
              -> IO HANDLE
openSemaphore amode inherit name =
  withTString name $ \c_name ->
  failIfNull "openSemaphore" $ c_OpenSemaphore (fromIntegral amode) inherit c_name

foreign import WINDOWS_CCONV unsafe "windows.h OpenSemaphoreW"
  c_OpenSemaphore :: DWORD -> BOOL -> LPCWSTR -> IO HANDLE

-- | Increase the count of the semaphore by the given amount.
--
-- Returns the count of the semaphore before the increase.
--
-- Throws an error if the count would exceeded the maximum count
-- of the semaphore.
releaseSemaphore :: HANDLE -> LONG -> IO LONG
releaseSemaphore sem count =
  with 0 $ \ ptr_prevCount -> do
  failIfFalse_ "releaseSemaphore" $ c_ReleaseSemaphore sem count ptr_prevCount
  peek ptr_prevCount

foreign import WINDOWS_CCONV unsafe "windows.h ReleaseSemaphore"
  c_ReleaseSemaphore :: HANDLE -> LONG -> Ptr LONG -> IO BOOL

----------------------------------------------------------------
-- End
----------------------------------------------------------------
