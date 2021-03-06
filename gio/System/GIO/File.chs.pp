--  GIMP Toolkit (GTK) Binding for Haskell: binding to gio -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 13-Oct-2008
--
--  Copyright (c) 2008 Peter Gavin
--
--  This library is free software: you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public License
--  as published by the Free Software Foundation, either version 3 of
--  the License, or (at your option) any later version.
--  
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--  
--  You should have received a copy of the GNU Lesser General Public
--  License along with this program.  If not, see
--  <http://www.gnu.org/licenses/>.
--  
--  GIO, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GIO documentation.
--  
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.GIO.File (
    File(..),
    FileClass,
    FileQueryInfoFlags(..),
    FileCreateFlags(..),
    FileCopyFlags(..),
    FileMonitorFlags(..),
    FilesystemPreviewType(..),
    FileProgressCallback,
    FileReadMoreCallback,
    fileFromPath,
    fileFromURI,
    fileFromCommandlineArg,
    fileFromParseName,
    fileEqual,
    fileBasename,
    filePath,
    fileURI,
    fileParseName,
    fileGetChild,
    fileGetChildForDisplayName,
    fileHasPrefix,
    fileGetRelativePath,
    fileResolveRelativePath,
    fileIsNative,
    fileHasURIScheme,
    fileURIScheme,
    fileRead,
    fileReadAsync,
    fileReadFinish,
    fileAppendTo,
    fileCreate,
    fileReplace,
    fileAppendToAsync,
    fileAppendToFinish,
    fileCreateAsync,
    fileCreateFinish,
    fileReplaceAsync,
    fileReplaceFinish,
    fileQueryInfo,
    fileQueryInfoAsync,
    fileQueryInfoFinish,
    fileQueryExists,
    fileQueryFilesystemInfo,
    fileQueryFilesystemInfoAsync,
    fileQueryFilesystemInfoFinish,
    fileQueryDefaultHandler,
    fileFindEnclosingMount,
    fileFindEnclosingMountAsync,
    fileFindEnclosingMountFinish,
    fileEnumerateChildren,
    fileEnumerateChildrenAsync,
    fileEnumerateChildrenFinish,
    fileSetDisplayName,
    fileSetDisplayNameAsync,
    fileSetDisplayNameFinish,
    fileDelete,
    fileTrash,
    fileCopy,
    fileCopyAsync,
    fileCopyFinish,
    fileMove,
    fileMakeDirectory,
#if GLIB_CHECK_VERSION(2,18,0)
    fileMakeDirectoryWithParents,
#endif
    fileMakeSymbolicLink,
    fileQuerySettableAttributes,
    fileQueryWritableNamespaces
    ) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Typeable

import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.UTFString

import System.GIO.Base
{#import System.GIO.Types#}
import System.GIO.FileAttribute

{# context lib = "gio" prefix = "g" #}

{# enum GFileQueryInfoFlags as FileQueryInfoFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags FileQueryInfoFlags

{# enum GFileCreateFlags as FileCreateFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags FileCreateFlags

{# enum GFileCopyFlags as FileCopyFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags FileCopyFlags

{# enum GFileMonitorFlags as FileMonitorFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags FileMonitorFlags

{# enum GFilesystemPreviewType as FilesystemPreviewType {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

type FileProgressCallback = Offset -> Offset -> IO ()
type CFileProgressCallback = {# type goffset #} -> {# type goffset #} -> Ptr () -> IO ()
foreign import ccall "wrapper"
    makeFileProgressCallback :: CFileProgressCallback
                             -> IO {# type GFileProgressCallback #}

marshalFileProgressCallback :: FileProgressCallback -> IO {# type GFileProgressCallback #}
marshalFileProgressCallback fileProgressCallback =
    makeFileProgressCallback cFileProgressCallback
    where cFileProgressCallback :: CFileProgressCallback
          cFileProgressCallback cCurrentNumBytes cTotalNumBytes _ = do
            fileProgressCallback (fromIntegral cCurrentNumBytes)
                                 (fromIntegral cTotalNumBytes)

type FileReadMoreCallback = BS.ByteString -> IO Bool

fileFromPath :: FilePath -> File
fileFromPath path =
    unsafePerformIO $ withUTFString path $ \cPath -> {# call file_new_for_path #} cPath >>= takeGObject

fileFromURI :: String -> File
fileFromURI uri =
    unsafePerformIO $ withUTFString uri $ \cURI -> {# call file_new_for_uri #} cURI >>= takeGObject

fileFromCommandlineArg :: String -> File
fileFromCommandlineArg arg =
    unsafePerformIO $ withUTFString arg $ \cArg -> {# call file_new_for_commandline_arg #} cArg >>= takeGObject

fileFromParseName :: String -> File
fileFromParseName parseName =
    unsafePerformIO $ withUTFString parseName $ \cParseName -> {# call file_parse_name #} cParseName >>= takeGObject

-- | Compare two file descriptors for equality. This test is also used to
--   implement the '(==)' function, that is, comparing two descriptions
--   will compare their content, not the pointers to the two structures.
--
fileEqual :: (FileClass file1, FileClass file2)
          => file1 -> file2 -> Bool
fileEqual file1 file2 =
    unsafePerformIO $ liftM toBool $ {# call file_equal #} (toFile file1) (toFile file2)

instance Eq File where
    (==) = fileEqual

fileBasename :: FileClass file => file -> String
fileBasename file =
    unsafePerformIO $ {# call file_get_basename #} (toFile file) >>= readUTFString

filePath :: FileClass file => file -> FilePath
filePath file =
    unsafePerformIO $ {# call file_get_path #} (toFile file) >>= readUTFString

fileURI :: FileClass file => file -> String
fileURI file =
    unsafePerformIO $ {# call file_get_uri #} (toFile file) >>= readUTFString

fileParseName :: FileClass file => file -> String
fileParseName file =
    unsafePerformIO $ {# call file_get_parse_name #} (toFile file) >>= readUTFString

fileParent :: FileClass file => file -> Maybe File
fileParent file =
    unsafePerformIO $ {# call file_get_parent #} (toFile file) >>= maybePeek takeGObject

fileGetChild :: FileClass file => file -> String -> File
fileGetChild file name =
    unsafePerformIO $
        withUTFString name $ \cName ->
        {# call file_get_child #} (toFile file) cName >>= takeGObject

fileGetChildForDisplayName :: FileClass file => file -> String -> Maybe File
fileGetChildForDisplayName file displayName =
    unsafePerformIO $
        withUTFString displayName $ \cDisplayName ->
        propagateGError ({# call file_get_child_for_display_name #} (toFile file) cDisplayName) >>=
        maybePeek takeGObject

fileHasPrefix :: (FileClass file1, FileClass file2) => file1 -> file2 -> Bool
fileHasPrefix file1 file2 =
    unsafePerformIO $
        liftM toBool $ {# call file_has_prefix #} (toFile file1) (toFile file2)

fileGetRelativePath :: (FileClass file1, FileClass file2) => file1 -> file2 -> Maybe FilePath
fileGetRelativePath file1 file2 =
    unsafePerformIO $
        {# call file_get_relative_path #} (toFile file1) (toFile file2) >>=
        maybePeek readUTFString

fileResolveRelativePath :: FileClass file => file -> FilePath -> Maybe File
fileResolveRelativePath file relativePath =
    unsafePerformIO $
        withUTFString relativePath $ \cRelativePath ->
        {# call file_resolve_relative_path #} (toFile file) cRelativePath >>=
        maybePeek takeGObject

fileIsNative :: FileClass file => file -> Bool
fileIsNative =
    unsafePerformIO .
        liftM toBool . {# call file_is_native #} . toFile

fileHasURIScheme :: FileClass file => file -> String -> Bool
fileHasURIScheme file uriScheme =
    unsafePerformIO $
        withUTFString uriScheme $ \cURIScheme ->
        liftM toBool $ {# call file_has_uri_scheme #} (toFile file) cURIScheme

fileURIScheme :: FileClass file => file -> String
fileURIScheme file =
    unsafePerformIO $ {# call file_get_uri_scheme #} (toFile file) >>= readUTFString

fileRead :: FileClass file => file -> Maybe Cancellable -> IO FileInputStream
fileRead file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
            propagateGError (g_file_read cFile cCancellable) >>= takeGObject
    where _ = {# call file_read #}

fileReadAsync :: FileClass file
              => file
              -> Int
              -> Maybe Cancellable
              -> AsyncReadyCallback
              -> IO ()
fileReadAsync file ioPriority mbCancellable callback =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject mbCancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_read_async cFile
                            (fromIntegral ioPriority)
                            cCancellable
                            cCallback
                            (castFunPtrToPtr cCallback)
    where _ = {# call file_read_async #}

fileReadFinish :: FileClass file
               => file
               -> AsyncResult
               -> IO FileInputStream
fileReadFinish file asyncResult =
    propagateGError ({# call file_read_finish #} (toFile file) asyncResult) >>= takeGObject

fileAppendTo :: FileClass file
             => file
             -> [FileCreateFlags]
             -> Maybe Cancellable
             -> IO FileOutputStream
fileAppendTo file flags cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_append_to cFile (cFromFlags flags) cCancellable) >>=
        takeGObject
    where _ = {# call file_append_to #}

fileCreate :: FileClass file
           => file
           -> [FileCreateFlags]
           -> Maybe Cancellable
           -> IO FileOutputStream
fileCreate file flags cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_create cFile (cFromFlags flags) cCancellable) >>=
        takeGObject
    where _ = {# call file_create #}

fileReplace :: FileClass file
            => file
            -> Maybe String
            -> Bool
            -> [FileCreateFlags]
            -> Maybe Cancellable
            -> IO FileOutputStream
fileReplace file etag makeBackup flags cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withUTFString etag $ \cEtag ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_replace cFile
                                        cEtag
                                        (fromBool makeBackup)
                                        (cFromFlags flags) cCancellable) >>=
        takeGObject
    where _ = {# call file_replace #}

fileAppendToAsync :: FileClass file
                  => file
                  -> [FileCreateFlags]
                  -> Int
                  -> Maybe Cancellable
                  -> AsyncReadyCallback
                  -> IO ()
fileAppendToAsync file flags ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_append_to_async cFile
                                 (cFromFlags flags)
                                 (fromIntegral ioPriority)
                                 cCancellable
                                 cCallback
                                 (castFunPtrToPtr cCallback)
    where _ = {# call file_append_to_async #}

fileAppendToFinish :: FileClass file
                   => file
                   -> AsyncResult
                   -> IO FileOutputStream
fileAppendToFinish file asyncResult =
    propagateGError ({# call file_append_to_finish #} (toFile file) asyncResult) >>= takeGObject

fileCreateAsync :: FileClass file
                  => file
                  -> [FileCreateFlags]
                  -> Int
                  -> Maybe Cancellable
                  -> AsyncReadyCallback
                  -> IO ()
fileCreateAsync file flags ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_create_async cFile
                              (cFromFlags flags)
                              (fromIntegral ioPriority)
                              cCancellable
                              cCallback
                              (castFunPtrToPtr cCallback)
    where _ = {# call file_create_async #}

fileCreateFinish :: FileClass file
                   => file
                   -> AsyncResult
                   -> IO FileOutputStream
fileCreateFinish file asyncResult =
    propagateGError ({# call file_create_finish #} (toFile file) asyncResult) >>= takeGObject

fileReplaceAsync :: FileClass file
                 => file
                 -> String
                 -> Bool
                 -> [FileCreateFlags]
                 -> Int
                 -> Maybe Cancellable
                 -> AsyncReadyCallback
                 -> IO ()
fileReplaceAsync file etag makeBackup flags ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        withUTFString etag $ \cEtag ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_replace_async cFile
                               cEtag
                               (fromBool makeBackup)
                               (cFromFlags flags)
                               (fromIntegral ioPriority)
                               cCancellable
                               cCallback
                               (castFunPtrToPtr cCallback)
    where _ = {# call file_replace_async #}

fileReplaceFinish :: FileClass file
                  => file
                  -> AsyncResult
                  -> IO FileOutputStream
fileReplaceFinish file asyncResult =
    propagateGError ({# call file_replace_finish #} (toFile file) asyncResult) >>= takeGObject

fileQueryInfo :: FileClass file
              => file
              -> String
              -> [FileQueryInfoFlags]
              -> Maybe Cancellable
              -> IO FileInfo
fileQueryInfo file attributes flags cancellable =
    withGObject (toFile file) $ \cFile ->
        withUTFString attributes $ \cAttributes ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_query_info cFile cAttributes (cFromFlags flags) cCancellable) >>=
        takeGObject
    where _ = {# call file_query_info #}

fileQueryInfoAsync :: FileClass file
                   => file
                   -> String
                   -> [FileQueryInfoFlags]
                   -> Int
                   -> Maybe Cancellable
                   -> AsyncReadyCallback
                   -> IO ()
fileQueryInfoAsync file attributes flags ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        withUTFString attributes $ \cAttributes ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_query_info_async cFile
                                  cAttributes
                                  (cFromFlags flags)
                                  (fromIntegral ioPriority)
                                  cCancellable
                                  cCallback
                                  (castFunPtrToPtr cCallback)
    where _ = {# call file_query_info_async #}

fileQueryInfoFinish :: FileClass file
                    => file
                    -> AsyncResult
                    -> IO FileInfo
fileQueryInfoFinish file asyncResult =
    propagateGError ({#call file_query_info_finish #} (toFile file) asyncResult) >>= takeGObject

fileQueryExists :: FileClass file
                => file
                -> Maybe Cancellable
                -> IO Bool
fileQueryExists file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
            liftM toBool $ g_file_query_exists cFile cCancellable
    where _ = {# call file_query_exists #}

fileQueryFilesystemInfo :: FileClass file
                        => file
                        -> String
                        -> Maybe Cancellable
                        -> IO FileInfo
fileQueryFilesystemInfo file attributes cancellable =
    withGObject (toFile file) $ \cFile ->
        withUTFString attributes $ \cAttributes ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_query_filesystem_info cFile cAttributes cCancellable) >>=
        takeGObject
    where _ = {# call file_query_filesystem_info #}

fileQueryFilesystemInfoAsync :: FileClass file
                             => file
                             -> String
                             -> Int
                             -> Maybe Cancellable
                             -> AsyncReadyCallback
                             -> IO ()
fileQueryFilesystemInfoAsync file attributes ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        withUTFString attributes $ \cAttributes ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_query_filesystem_info_async cFile
                                             cAttributes
                                             (fromIntegral ioPriority)
                                             cCancellable
                                             cCallback
                                             (castFunPtrToPtr cCallback)
    where _ = {# call file_query_filesystem_info_async #}

fileQueryFilesystemInfoFinish :: FileClass file
                              => file
                              -> AsyncResult
                              -> IO FileInfo
fileQueryFilesystemInfoFinish file asyncResult =
    propagateGError ({# call file_query_filesystem_info_finish #} (toFile file) asyncResult) >>=
        takeGObject

fileQueryDefaultHandler :: FileClass file
                        => file
                        -> Maybe Cancellable
                        -> IO AppInfo
fileQueryDefaultHandler file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_query_default_handler cFile cCancellable) >>=
        takeGObject
    where _ = {# call file_query_default_handler #}

fileFindEnclosingMount :: FileClass file
                       => file
                       -> Maybe Cancellable
                       -> IO Mount
fileFindEnclosingMount file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_find_enclosing_mount cFile cCancellable) >>=
        takeGObject
    where _ = {# call file_find_enclosing_mount #}

fileFindEnclosingMountAsync :: FileClass file
                            => file
                            -> Int
                            -> Maybe Cancellable
                            -> AsyncReadyCallback
                            -> IO ()
fileFindEnclosingMountAsync file ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_find_enclosing_mount_async cFile
                                            (fromIntegral ioPriority)
                                            cCancellable
                                            cCallback
                                            (castFunPtrToPtr cCallback)
    where _ = {# call file_find_enclosing_mount_async #}

fileFindEnclosingMountFinish :: FileClass file
                             => file
                             -> AsyncResult
                             -> IO Mount
fileFindEnclosingMountFinish file asyncResult =
    propagateGError ({# call file_find_enclosing_mount_finish #} (toFile file) asyncResult) >>=
        takeGObject

fileEnumerateChildren :: FileClass file
                      => file
                      -> String
                      -> [FileQueryInfoFlags]
                      -> Maybe Cancellable
                      -> IO FileEnumerator
fileEnumerateChildren file attributes flags cancellable =
    withGObject (toFile file) $ \cFile ->
        withUTFString attributes $ \cAttributes ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_enumerate_children cFile cAttributes (cFromFlags flags) cCancellable) >>=
        takeGObject
    where _ = {# call file_enumerate_children #}

fileEnumerateChildrenAsync :: FileClass file
                           => file
                           -> String
                           -> [FileQueryInfoFlags]
                           -> Int
                           -> Maybe Cancellable
                           -> AsyncReadyCallback
                           -> IO ()
fileEnumerateChildrenAsync file attributes flags ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        withUTFString attributes $ \cAttributes ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_enumerate_children_async cFile
                                          cAttributes
                                          (cFromFlags flags)
                                          (fromIntegral ioPriority)
                                          cCancellable
                                          cCallback
                                          (castFunPtrToPtr cCallback)
    where _ = {# call file_enumerate_children_async #}

fileEnumerateChildrenFinish :: FileClass file
                             => file
                             -> AsyncResult
                             -> IO FileEnumerator
fileEnumerateChildrenFinish file asyncResult =
    propagateGError ({# call file_enumerate_children_finish #} (toFile file) asyncResult) >>=
        takeGObject

fileSetDisplayName :: FileClass file
                   => file
                   -> String
                   -> Maybe Cancellable
                   -> IO File
fileSetDisplayName file displayName cancellable =
    withGObject (toFile file) $ \cFile ->
        withUTFString displayName $ \cDisplayName ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_set_display_name cFile cDisplayName cCancellable) >>=
        takeGObject
    where _ = {# call file_set_display_name #}

fileSetDisplayNameAsync :: FileClass file
                        => file
                        -> String
                        -> Int
                        -> Maybe Cancellable
                        -> AsyncReadyCallback
                        -> IO ()
fileSetDisplayNameAsync file displayName ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        withUTFString displayName $ \cDisplayName ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_set_display_name_async cFile
                                        cDisplayName
                                        (fromIntegral ioPriority)
                                        cCancellable
                                        cCallback
                                        (castFunPtrToPtr cCallback)
    where _ = {# call file_set_display_name_async #}

fileSetDisplayNameFinish :: FileClass file
                         => file
                         -> AsyncResult
                         -> IO File
fileSetDisplayNameFinish file asyncResult =
    propagateGError ({# call file_set_display_name_finish #} (toFile file) asyncResult) >>=
        takeGObject

fileDelete :: FileClass file
           => file
           -> Maybe Cancellable
           -> IO ()
fileDelete file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_delete cFile cCancellable) >> return ()
    where _ = {# call file_delete #}

fileTrash :: FileClass file
           => file
           -> Maybe Cancellable
           -> IO ()
fileTrash file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_trash cFile cCancellable) >> return ()
    where _ = {# call file_trash #}

fileCopy :: (FileClass source, FileClass destination)
         => source
         -> destination
         -> [FileCopyFlags]
         -> Maybe Cancellable
         -> Maybe FileProgressCallback
         -> IO Bool
fileCopy source destination flags cancellable progressCallback =
    withGObject (toFile source) $ \cSource ->
        withGObject (toFile destination) $ \cDestination ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cProgressCallback <- maybe (return nullFunPtr) marshalFileProgressCallback progressCallback
          propagateGError $ \cError -> do
            ret <- g_file_copy cSource
                               cDestination
                               (cFromFlags flags)
                               cCancellable
                               cProgressCallback
                               nullPtr
                               cError
            when (cProgressCallback /= nullFunPtr) $
              freeHaskellFunPtr cProgressCallback
            return $ toBool ret
    where _ = {# call file_copy #}

fileCopyAsync :: (FileClass source, FileClass destination)
              => source
              -> destination
              -> [FileCopyFlags]
              -> Int
              -> Maybe Cancellable
              -> Maybe FileProgressCallback
              -> AsyncReadyCallback
              -> IO ()
fileCopyAsync source destination flags ioPriority cancellable progressCallback callback =
    withGObject (toFile source) $ \cSource ->
        withGObject (toFile destination) $ \cDestination ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cProgressCallback <- maybe (return nullFunPtr) marshalFileProgressCallback progressCallback
          cCallback <- marshalAsyncReadyCallback $ \sourceObject res -> do
                         when (cProgressCallback /= nullFunPtr) $
                           freeHaskellFunPtr cProgressCallback
                         callback sourceObject res
          g_file_copy_async cSource
                            cDestination
                            (cFromFlags flags)
                            (fromIntegral ioPriority)
                            cCancellable
                            cProgressCallback
                            nullPtr
                            cCallback
                            (castFunPtrToPtr cCallback)
    where _ = {# call file_copy_async #}

fileCopyFinish :: FileClass file
               => file
               -> AsyncResult
               -> IO Bool
fileCopyFinish file asyncResult =
    liftM toBool $ propagateGError ({# call file_copy_finish #} (toFile file) asyncResult)

fileMove :: (FileClass source, FileClass destination)
         => source
         -> destination
         -> [FileCopyFlags]
         -> Maybe Cancellable
         -> Maybe FileProgressCallback
         -> IO Bool
fileMove source destination flags cancellable progressCallback =
    withGObject (toFile source) $ \cSource ->
        withGObject (toFile destination) $ \cDestination ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cProgressCallback <- maybe (return nullFunPtr) marshalFileProgressCallback progressCallback
          propagateGError $ \cError -> do
            ret <- g_file_move cSource
                               cDestination
                               (cFromFlags flags)
                               cCancellable
                               cProgressCallback
                               nullPtr
                               cError
            when (cProgressCallback /= nullFunPtr) $
              freeHaskellFunPtr cProgressCallback
            return $ toBool ret
    where _ = {# call file_move #}

fileMakeDirectory :: FileClass file
                  => file
                  -> Maybe Cancellable
                  -> IO ()
fileMakeDirectory file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          propagateGError $ g_file_make_directory cFile cCancellable
          return ()
    where _ = {# call file_make_directory #}

#if GLIB_CHECK_VERSION(2,18,0)
fileMakeDirectoryWithParents :: FileClass file
                             => file
                             -> Maybe Cancellable
                             -> IO ()
fileMakeDirectoryWithParents file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          propagateGError $ g_file_make_directory_with_parents cFile cCancellable
          return ()
    where _ = {# call file_make_directory_with_parents #}
#endif

fileMakeSymbolicLink :: FileClass file
                     => file
                     -> String
                     -> Maybe Cancellable
                     -> IO ()
fileMakeSymbolicLink file symlinkValue cancellable =
    withGObject (toFile file) $ \cFile ->
        withUTFString symlinkValue $ \cSymlinkValue ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          propagateGError $ g_file_make_symbolic_link cFile cSymlinkValue cCancellable
          return ()
    where _ = {# call file_make_symbolic_link #}

{# pointer *FileAttributeInfoList newtype #}
takeFileAttributeInfoList :: Ptr FileAttributeInfoList
                          -> IO [FileAttributeInfo]
takeFileAttributeInfoList ptr =
    do cInfos <- liftM castPtr $ {# get FileAttributeInfoList->infos #} ptr
       cNInfos <- {# get FileAttributeInfoList->n_infos #} ptr
       infos <- peekArray (fromIntegral cNInfos) cInfos
       g_file_attribute_info_list_unref ptr
       return infos
    where _ = {# call file_attribute_info_list_unref #}

fileQuerySettableAttributes :: FileClass file
                            => file
                            -> Maybe Cancellable
                            -> IO [FileAttributeInfo]
fileQuerySettableAttributes file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          ptr <- propagateGError $ g_file_query_settable_attributes cFile cCancellable
          infos <- takeFileAttributeInfoList ptr
          return infos
    where _ = {# call file_query_settable_attributes #}

fileQueryWritableNamespaces :: FileClass file
                            => file
                            -> Maybe Cancellable
                            -> IO [FileAttributeInfo]
fileQueryWritableNamespaces file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          ptr <- propagateGError $ g_file_query_writable_namespaces cFile cCancellable
          infos <- takeFileAttributeInfoList ptr
          return infos
    where _ = {# call file_query_writable_namespaces #}

