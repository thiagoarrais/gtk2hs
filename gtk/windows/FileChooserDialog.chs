-- -*-haskell-*-
-- |GIMP Toolkit (GTK) @entry Widget FileChooserDialog@
--
--  Author : Duncan Coutts
--  Created: 24 April 2004
--
--  Copyright (c) 2004 Duncan Coutts
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
--- Description ---------------------------------------------------------------
--
--  Language: Haskell 98 Binding Module
--
--  The file chooser dialog & widget is a replacement (introduced with gtk+ 2.4)
--  for the old and ugly GtkFileSelection dialog. It provides a better user
--  interface and an improved API
--
--  This is the dialog variant of the FileChooser
--

module FileChooserDialog (
  FileChooserDialogClass,
  FileChooserDialog,
  fileChooserDialogNew,
  fileChooserDialogNewWithBackend
) where

import Monad (liftM, when)
import Maybe (isJust, fromJust)
import FFI
{#import Hierarchy#}
{#import FileChooser#}
import GObject (objectNew)
import Object (makeNewObject)
import Window
import Dialog
import GValue
import StoreValue

{# context lib="gtk" prefix ="gtk" #}

-- The FileChooserDialog implements the FileChooser interface
-- which we model in Haskell as another instance decleration
instance FileChooserClass FileChooserDialog

fileChooserDialogNew
  :: Maybe String ->           -- Title of the dialog (or default)
     Maybe Window ->           -- Transient parent of the dialog (or none)
     FileChooserAction ->      -- Open or save mode for the dialog
     [(String, ResponseId)] -> -- buttons and their response codes
     IO FileChooserDialog
fileChooserDialogNew title parent action buttons =
  internalFileChooserDialogNew title parent action buttons Nothing


fileChooserDialogNewWithBackend
  :: Maybe String ->           -- Title of the dialog (or default)
     Maybe Window ->           -- Transient parent of the dialog (or none)
     FileChooserAction ->      -- Open or save mode for the dialog
     [(String, ResponseId)] -> -- buttons and their response codes
     String ->                 -- The name of the specific filesystem backend to use
     IO FileChooserDialog
fileChooserDialogNewWithBackend title parent action buttons backend =
  internalFileChooserDialogNew title parent action buttons (Just backend)
  

-- Annoyingly, the constructor for FileChooserDialog uses varargs so we can't
-- call it using the Haskell FFI. The GTK people do not consider this an api
-- bug, see http://bugzilla.gnome.org/show_bug.cgi?id=141004
-- The solution is to call objectNew and add the buttons manually.

internalFileChooserDialogNew :: Maybe String ->           -- Title of the dialog (or default)
                                Maybe Window ->           -- Transient parent of the dialog (or none)
                                FileChooserAction ->      -- Open or save mode for the dialog
                                [(String, ResponseId)] -> -- buttons and their response codes
				Maybe String ->           -- The name of the backend to use (optional)
                                IO FileChooserDialog
internalFileChooserDialogNew title parent action buttons backend = do
  objType <- {# call unsafe gtk_file_chooser_dialog_get_type #}
  dialog <-makeNewObject mkFileChooserDialog $ liftM castPtr $
           if (isJust backend)
             then with (GVstring backend) $ \backendGValue ->
                  objectNew objType [("file-system-backend", backendGValue)]
             else objectNew objType []
  when (isJust title)
       (dialog `windowSetTitle` fromJust title)
  when (isJust parent)
       (dialog `windowSetTransientFor` fromJust parent)
  dialog `fileChooserSetAction` action
  mapM_ (\(btnName, btnResponse) ->
          dialogAddButton dialog btnName btnResponse) buttons
  return dialog
