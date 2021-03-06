-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceLanguagesManager
--
--  Author : Duncan Coutts
--  derived from GtkTextView bindings by Axel Simon
--
--  Created: 14 October 2003
--
--  Copyright (C) 2003-2005 Duncan Coutts, Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
module Graphics.UI.Gtk.SourceView.SourceLanguagesManager (
  SourceLanguagesManager,
  castToSourceLanguagesManager,
  sourceLanguagesManagerNew,
  sourceLanguagesManagerGetAvailableLanguages,
  sourceLanguagesManagerGetLanguageFromMimeType,
  sourceLanguagesManagerGetLangFilesDirs
  ) where

import Control.Monad	(liftM, mapM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList        (readGSList)
import System.Glib.GObject	(makeNewGObject, constructNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}


-- methods

-- | Create a new 'SourceLanguagesManager'.
--
sourceLanguagesManagerNew :: IO SourceLanguagesManager
sourceLanguagesManagerNew = constructNewGObject mkSourceLanguagesManager
  {#call source_languages_manager_new#} 

-- | Gets a list of available languages for the given language manager.
-- 
sourceLanguagesManagerGetAvailableLanguages :: SourceLanguagesManager -> 
					       IO [SourceLanguage]
sourceLanguagesManagerGetAvailableLanguages lm = do
  gList <- {#call source_languages_manager_get_available_languages#} lm
  wList <- readGSList gList
  mapM (makeNewGObject mkSourceLanguage) (map return wList)

-- | Gets the 'SourceLanguage' which is associated with the given mime type
--   in the language manager.
-- 
sourceLanguagesManagerGetLanguageFromMimeType :: SourceLanguagesManager -> String -> IO (Maybe SourceLanguage)
sourceLanguagesManagerGetLanguageFromMimeType lm mimeType = do
  langPtr <- withCString mimeType $ \strPtr ->
             {#call source_languages_manager_get_language_from_mime_type#} lm strPtr
  if langPtr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkSourceLanguage (return langPtr)

-- | Retrieve filenames with language specifications.
-- 
sourceLanguagesManagerGetLangFilesDirs :: SourceLanguagesManager -> 
					  IO [FilePath]
sourceLanguagesManagerGetLangFilesDirs lm = do
  gsList <- {#call source_languages_manager_get_lang_files_dirs#} lm
  -- The returned structure is private and nothing is to be freed.
  dirList <- readGSList gsList
  mapM peekUTFString dirList

