-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceBuffer
--
--  Author : Duncan Coutts
--  derived from GtkTextView bindings by Axel Simon
--
--  Created: 15 October 2003
--
--  Version $Revision: 1.3 $ from $Date: 2005/09/20 00:05:34 $
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
module Graphics.UI.Gtk.SourceView.SourceBuffer (
  SourceBuffer,
  SourceBufferClass,
  castToSourceBuffer,
  sourceBufferNew,
  sourceBufferNewWithLanguage,
  sourceBufferSetCheckBrackets,
  sourceBufferGetCheckBrackets,
  sourceBufferSetBracketsMatchStyle,
  sourceBufferSetHighlight,
  sourceBufferGetHighlight,
  sourceBufferSetMaxUndoLevels,
  sourceBufferGetMaxUndoLevels,
  sourceBufferSetLanguage,
  sourceBufferGetLanguage,
  sourceBufferSetEscapeChar,
  sourceBufferGetEscapeChar,
  sourceBufferCanUndo,
  sourceBufferCanRedo,
  sourceBufferUndo,
  sourceBufferRedo,
  sourceBufferBeginNotUndoableAction,
  sourceBufferEndNotUndoableAction,
  sourceBufferCreateMarker,
  sourceBufferMoveMarker,
  sourceBufferDeleteMarker,
  sourceBufferGetMarker,
  sourceBufferGetMarkersInRegion,
  sourceBufferGetFirstMarker,
  sourceBufferGetLastMarker,
  sourceBufferGetIterAtMarker,
  sourceBufferGetNextMarker,
  sourceBufferGetPrevMarker
) where

import Monad	(liftM)
import Maybe    (fromMaybe)

import System.Glib.FFI
import System.Glib.GList		(fromGSList)
import System.Glib.GObject              (makeNewGObject)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.SourceView.SourceTagStyle
import Graphics.UI.Gtk.SourceView.SourceMarker
{#import Graphics.UI.Gtk.Multiline.TextIter#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new 'SourceBuffer', possibly
-- taking a 'SourceTagTable'.
--
sourceBufferNew :: Maybe SourceTagTable -> IO SourceBuffer
sourceBufferNew tt = makeNewGObject mkSourceBuffer $
  {#call unsafe source_buffer_new#} 
  (fromMaybe (mkSourceTagTable nullForeignPtr) tt)

-- | Create a new 'SourceBuffer'
-- with a 'SourceLanguage'.
--
sourceBufferNewWithLanguage :: SourceLanguage -> IO SourceBuffer
sourceBufferNewWithLanguage lang = makeNewGObject mkSourceBuffer $
  {#call unsafe source_buffer_new_with_language#} lang

-- | 
--
sourceBufferSetCheckBrackets :: SourceBuffer -> Bool -> IO ()
sourceBufferSetCheckBrackets sb newVal =
  {#call unsafe source_buffer_set_check_brackets#} sb (fromBool newVal)
  
-- | 
--
sourceBufferGetCheckBrackets :: SourceBuffer -> IO Bool 
sourceBufferGetCheckBrackets sb = liftM toBool $
  {#call unsafe source_buffer_get_check_brackets#} sb

-- | 
--
sourceBufferSetBracketsMatchStyle :: SourceBuffer -> SourceTagStyle -> IO () 
sourceBufferSetBracketsMatchStyle sb ts =
  alloca $ \tsPtr -> do
  poke tsPtr ts
  {#call unsafe source_buffer_set_bracket_match_style#} sb (castPtr tsPtr)

-- | 
--
sourceBufferSetHighlight :: SourceBuffer -> Bool -> IO ()
sourceBufferSetHighlight sb newVal =
  {#call unsafe source_buffer_set_highlight#} sb (fromBool newVal)
  
-- | 
--
sourceBufferGetHighlight :: SourceBuffer -> IO Bool 
sourceBufferGetHighlight sb = liftM toBool $
  {#call unsafe source_buffer_get_highlight#} sb

-- | 
--
sourceBufferSetMaxUndoLevels :: SourceBuffer -> Int -> IO ()
sourceBufferSetMaxUndoLevels sb newVal =
  {#call unsafe source_buffer_set_max_undo_levels#} sb (fromIntegral newVal)
  
-- | 
--
sourceBufferGetMaxUndoLevels :: SourceBuffer -> IO Int
sourceBufferGetMaxUndoLevels sb = liftM fromIntegral $
  {#call unsafe source_buffer_get_max_undo_levels#} sb

-- | 
--
sourceBufferSetLanguage :: SourceBuffer -> SourceLanguage -> IO ()
sourceBufferSetLanguage sb lang =
  {#call unsafe source_buffer_set_language#} sb lang
  
-- | 
--
sourceBufferGetLanguage :: SourceBuffer -> IO SourceLanguage
sourceBufferGetLanguage sb = makeNewGObject mkSourceLanguage $
  {#call unsafe source_buffer_get_language#} sb

-- | 
--
sourceBufferSetEscapeChar :: SourceBuffer -> Char -> IO ()
sourceBufferSetEscapeChar sb char =
  {#call unsafe source_buffer_set_escape_char#} sb ((toEnum . fromEnum) char)
  
-- | 
--
sourceBufferGetEscapeChar :: SourceBuffer -> IO Char
sourceBufferGetEscapeChar sb = liftM (toEnum . fromEnum) $
  {#call unsafe source_buffer_get_escape_char#} sb

-- | 
--
sourceBufferCanUndo :: SourceBuffer -> IO Bool
sourceBufferCanUndo sb = liftM toBool $
  {#call unsafe source_buffer_can_undo#} sb
  
-- | 
--
sourceBufferCanRedo :: SourceBuffer -> IO Bool
sourceBufferCanRedo sb = liftM toBool $
  {#call unsafe source_buffer_can_redo#} sb

-- | 
--
sourceBufferUndo :: SourceBuffer -> IO ()
sourceBufferUndo sb =
  {#call source_buffer_undo#} sb
  
-- | 
--
sourceBufferRedo :: SourceBuffer -> IO ()
sourceBufferRedo sb =
  {#call source_buffer_redo#} sb

-- | 
--
sourceBufferBeginNotUndoableAction :: SourceBuffer -> IO ()
sourceBufferBeginNotUndoableAction sb =
  {#call source_buffer_begin_not_undoable_action#} sb
  
-- | 
--
sourceBufferEndNotUndoableAction :: SourceBuffer -> IO ()
sourceBufferEndNotUndoableAction sb =
  {#call source_buffer_end_not_undoable_action#} sb

-- | 
--
sourceBufferCreateMarker :: SourceBuffer -> String -> String -> TextIter -> IO SourceMarker
sourceBufferCreateMarker sb name markerType iter =
  makeNewGObject mkSourceMarker $
  withCString name       $ \strPtr1 ->
  withCString markerType $ \strPtr2 ->
  {#call source_buffer_create_marker#} sb strPtr1 strPtr2 iter

-- | 
--
sourceBufferMoveMarker :: SourceBuffer -> SourceMarker -> TextIter -> IO ()
sourceBufferMoveMarker sb mark iter =
  {#call source_buffer_move_marker#} sb mark iter

-- | 
--
sourceBufferDeleteMarker :: SourceBuffer -> SourceMarker -> IO ()
sourceBufferDeleteMarker sb mark =
  {#call source_buffer_delete_marker#} sb mark

-- | 
--
sourceBufferGetMarker :: SourceBuffer -> String -> IO SourceMarker
sourceBufferGetMarker sb name =
  makeNewGObject mkSourceMarker $
  withCString name $ \strPtr1 ->
  {#call unsafe source_buffer_get_marker#} sb strPtr1

-- | 
--
sourceBufferGetMarkersInRegion :: SourceBuffer -> TextIter -> TextIter -> IO [SourceMarker]
sourceBufferGetMarkersInRegion sb begin end = do
  gList <- {#call unsafe source_buffer_get_markers_in_region#} sb begin end
  wList <- fromGSList gList
  mapM (makeNewGObject mkSourceMarker) (map return wList)

-- | 
--
sourceBufferGetFirstMarker :: SourceBuffer -> IO SourceMarker
sourceBufferGetFirstMarker sb =
  makeNewGObject mkSourceMarker $
  {#call unsafe source_buffer_get_first_marker#} sb

-- | 
--
sourceBufferGetLastMarker :: SourceBuffer -> IO SourceMarker
sourceBufferGetLastMarker sb =
  makeNewGObject mkSourceMarker $
  {#call unsafe source_buffer_get_last_marker#} sb

-- | 
--
sourceBufferGetIterAtMarker :: SourceBuffer -> SourceMarker -> IO TextIter
sourceBufferGetIterAtMarker sb mark = do
  iter <- makeEmptyTextIter
  {#call unsafe source_buffer_get_iter_at_marker#} sb iter mark
  return iter

-- | 
--
sourceBufferGetNextMarker :: SourceBuffer -> TextIter -> IO (Maybe SourceMarker)
sourceBufferGetNextMarker sb iter = do
  markPtr <- {#call unsafe source_buffer_get_next_marker#} sb iter
  if markPtr==nullPtr then return Nothing
                      else liftM Just $ makeNewGObject mkSourceMarker (return markPtr)

-- | 
--
sourceBufferGetPrevMarker :: SourceBuffer -> TextIter -> IO (Maybe SourceMarker)
sourceBufferGetPrevMarker sb iter = do
  markPtr <- {#call unsafe source_buffer_get_prev_marker#} sb iter
  if markPtr==nullPtr then return Nothing
                      else liftM Just $ makeNewGObject mkSourceMarker (return markPtr)
