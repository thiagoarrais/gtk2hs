-- -*-haskell-*-
--  GIMP Toolkit (GTK) Interface Editable
--
--  Author : Axel Simon, Duncan Coutts
--
--  Created: 30 July 2004
--
--  Version $Revision: 1.11 $ from $Date: 2005/12/07 12:57:37 $
--
--  Copyright (C) 1999-2005 Axel Simon, Duncan Coutts
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
-- Interface for text-editing widgets
--
module Graphics.UI.Gtk.Entry.Editable (
-- * Detail
-- 
-- | The 'Editable' interface is an interface which should be implemented by
-- text editing widgets, such as 'Entry'.
-- It contains functions for generically manipulating an editable
-- widget, a large number of action signals used for key bindings, and several
-- signals that an application can connect to to modify the behavior of a
-- widget.
--
-- As an example of the latter usage, by connecting the following handler to
-- \"insert_text\", an application can convert all entry into a widget into
-- uppercase.

-- * Class Hierarchy
-- |
-- @
-- |  GInterface
-- |   +----Editable
-- @

-- * Types
  Editable,
  EditableClass,
  castToEditable,
  toEditable,

-- * Methods
  editableSelectRegion,
  editableGetSelectionBounds,
  editableInsertText,
  editableDeleteText,
  editableGetChars,
  editableCutClipboard,
  editableCopyClipboard,
  editablePasteClipboard,
  editableDeleteSelection,
  editableSetEditable,
  editableGetEditable,
  editableSetPosition,
  editableGetPosition,

-- * Attributes
  editablePosition,
  editableEditable,

-- * Signals
  onEditableChanged,
  afterEditableChanged,
  onDeleteText,
  afterDeleteText,
  stopDeleteText,
  onInsertText,
  afterInsertText,
  stopInsertText
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Selects a region of text. The characters that are selected are those
-- characters at positions from @startPos@ up to, but not including @endPos@.
-- If @endPos@ is negative, then the the characters selected will be those
-- characters from @startPos@ to the end of the text.
--
-- Calling this function with @start@=1 and @end@=4 it will mark \"ask\" in
-- the string \"Haskell\". (FIXME: verify)
--
editableSelectRegion :: EditableClass self => self
 -> Int   -- ^ @start@ - the starting position.
 -> Int   -- ^ @end@ - the end position.
 -> IO ()
editableSelectRegion self start end =
  {# call editable_select_region #}
    (toEditable self)
    (fromIntegral start)
    (fromIntegral end)

-- | Gets the current selection bounds, if there is a selection.
--
editableGetSelectionBounds :: EditableClass self => self
 -> IO (Int,Int) -- ^ @(start, end)@ - the starting and end positions. This
                 -- pair is not ordered. The @end@ index represents the
                 -- position of the cursor. The @start@ index is the other end
                 -- of the selection. If both numbers are equal there is in
                 -- fact no selection.
editableGetSelectionBounds self =
  alloca $ \startPtr ->
  alloca $ \endPtr -> do
  {# call unsafe editable_get_selection_bounds #}
    (toEditable self)
    startPtr
    endPtr
  start <- liftM fromIntegral $ peek startPtr
  end	<- liftM fromIntegral $ peek endPtr
  return (start,end)

-- | Inserts text at a given position.
--
editableInsertText :: EditableClass self => self
 -> String -- ^ @newText@ - the text to insert.
 -> Int    -- ^ @position@ - the position at which to insert the text.
 -> IO Int -- ^ returns the position after the newly inserted text.
editableInsertText self newText position = 
  with (fromIntegral position) $ \positionPtr ->
  withUTFStringLen newText $ \(newTextPtr, newTextLength) -> do
  {# call editable_insert_text #}
    (toEditable self)
    newTextPtr
    (fromIntegral newTextLength)
    positionPtr
  position <- peek positionPtr
  return (fromIntegral position)

-- | Deletes a sequence of characters. The characters that are deleted are
-- those characters at positions from @startPos@ up to, but not including
-- @endPos@. If @endPos@ is negative, then the the characters deleted will be
-- those characters from @startPos@ to the end of the text.
--
editableDeleteText :: EditableClass self => self
 -> Int   -- ^ @startPos@ - the starting position.
 -> Int   -- ^ @endPos@ - the end position.
 -> IO ()
editableDeleteText self startPos endPos =
  {# call editable_delete_text #}
    (toEditable self)
    (fromIntegral startPos)
    (fromIntegral endPos)

-- | Retrieves a sequence of characters. The characters that are retrieved are
-- those characters at positions from @startPos@ up to, but not including
-- @endPos@. If @endPos@ is negative, then the the characters retrieved will be
-- those characters from @startPos@ to the end of the text.
--
editableGetChars :: EditableClass self => self
 -> Int       -- ^ @startPos@ - the starting position.
 -> Int       -- ^ @endPos@ - the end position.
 -> IO String -- ^ returns the characters in the indicated region.
editableGetChars self startPos endPos =
  {# call unsafe editable_get_chars #}
    (toEditable self)
    (fromIntegral startPos)
    (fromIntegral endPos)
  >>= readUTFString

-- | Causes the characters in the current selection to be copied to the
-- clipboard and then deleted from the widget.
--
editableCutClipboard :: EditableClass self => self -> IO ()
editableCutClipboard self =
  {# call editable_cut_clipboard #}
    (toEditable self)

-- | Causes the characters in the current selection to be copied to the
-- clipboard.
--
editableCopyClipboard :: EditableClass self => self -> IO ()
editableCopyClipboard self =
  {# call editable_copy_clipboard #}
    (toEditable self)

-- | Causes the contents of the clipboard to be pasted into the given widget
-- at the current cursor position.
--
editablePasteClipboard :: EditableClass self => self -> IO ()
editablePasteClipboard self =
  {# call editable_paste_clipboard #}
    (toEditable self)

-- | Deletes the current contents of the widgets selection and disclaims the
-- selection.
--
editableDeleteSelection :: EditableClass self => self -> IO ()
editableDeleteSelection self =
  {# call editable_delete_selection #}
    (toEditable self)

-- | Sets the cursor position.
--
editableSetPosition :: EditableClass self => self
 -> Int   -- ^ @position@ - the position of the cursor. The cursor is
          -- displayed before the character with the given (base 0) index in
          -- the widget. The value must be less than or equal to the number of
          -- characters in the widget. A value of -1 indicates that the
          -- position should be set after the last character in the entry.
 -> IO ()
editableSetPosition self position =
  {# call editable_set_position #}
    (toEditable self)
    (fromIntegral position)

-- | Retrieves the current cursor position.
--
editableGetPosition :: EditableClass self => self
 -> IO Int -- ^ returns the position of the cursor. The cursor is displayed
           -- before the character with the given (base 0) index in the widget.
           -- The value will be less than or equal to the number of characters
           -- in the widget. Note that this position is in characters, not in
           -- bytes.
editableGetPosition self =
  liftM fromIntegral $
  {# call unsafe editable_get_position #}
    (toEditable self)

-- | Determines if the user can edit the text in the editable widget or not.
--
editableSetEditable :: EditableClass self => self
 -> Bool  -- ^ @isEditable@ - @True@ if the user is allowed to edit the text
          -- in the widget.
 -> IO ()
editableSetEditable self isEditable =
  {# call editable_set_editable #}
    (toEditable self)
    (fromBool isEditable)

-- | Retrieves whether the text is editable. See 'editableSetEditable'.
--
editableGetEditable :: EditableClass self => self -> IO Bool
editableGetEditable self =
  liftM toBool $
  {# call editable_get_editable #}
    (toEditable self)

--------------------
-- Attributes

-- | \'position\' property. See 'editableGetPosition' and
-- 'editableSetPosition'
--
editablePosition :: EditableClass self => Attr self Int
editablePosition = newAttr
  editableGetPosition
  editableSetPosition

-- | \'editable\' property. See 'editableGetEditable' and
-- 'editableSetEditable'
--
editableEditable :: EditableClass self => Attr self Bool
editableEditable = newAttr
  editableGetEditable
  editableSetEditable

--------------------
-- Signals

-- | Emitted when the settings of the 'Editable' widget changes.
--
onEditableChanged, afterEditableChanged :: EditableClass ec => ec -> IO () ->
                                     IO (ConnectId ec)
onEditableChanged = connect_NONE__NONE "changed" False
afterEditableChanged = connect_NONE__NONE "changed" True

-- | Emitted when a piece of text is deleted from the 'Editable' widget.
--
-- * See 'onInsertText' for information on how to use this signal.
--
onDeleteText, afterDeleteText :: EditableClass self => self
 -> (Int -> Int -> IO ()) -- ^ @(\startPos endPos -> ...)@
 -> IO (ConnectId self)
onDeleteText = connect_INT_INT__NONE "delete_text" False
afterDeleteText = connect_INT_INT__NONE "delete_text" True

-- | Stop the current signal that deletes text.
stopDeleteText :: EditableClass self => ConnectId self -> IO ()
stopDeleteText (ConnectId _ obj) =
  signalStopEmission obj "delete_text"

-- | Emitted when a piece of text is inserted into the 'Editable' widget.
--
-- * The connected signal receives the text that is inserted, together with
--   the position in the entry widget. The return value should be the position
--   in the entry widget that lies past the recently inserted text (i.e.
--   you should return the given position plus the length of the string).
--
-- * To modify the text that the user inserts, you need to connect to this
--   signal, modify the text the way you want and then call
--   'editableInsertText'. To avoid that this signal handler is called
--   recursively, you need to temporarily block it using
--   'System.Glib.Signals.signalBlock'. After the default signal
--   handler has inserted your modified text, it is important that you
--   prevent the default handler from being executed again when this signal
--   handler returns. To stop the current signal, use 'stopInsertText'.
--   The following code is an example of how to turn all input into uppercase:
--
--   > idRef <- newIORef undefined
--   > id <- onInsertText entry $ \str pos -> do
--   >   id <- readIORef idRef
--   >   signalBlock id
--   >   pos' <- editableInsertText entry (map toUpper str) pos
--   >   signalUnblock id
--   >   stopInsertText id
--   >   return pos'
--   > writeIORef idRef id
--
--   Note that the 'afterInsertText' function is not very useful, except to
--   track editing actions.
--
onInsertText, afterInsertText :: EditableClass self => self
 -> (String -> Int -> IO Int)
 -> IO (ConnectId self)
onInsertText obj handler =
  connect_PTR_INT_PTR__NONE "insert_text" False obj
  (\strPtr strLen posPtr -> do
    str <- if strLen<0 then peekUTFString strPtr
	   else peekUTFStringLen (strPtr, strLen)
    pos <- peek posPtr
    pos' <- handler str pos
    poke posPtr pos'
  )
afterInsertText obj handler =
  connect_PTR_INT_PTR__NONE "insert_text" True obj
  (\strPtr strLen posPtr -> do
    str <- if strLen<0 then peekUTFString strPtr
	   else peekUTFStringLen (strPtr, strLen)
    pos <- peek posPtr
    pos' <- handler str pos
    poke posPtr pos'
  )

-- | Stop the current signal that inserts text.
stopInsertText :: EditableClass self => ConnectId self -> IO ()
stopInsertText (ConnectId _ obj) =
  signalStopEmission obj "insert_text"
