-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Label
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--
--  Created: 2 May 2001
--
--  Version $Revision: 1.7 $ from $Date: 2005/04/02 19:38:29 $
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- A widget that displays a small to medium amount of text
--
module Graphics.UI.Gtk.Display.Label (
-- * Detail
-- 
-- | The 'Label' widget displays a small amount of text. As the name implies,
-- most labels are used to label another widget such as a 'Button', a
-- 'MenuItem', or a 'OptionMenu'.

-- ** Mnemonics
-- 
-- | Labels may contain mnemonics. Mnemonics are underlined characters in the
-- label, used for keyboard navigation. Mnemonics are created by providing a
-- string with an underscore before the mnemonic character, such as
-- @\"_File\"@, to the functions 'labelNewWithMnemonic' or
-- 'labelSetTextWithMnemonic'.
--
-- Mnemonics automatically activate any activatable widget the label is
-- inside, such as a 'Button'; if the label is not inside the mnemonic's target
-- widget, you have to tell the label about the target using
-- 'labelSetMnemonicWidget'. Here's a simple example where the label is inside
-- a button: There's a convenience function to create buttons with a mnemonic
-- label already inside: To create a mnemonic for a widget alongside the label,
-- such as a 'Entry', you have to point the label at the entry with
-- 'labelSetMnemonicWidget':
-- 
-- >   -- Pressing Alt+H will activate this button
-- >   button <- buttonNew
-- >   label <- labelNewWithMnemonic "_Hello"
-- >   containerAdd button label
--
-- >   -- Pressing Alt+H will activate this button
-- >   button <- buttonNewWithMnemonic "_Hello"
--
-- >   -- Pressing Alt+H will focus the entry
-- >   entry <- entryNew
-- >   label <- labelNewWithMnemonic "_Hello"
-- >   labelSetMnemonicWidget label entry

-- ** Markup (styled text)
-- 
-- | To make it easy to format text in a label (changing colors, fonts, etc.),
-- label text can be provided in a simple markup format. Here's how to create a
-- label with a small font: (See complete documentation of available tags in
-- the Pango manual.)
--
-- >   label <- labelNew Nothing
-- >   labelSetMarkup label "<small>Small text</small>"
--
-- The markup passed to 'labelSetMarkup' must be valid; for example, literal
-- \<\/>\/& characters must be escaped as @\"&lt;\"@, @\"&gt;\"@, and
-- @\"&amp;@\". If you pass
-- text obtained from the user, file, or a network to 'labelSetMarkup', you\'ll
-- want to escape it with 'gMarkupEscapeText'.

-- ** Selectable labels
-- 
-- | Labels can be made selectable with 'labelSetSelectable'. Selectable
-- labels allow the user to copy the label contents to the clipboard. Only
-- labels that contain useful-to-copy information - such as error messages -
-- should be made selectable.

-- ** Text layout
-- 
-- | A label can contain any number of paragraphs, but will have performance
-- problems if it contains more than a small number. Paragraphs are separated
-- by newlines or other paragraph separators understood by Pango.
--
-- Labels can automatically wrap text if you call 'labelSetLineWrap'.
--
-- 'labelSetJustify' sets how the lines in a label align with one another.
-- If you want to set how the label as a whole aligns in its available space,
-- see 'miscSetAlignment'.
-- 

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Misc'
-- |                     +----Label
-- |                           +----'AccelLabel'
-- |                           +----'TipsQuery'
-- @

-- * Types
  Label,
  LabelClass,
  castToLabel,

-- * Constructors
  labelNew,
  labelNewWithMnemonic,

-- * Methods
  labelSetText,
  labelSetLabel,
  labelSetTextWithMnemonic,
  labelSetMarkup,
  labelSetMarkupWithMnemonic,
  labelSetMnemonicWidget,
  labelGetMnemonicWidget,
  KeyVal,
  labelGetMnemonicKeyval,
  labelSetUseMarkup,
  labelGetUseMarkup,
  labelSetUseUnderline,
  labelGetUseUnderline,
  labelGetText,
  labelGetLabel,
--  labelSetAttributes,
  labelSetPattern,
  Justification(..),
  labelSetJustify,
  labelGetJustify,
  labelGetLayout,
  labelSetLineWrap,
  labelGetLineWrap,
  labelSetSelectable,
  labelGetSelectable,
  labelSelectRegion,
  labelGetSelectionBounds,
  labelGetLayoutOffsets,

-- * Properties
  labelUseMarkup,
  labelUseUnderline,
  labelJustify,
  labelSelectable,
  labelLineWrap
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes		(Attr(..))
import System.Glib.GObject		(makeNewGObject)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(Justification(..))
import Graphics.UI.Gtk.Pango.Markup

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new label with the given text inside it. You can pass @Nothing@
-- to get an empty label widget.
--
labelNew :: Maybe String -> IO Label
labelNew str =
  makeNewObject mkLabel $
  liftM (castPtr :: Ptr Widget -> Ptr Label) $
  maybeWith withUTFString str $ \strPtr ->
  {# call label_new #}
    strPtr

-- | Creates a new 'Label', containing the text in @str@.
--
-- If characters in @str@ are preceded by an underscore, they are
-- underlined. If you need a literal underscore character in a label, use
-- \'__\' (two underscores). The first underlined character represents a
-- keyboard accelerator called a mnemonic. The mnemonic key can be used to
-- activate another widget, chosen automatically, or explicitly using
-- 'labelSetMnemonicWidget'.
--
-- If 'labelSetMnemonicWidget' is not called, then the first activatable
-- ancestor of the 'Label' will be chosen as the mnemonic widget. For instance,
-- if the label is inside a button or menu item, the button or menu item will
-- automatically become the mnemonic widget and be activated by the mnemonic.
--
labelNewWithMnemonic :: 
    String   -- ^ @str@ - The text of the label, with an underscore in front
             -- of the mnemonic character
 -> IO Label
labelNewWithMnemonic str =
  makeNewObject mkLabel $
  liftM (castPtr :: Ptr Widget -> Ptr Label) $
  withUTFString str $ \strPtr ->
  {# call label_new_with_mnemonic #}
    strPtr

--------------------
-- Methods

-- | Sets the text within the 'Label' widget. It overwrites any text that was
-- there before.
--
-- This will also clear any previously set mnemonic accelerators.
--
labelSetText :: LabelClass self => self -> String -> IO ()
labelSetText self str =
  withUTFString str $ \strPtr ->
  {# call label_set_text #}
    (toLabel self)
    strPtr

-- | Sets the text of the label. The label is interpreted as including
-- embedded underlines and\/or Pango markup depending on the markup and
-- underline properties.
--
labelSetLabel :: LabelClass self => self -> String -> IO ()
labelSetLabel self str =
  withUTFString str $ \strPtr ->
  {# call label_set_label #}
    (toLabel self)
    strPtr

{-
-- | Set the text attributes.
--
-- labelSetAttributes :: LabelClass l => PangoAttrList -> IO ()
-}

-- | Parses @str@ which is marked up with the Pango text markup language,
-- setting the label's text and attribute list based on the parse results. If
-- the @str@ is external data, you may need to escape it.
--
labelSetMarkup :: LabelClass self => self
 -> Markup -- ^ @str@ - a markup string (see Pango markup format)
 -> IO ()
labelSetMarkup self str =
  withUTFString str $ \strPtr ->
  {# call label_set_markup #}
    (toLabel self)
    strPtr

-- | Parses @str@ which is marked up with the Pango text markup language,
-- setting the label's text and attribute list based on the parse results. If
-- characters in @str@ are preceded by an underscore, they are underlined
-- indicating that they represent a keyboard accelerator called a mnemonic.
--
-- The mnemonic key can be used to activate another widget, chosen
-- automatically, or explicitly using 'labelSetMnemonicWidget'.
--
labelSetMarkupWithMnemonic :: LabelClass self => self
 -> Markup -- ^ @str@ - a markup string (see Pango markup format)
 -> IO ()
labelSetMarkupWithMnemonic self str =
  withUTFString str $ \strPtr ->
  {# call label_set_markup_with_mnemonic #}
    (toLabel self)
    strPtr

-- | Underline parts of the text, odd indices of the list represent underlined
-- parts.
--
labelSetPattern :: LabelClass l => l -> [Int] -> IO ()
labelSetPattern self list =
  withUTFString str $
  {# call label_set_pattern #}
    (toLabel self)
  where
    str = concat $ zipWith replicate list (cycle ['_',' '])

-- | Sets the alignment of the lines in the text of the label relative to each
-- other. 'JustifyLeft' is the default value when the widget is first created
-- with 'labelNew'. If you instead want to set the alignment of the label as a
-- whole, use 'miscSetAlignment' instead. 'labelSetJustify' has no effect on
-- labels containing only a single line.
--
labelSetJustify :: LabelClass self => self -> Justification -> IO ()
labelSetJustify self jtype =
  {# call label_set_justify #}
    (toLabel self)
    ((fromIntegral . fromEnum) jtype)

-- | Returns the justification of the label. See 'labelSetJustify'.
--
labelGetJustify :: LabelClass self => self -> IO Justification
labelGetJustify self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe label_get_justify #}
    (toLabel self)

-- | Gets the 'Layout' used to display the label. The layout is useful to e.g.
-- convert text positions to pixel positions, in combination with
-- 'labelGetLayoutOffsets'.
--
labelGetLayout :: LabelClass self => self
 -> IO PangoLayout -- ^ returns the 'Layout' for this label
labelGetLayout self =
  makeNewGObject mkPangoLayout $
  {# call unsafe label_get_layout #}
    (toLabel self)

-- | Toggles line wrapping within the 'Label' widget. @True@ makes it break
-- lines if text exceeds the widget's size. @False@ lets the text get cut off
-- by the edge of the widget if it exceeds the widget size.
--
labelSetLineWrap :: LabelClass self => self
 -> Bool  -- ^ @wrap@ - the setting
 -> IO ()
labelSetLineWrap self wrap =
  {# call label_set_line_wrap #}
    (toLabel self)
    (fromBool wrap)

-- | Returns whether lines in the label are automatically wrapped. See
-- 'labelSetLineWrap'.
--
labelGetLineWrap :: LabelClass self => self
 -> IO Bool -- ^ returns @True@ if the lines of the label are automatically
            -- wrapped.
labelGetLineWrap self =
  liftM toBool $
  {# call unsafe label_get_line_wrap #}
    (toLabel self)

-- | Obtains the coordinates where the label will draw the 'Layout'
-- representing the text in the label; useful to convert mouse events into
-- coordinates inside the 'Layout', e.g. to take some action if some part of
-- the label is clicked. Of course you will need to create a 'EventBox' to
-- receive the events, and pack the label inside it, since labels are a
-- \'NoWindow\' widget.
--
labelGetLayoutOffsets :: LabelClass self => self -> IO (Int, Int)
labelGetLayoutOffsets self =
  alloca $ \xPtr ->
  alloca $ \yPtr -> do
  {# call unsafe label_get_layout_offsets #}
    (toLabel self)
    xPtr
    yPtr
  x <- peek xPtr
  y <- peek yPtr
  return (fromIntegral x, fromIntegral y)

-- | KeyVal is a synonym for a hot key number.
--
type KeyVal = {#type guint#}

-- | If the label has been set so that it has an mnemonic key this function
-- returns the keyval used for the mnemonic accelerator.
--
labelGetMnemonicKeyval :: LabelClass self => self -> IO KeyVal
labelGetMnemonicKeyval self =
  {# call unsafe label_get_mnemonic_keyval #}
    (toLabel self)

-- | Gets whether the text selectable.
--
labelGetSelectable :: LabelClass self => self
 -> IO Bool -- ^ returns @True@ if the user can copy text from the label
labelGetSelectable self =
  liftM toBool $
  {# call unsafe label_get_selectable #}
    (toLabel self)

-- | Sets whether the text of the label contains markup in Pango's text markup
-- language. See 'labelSetMarkup'.
--
labelSetUseMarkup :: LabelClass self => self
 -> Bool  -- ^ @setting@ - @True@ if the label's text should be parsed for
          -- markup.
 -> IO ()
labelSetUseMarkup self setting =
  {# call label_set_use_markup #}
    (toLabel self)
    (fromBool setting)

-- | Returns whether the label's text is interpreted as marked up with the
-- Pango text markup language. See 'labelSetUseMarkup'.
--
labelGetUseMarkup :: LabelClass self => self
 -> IO Bool -- ^ returns @True@ if the label's text will be parsed for markup.
labelGetUseMarkup self =
  liftM toBool $
  {# call unsafe label_get_use_markup #}
    (toLabel self)

-- | If @True@, an underline in the text indicates the next character should be
-- used for the mnemonic accelerator key.
--
labelSetUseUnderline :: LabelClass self => self -> Bool -> IO ()
labelSetUseUnderline self useUnderline =
  {# call label_set_use_underline #}
    (toLabel self)
    (fromBool useUnderline)

-- | Returns whether an embedded underline in the label indicates a mnemonic.
-- See 'labelSetUseUnderline'.
--
labelGetUseUnderline :: LabelClass self => self -> IO Bool
labelGetUseUnderline self =
  liftM toBool $
  {# call unsafe label_get_use_underline #}
    (toLabel self)

-- | Gets the text from a label widget, as displayed on the screen. This
-- does not include any embedded underlines indicating mnemonics or Pango
-- markup. (See 'labelGetLabel')
--
labelGetText :: LabelClass self => self -> IO String
labelGetText self =
  {# call unsafe label_get_text #}
    (toLabel self)
  >>= peekUTFString

-- | Gets the text from a label widget including any embedded underlines
-- indicating mnemonics and Pango markup. (See 'labelGetText').
--
labelGetLabel :: LabelClass self => self -> IO String
labelGetLabel self =
  {# call unsafe label_get_label #}
    (toLabel self)
  >>= peekUTFString

-- | Selects a range of characters in the label, if the label is selectable.
-- See 'labelSetSelectable'. If the label is not selectable, this function has
-- no effect. If @startOffset@ or @endOffset@ are -1, then the end of the label
-- will be substituted.
--
labelSelectRegion :: LabelClass self => self
 -> Int   -- ^ @startOffset@ - start offset
 -> Int   -- ^ @endOffset@ - end offset
 -> IO ()
labelSelectRegion self startOffset endOffset =
  {# call label_select_region #}
    (toLabel self)
    (fromIntegral startOffset)
    (fromIntegral endOffset)

-- | Gets the selected range of characters in the label, if any. If there is
-- a range selected the result is the start and end of the selection as
-- character offsets.
--
labelGetSelectionBounds :: LabelClass self => self
 -> IO (Maybe (Int, Int))
labelGetSelectionBounds self =
  alloca $ \startPtr ->
  alloca $ \endPtr -> do
  isSelection <-
    liftM toBool $
    {# call unsafe label_get_selection_bounds #}
    (toLabel self)
    startPtr
    endPtr
  if isSelection
    then do start <- peek startPtr
            end <- peek endPtr
	    return $ Just $ (fromIntegral start, fromIntegral end)
    else return Nothing

-- | If the label has been set so that it has an mnemonic key (using i.e.
-- 'labelSetMarkupWithMnemonic', 'labelSetTextWithMnemonic',
-- 'labelNewWithMnemonic' or the \"use_underline\" property) the label can be
-- associated with a widget that is the target of the mnemonic. When the label
-- is inside a widget (like a 'Button' or a 'Notebook' tab) it is automatically
-- associated with the correct widget, but sometimes (i.e. when the target is a
-- 'Entry' next to the label) you need to set it explicitly using this
-- function.
--
-- The target widget will be accelerated by emitting \"mnemonic_activate\"
-- on it. The default handler for this signal will activate the widget if there
-- are no mnemonic collisions and toggle focus between the colliding widgets
-- otherwise.
--
labelSetMnemonicWidget :: (LabelClass self, WidgetClass widget) => self
 -> widget -- ^ @widget@ - the target 'Widget'
 -> IO ()
labelSetMnemonicWidget self widget =
  {# call unsafe label_set_mnemonic_widget #}
    (toLabel self)
    (toWidget widget)

-- | Retrieves the target of the mnemonic (keyboard shortcut) of this label.
-- See 'labelSetMnemonicWidget'.
--
labelGetMnemonicWidget :: LabelClass self => self
 -> IO (Maybe Widget) -- ^ returns the target of the label's mnemonic, or
                      -- @Nothing@ if none has been set and the default
                      -- algorithm will be used.
labelGetMnemonicWidget self =
  maybeNull (makeNewObject mkWidget) $
  {# call unsafe label_get_mnemonic_widget #}
    (toLabel self)

-- | Selectable labels allow the user to select text from the label, for
-- copy-and-paste.
--
labelSetSelectable :: LabelClass self => self
 -> Bool  -- ^ @setting@ - @True@ to allow selecting text in the label
 -> IO ()
labelSetSelectable self setting =
  {# call unsafe label_set_selectable #}
    (toLabel self)
    (fromBool setting)

-- | Sets the label's text from the given string. If characters in the string are
-- preceded by an underscore, they are underlined indicating that they
-- represent a keyboard accelerator called a mnemonic. The mnemonic key can be
-- used to activate another widget, chosen automatically, or explicitly using
-- 'labelSetMnemonicWidget'.
--
labelSetTextWithMnemonic :: LabelClass self => self -> String -> IO ()
labelSetTextWithMnemonic self str =
  withUTFString str $ \strPtr ->
  {# call label_set_text_with_mnemonic #}
    (toLabel self)
    strPtr

--------------------
-- Properties

-- | The text of the label includes XML markup. See pango_parse_markup().
--
-- Default value: @False@
--
labelUseMarkup :: LabelClass self => Attr self Bool
labelUseMarkup = Attr 
  labelGetUseMarkup
  labelSetUseMarkup

-- | If set, an underline in the text indicates the next character should be
-- used for the mnemonic accelerator key.
--
-- Default value: @False@
--
labelUseUnderline :: LabelClass self => Attr self Bool
labelUseUnderline = Attr 
  labelGetUseUnderline
  labelSetUseUnderline

-- | The alignment of the lines in the text of the label relative to each
-- other. This does NOT affect the alignment of the label within its
-- allocation. See 'Misc'::xalign for that.
--
-- Default value: 'JustifyLeft'
--
labelJustify :: LabelClass self => Attr self Justification
labelJustify = Attr 
  labelGetJustify
  labelSetJustify

-- | Whether the label text can be selected with the mouse.
--
-- Default value: @False@
--
labelSelectable :: LabelClass self => Attr self Bool
labelSelectable = Attr 
  labelGetSelectable
  labelSetSelectable

-- | \'lineWrap\' property. See 'labelGetLineWrap' and 'labelSetLineWrap'
--
labelLineWrap :: LabelClass self => Attr self Bool
labelLineWrap = Attr 
  labelGetLineWrap
  labelSetLineWrap
