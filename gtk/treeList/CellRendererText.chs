-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry CellRendererText TreeView@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.10 $ from $Date: 2003/07/09 22:42:46 $
--
--  Copyright (c) 1999..2002 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- @description@ --------------------------------------------------------------
--
-- * A @ref data CellRenderer@ which displays a single-line text.
--
-- @documentation@ ------------------------------------------------------------
--
-- * This widget derives from @ref data CellRenderer@. It provides the 
--   possibility to
--   display some text by setting the @ref data Attribute@ 
--   @ref function cellText@ to the column
--   of a @ref data TreeModel@ by means of 
--   @ref method treeViewAddAttribute@ from @ref data TreeModelColumn@.
--
-- @todo@ ---------------------------------------------------------------------

module CellRendererText(
  CellRendererText,
  CellRendererTextClass,
  castToCellRendererText,
  cellRendererTextNew,
  cellText,
  cellMarkup,
  cellBackground,
  cellForeground,
  cellEditable,
  onEdited,
  afterEdited
  ) where

import Maybe	(fromMaybe)
import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
{#import TreeModel#}
import Structs	    (treeIterSize)
import CellRenderer (Attribute(..))
import StoreValue   (GenericValue(..), TMType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor cellRendererTextNew@ Create a new CellRendererText object.
--
cellRendererTextNew :: IO CellRendererText
cellRendererTextNew  = makeNewObject mkCellRendererText $ liftM castPtr $
  {#call unsafe cell_renderer_text_new#}

-- helper function
--
strAttr :: [String] -> Attribute CellRendererText String
strAttr str = Attribute str [TMstring]
	        (return . (\x -> [x]) . GVstring . Just)
		(\[GVstring str] -> return (fromMaybe "" str))

mStrAttr :: [String] -> Attribute CellRendererText (Maybe String)
mStrAttr str = Attribute str [TMstring]
	        (return . (\x -> [x]) . GVstring)
		(\[GVstring str] -> return str)

-- @constant cellText@ Define the attribute that specifies the text to be
-- rendered.
--
cellText :: Attribute CellRendererText String
cellText  = strAttr ["text"]

-- @constant cellMarkup@ Define a markup string instead of a text.
--
cellMarkup :: Attribute CellRendererText String
cellMarkup  = strAttr ["markup"]

-- @constant cellBackground@ A named color for the background paint.
--
cellBackground :: Attribute CellRendererText (Maybe String)
cellBackground  = mStrAttr ["background"]

-- @constant cellForeground@ A named color for the foreground paint.
--
cellForeground :: Attribute CellRendererText (Maybe String)
cellForeground  = mStrAttr ["foreground"]

-- @constant cellEditable@ Determines wether the content can be altered.
--
-- * If this flag is set, the user can alter the cell.
--
cellEditable :: Attribute CellRendererText (Maybe Bool)
cellEditable = Attribute ["editable","editable-set"] [TMboolean,TMboolean]
	         (\mb -> return $ case mb of
		   (Just bool) -> [GVboolean bool, GVboolean True]
		   Nothing     -> [GVboolean True, GVboolean True])
		 (\[GVboolean e, GVboolean s] -> return $
		   if s then Just e else Nothing)

-- @signal connectToEdited@ Emitted when the user finished editing a cell.
--
-- * This signal is not emitted when editing is disabled (see 
--   @ref constant cellEditable@) or when the user aborts editing.
--
onEdited, afterEdited :: TreeModelClass tm => CellRendererText -> tm ->
			 (TreeIter -> String -> IO ()) ->
			 IO (ConnectId CellRendererText)
onEdited cr tm user = connect_PTR_STRING__NONE "edited" False cr $
  \strPtr string -> do
    iterPtr <- mallocBytes treeIterSize
    iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
    res <- liftM toBool $ withForeignPtr ((unTreeModel . toTreeModel) tm) $
      \tmPtr -> withForeignPtr (unTreeIter iter) $ \iterPtr ->
	gtk_tree_model_get_iter_from_string tmPtr iterPtr strPtr
    if res then user iter string else
      putStrLn "edited signal: invalid tree path"

afterEdited cr tm user = connect_PTR_STRING__NONE "edited" True cr $
  \strPtr string -> do
    iterPtr <- mallocBytes treeIterSize
    iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
    res <- liftM toBool $ withForeignPtr ((unTreeModel . toTreeModel) tm) $
      \tmPtr -> withForeignPtr (unTreeIter iter) $ \iterPtr ->
	gtk_tree_model_get_iter_from_string tmPtr iterPtr strPtr
    if res then user iter string else
      putStrLn "edited signal: invalid tree path"

unTreeIter (TreeIter iter) = iter
