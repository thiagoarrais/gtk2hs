-- -*-haskell-*-
--  The Monad GUI Library (Mogul): @entry Widget TreeView@
--
--  Author : Axel Simon
--          
--  Created: 2 June 2001
--
--  Version $Revision: 1.8 $ from $Date: 2002/11/08 10:39:22 $
--
--  Copyright (c) 2001 Axel Simon
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
-- @documentation@ ------------------------------------------------------------
--
-- * This module provides all object for a widget displaying data organized
--   in a table. There are two flavors: A simple list organizes
--   data in rows and a tree which provides the possibility to impose a
--   hierarchical structure on the entries of one column.
--
-- * The widget is composed of two parts: A database holding rows of data and
--   the widget object itself which displays items based on the database.
--   Several widgets may use a single storage object. The data in the database
--   may be what is directly displayed like strings and images or it may be
--   some meta information like the padding or color of an item. Several
--   attributes in the storage object may be used to display one column
--   in the widget. In contrast each row in the store corresponds to a row
--   in the widget.
--
-- * The widget that displays the data and can be inserted like any other
--   into a container is called @ref data TreeView@. This widget is itself
--   a container for @ref data TreeViewColumn@s which has a title at the top
--   of the column. Each @ref data TreeViewColumn@ in turn can contain
--   several @ref data Renderer@. There are currently three
--   @ref data Renderer@, one for each of the following items: 
--   text, @ref data Pixbuf@ and @ref data ToggleButton@.
--
-- * The database is called store, specifically for simple lists it is
--   @ref data ListStore@ and for hierachical data it is called 
--   @ref data TreeStore@. A store is created from a skeleton. 
--   @ref data  Attribute@s can be added to an empty 
--   @ref data ListSkel@ or @ref data TreeSkel@ skeleton which yields
--   a functions to access the attribute and an @ref data Association@.
--   After the skeleton is turned into a store by calling either
--   @ref constructor newListStore@ or @ref constructor newTreeStore@, 
--   @ref data Association@s can be inserted together with an appropriate
--   @ref data Renderer@ into a @ref data TreeViewColumn@.
--
-- @todo@ ---------------------------------------------------------------------
--
-- * Figure out if properties in the store have priority over global
--   properties when both are set.
--
module TreeList(
  ListSkel,
  emptyListSkel,
  listSkelAddAttribute,
  newListStore,
  TreeSkel,
  emptyTreeSkel,
  treeSkelAddAttribute,
  newTreeStore,
  Association,
  Renderer,
  treeViewColumnNewText,
  treeViewColumnNewPixbuf,
  treeViewColumnNewToggle,
  treeViewColumnAssociate,
  cellRendererSetAttribute,
  cellRendererGetAttribute,
  onEdited,
  afterEdited,  
  TreePath,
  treeModelGetIter,
  treeModelGetPath
  ) where

import Monad	(liftM, mapM, mapM_, foldM)
import GType	(typeInstanceIsA)
import Gtk	hiding (
  -- TreeModel
  treeModelGetValue,
  TreePath,
  treePathNew,
  treePathNewFromString,
  treePathToString,
  treePathNewFirst,
  treePathAppendIndex,
  treePathPrependIndex,
  treePathGetDepth,
  treePathGetIndices,
  treePathCopy,
  treePathCompare,
  treePathNext,
  treePathPrev,
  treePathUp,
  treePathDown,
  treeModelGetIter,
  treeModelGetPath,
  -- ListStore
  listStoreNew,
  listStoreSetValue,
  -- TreeStore
  treeStoreNew,
  treeStoreSetValue,
  -- TreeViewColumn
  treeViewColumnAddAttribute,
  -- CellRendererText
  onEdited,
  afterEdited)
import qualified Gtk
import LocalData	(IORef(..), newIORef, readIORef, writeIORef)
import LocalControl	(throw, Exception(AssertionFailed))

-- @entry ListStore TreeModel@

-- @data ListSkel@ A skeleton of a @ref data ListStore@ database.
--
-- * This datastructure describes what columns the database will have when
--   it is finally created by @ref constructor newListStore@.
--
newtype ListSkel = ListSkel (IORef ListSkelState)

data ListSkelState = LSSPrepare [TMType]
		   | LSSActive ListStore


-- @method emptyListSkel@ Returns an empty @ref data ListSkel@.
--
emptyListSkel :: IO ListSkel
emptyListSkel = liftM ListSkel (newIORef (LSSPrepare []))

-- @method listSkelAddAttribute@ Reserve a new column in
-- @ref data ListSkel@ to hold values for the given attribute.
--
-- * The type of the column is determined by the given @ref data Attribute@
--   of the @ref data ViewColumn@ which should be stored here. It is possible
--   to associate this column with several @ref data ViewColumn@s.
--
listSkelAddAttribute :: CellRendererClass cr => 
			ListSkel -> 
			Attribute cr argTy ->
			IO (Association cr,
			    TreeIter -> IO argTy,
			    TreeIter -> argTy -> IO ())
listSkelAddAttribute (ListSkel statusRef) 
		     (Attribute prop ty toGen fromGen) = do
  status <- readIORef statusRef
  case status of 
    LSSPrepare tTree -> do
      writeIORef statusRef (LSSPrepare (ty++tTree))
      let columnNo = length tTree
      let cols	   = length ty
      return (Association prop columnNo,
	\ti -> do
        status <- readIORef statusRef
	case status of 
	  LSSPrepare _ -> throw $ AssertionFailed 
	    "Modul.TreeStore<readValue>: \
	    \skeleton was not converted to a TreeStore before data access."
	  LSSActive ls -> mapM (Gtk.treeModelGetValue ls ti) 
			  [columnNo..columnNo+cols-1]
			  >>= fromGen,
	\ti arg -> do
        status <- readIORef statusRef
	case status of 
	  LSSPrepare _ -> throw $ AssertionFailed 
	    "Modul.TreeStore<writeValue>: \
	    \skeleton was not converted to a TreeStore before data access."
	  LSSActive ls -> liftM (zip [columnNo..]) (toGen arg) >>= 
			  mapM_ (uncurry (Gtk.listStoreSetValue ls ti))
	)


-- @constructor newListStore@ Create a new @ref data ListStore@ database.
--
-- * This method throws an exception if the skeleton has been used before.
--
newListStore :: ListSkel -> IO ListStore
newListStore (ListSkel statusRef) = do
  status <- readIORef statusRef
  case status of
    LSSPrepare tList -> do
      ls <- Gtk.listStoreNew (reverse tList)
      writeIORef statusRef (LSSActive ls)
      return ls
    LSSActive _ -> throw $ AssertionFailed 
      "Mogul.newListStore: tried to reuse a ListStore skeleton."

-- @entry TreeStore@

-- @data TreeSkel@ A skeleton of a @ref data TreeStore@ database.
--
-- * This datastructure describes what columns the database will have when
--   it is finally created by @ref constructor newTreeStore@
--
newtype TreeSkel = TreeSkel (IORef TreeSkelState)

data TreeSkelState = TSSPrepare [TMType]
		   | TSSActive TreeStore


-- @method emptyTreeSkel@ Returns an empty @ref data TreeSkel@.
--
emptyTreeSkel :: IO TreeSkel
emptyTreeSkel = liftM TreeSkel (newIORef (TSSPrepare []))

-- @method treeSkelAddAttribute@ Reserve a new column in
-- @ref data TreeSkel@ to hold values for the given attribute.
--
-- * The type of the column is determined by the given @ref data Attribute@
--   of the @ref data ViewColumn@ which should be stored here. It is possible
--   to associate this column with several @ref data ViewColumn@s.
--
treeSkelAddAttribute :: CellRendererClass r => TreeSkel -> 
			Attribute r argTy ->
			IO (Association r,
			    TreeIter -> IO argTy,
			    TreeIter -> argTy -> IO ())
treeSkelAddAttribute (TreeSkel statusRef) 
		     (Attribute prop ty toGen fromGen) = do
  status <- readIORef statusRef
  case status of 
    TSSPrepare tTree -> do
      writeIORef statusRef (TSSPrepare (ty++tTree))
      let columnNo = length tTree
      let cols	   = length ty
      return (Association prop columnNo,
	\ti -> do
        status <- readIORef statusRef
	case status of 
	  TSSPrepare _ -> throw $ AssertionFailed 
	    "Modul.TreeStore<readValue>: \
	    \skeleton was not converted to a TreeStore before data access."
	  TSSActive ls -> mapM (Gtk.treeModelGetValue ls ti)
			  [columnNo..columnNo+cols-1]
			  >>= fromGen,
	\ti arg -> do
        status <- readIORef statusRef
	case status of 
	  TSSPrepare _ -> throw $ AssertionFailed 
	    "Modul.TreeStore<writeValue>: \
	    \skeleton was not converted to a TreeStore before data access."
	  TSSActive ls -> liftM (zip [columnNo..]) (toGen arg) >>= 
			  mapM_ (uncurry $Gtk.treeStoreSetValue ls ti)
	)

-- @constructor newTreeStore@ Create a new @ref data TreeStore@ database.
--
-- * This method throws an exception if the skeleton has been used before.
--
newTreeStore :: TreeSkel -> IO TreeStore
newTreeStore (TreeSkel statusRef) = do
  status <- readIORef statusRef
  case status of
    TSSPrepare tTree -> do
      ls <- Gtk.treeStoreNew (reverse tTree)
      writeIORef statusRef (TSSActive ls)
      return ls
    TSSActive _ -> throw $ AssertionFailed 
      "Mogul.newTreeStore: tried to reuse a TreeStore skeleton."

-- @entry Widget TreeView@

-- @data Association@ An abstract link between a store and a view.
--
data CellRendererClass cr => Association cr = Association [String] Int

-- @data TextRenderer@ A renderer for text in a @ref data TreeView@.
--
data CellRendererClass cr => Renderer cr = Renderer cr TreeViewColumn

-- @method treeViewColumnNewText@ Create a new rederer showing text.
--
-- * There can be several @ref data Renderer@ in each 
--   @ref data TreeViewColumn@. Each @ref data Renderer@ can reflect
--   several @ref data Attributes@ from a @ref data ListStore@ or
--   @ref data TreeStore@.
--
treeViewColumnNewText :: TreeViewColumn -> Bool -> Bool -> 
			 IO (Renderer CellRendererText)
treeViewColumnNewText tvc atStart expand = do
  ren <- cellRendererTextNew
  (if atStart then Gtk.treeViewColumnPackStart else Gtk.treeViewColumnPackEnd)
    tvc ren expand
  return $ Renderer ren tvc

-- @method treeViewColumnNewPixbuf@ Create a new renderer showing a
-- @ref data Pixbuf@.
--
--
-- * There can be several @ref data Renderer@ in each 
--   @ref data TreeViewColumn@. Each @ref data Renderer@ can reflect
--   several @ref data Attributes@ from a @ref data ListStore@ or
--   @ref data TreeStore@.
--
treeViewColumnNewPixbuf :: TreeViewColumn -> Bool -> Bool -> 
			   IO (Renderer CellRendererPixbuf)
treeViewColumnNewPixbuf tvc atStart expand = do
  ren <- cellRendererPixbufNew
  (if atStart then Gtk.treeViewColumnPackStart else Gtk.treeViewColumnPackEnd)
    tvc ren expand
  return $ Renderer ren tvc

-- @method treeViewColumnNewPixbuf@ Create a new renderer showing a
-- @ref data ToggleButton@.
--
--
-- * There can be several @ref data Renderer@ in each 
--   @ref data TreeViewColumn@. Each @ref data Renderer@ can reflect
--   several @ref data Attributes@ from a @ref data ListStore@ or
--   @ref data TreeStore@.
--
treeViewColumnNewToggle :: TreeViewColumn -> Bool -> Bool ->
			   IO (Renderer CellRendererToggle)
treeViewColumnNewToggle tvc atStart expand = do
  ren <- cellRendererToggleNew
  (if atStart then Gtk.treeViewColumnPackStart else Gtk.treeViewColumnPackEnd)
    tvc ren expand
  return $ Renderer ren tvc

-- @method treeViewColumnAssociate@ Create a link between the store and this
-- model.
--
-- * The results are undefined, if this @ref data TreeViewColumn@ was not
--   created with the same @ref data TreeModel@ as the @ref data Association@s.
--
treeViewColumnAssociate :: CellRendererClass r => Renderer r -> 
						  [Association r] -> IO ()
treeViewColumnAssociate (Renderer ren  tvc) assocs = do
  let assocs' = concatMap (\(Association strs col) -> zip strs [col..]) assocs
  mapM_ (\(attr,col) ->
    Gtk.treeViewColumnAddAttribute tvc ren attr col) assocs'

-- @entry CellRenderer TreeView@

-- @method cellRendererSetAttribute@ Set an @ref data Attribute@ globally.
--
-- * An @ref data Attribute@ of a @ref data Renderer@ can either be set
--   on a row-by-row basis using @ref method listSkelAddAttribute@ and
--   @ref method treeSkelAddAttribute@ or globally through this function.
--   
cellRendererSetAttribute :: CellRendererClass cr => Renderer cr ->
						    Attribute cr val ->
						    val -> IO ()
cellRendererSetAttribute (Renderer ren _) = Gtk.cellRendererSet ren

-- @method cellRendererGetAttribute@ Get an global @ref data Attribute@.
--
cellRendererGetAttribute :: CellRendererClass cr => Renderer cr ->
						    Attribute cr val ->
						    IO val
cellRendererGetAttribute (Renderer ren _) = Gtk.cellRendererGet ren

-- @entry CellRendererText TreeView@

-- @signal edited@ Emitted when the user finished editing a cell.
--
-- * This signal is not emitted when editing is disabled (see 
--   @ref constant cellEditable@) or when the user aborts editing.
--
onEdited, afterEdited :: TreeModelClass tm =>
			 Renderer CellRendererText -> tm ->
			 (TreeIter -> String -> IO ()) ->
			 IO (ConnectId CellRendererText)
onEdited (Renderer ren _) = Gtk.onEdited ren
afterEdited (Renderer ren _) = Gtk.afterEdited ren

-- @entry TreeModel@

-- @type TreePath@ A simple way of addressing nodes.
--
-- These integer lists are used to address nodes in a hierarchical 
-- @ref data ListStore@ structure. 
--
type TreePath = [Int]

-- @method treeModelGetIter@ Turn a @ref type TreePath@ into an abstract
-- @ref data TreeIter@ator.
--
treeModelGetIter :: TreeModelClass tm => tm -> TreePath -> IO (Maybe TreeIter)
treeModelGetIter _  [] = throw $ AssertionFailed "Mogul.treeModelGetIter: \
			 \a path must contain at least one element."
treeModelGetIter tm tp = do
  realPath <- Gtk.treePathNew
  mapM_ (Gtk.treePathAppendIndex realPath) tp
  Gtk.treeModelGetIter tm realPath

-- @method treeModelGetPath@ Turn an abstract @ref data TreeIter@ into a
-- @ref type TreePath@.
--
treeModelGetPath :: TreeModelClass tm => tm -> TreeIter -> IO TreePath
treeModelGetPath tm ti = do
  realPath <- Gtk.treeModelGetPath tm ti
  Gtk.treePathGetIndices realPath
