-- -*-haskell-*-
--  GIMP Toolkit (GTK) ListStore TreeModel
--
--  Author : Axel Simon
--
--  Created: 9 May 2001
--
--  Copyright (C) 2001-2005 Axel Simon
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
-- The database for simple (non-hierarchical) tables.
--
-- * This module and all other modules in 'Graphics.UI.Gtk.TreeList' are
--   deprecated. Please use the modules in 'Graphics.UI.Gtk.ModelView'.
--
module Graphics.UI.Gtk.TreeList.ListStore (
-- * Detail
-- 
-- | The 'ListStore' object is a list model for use with a 'TreeView' widget.
-- It implements the 'TreeModel' interface, and consequentialy, can use all of
-- the methods available there. It also implements the 'TreeSortable' interface
-- so it can be sorted by the view. Finally, it also implements the tree drag
-- and drop interfaces.

-- ** Performance Considerations
-- 
-- | Internally, the 'ListStore' is implemented with a linked list with a tail
-- pointer. As a result, it is fast at data insertion and deletion, and not as
-- fast at random data access. The 'ListStore' sets the 'TreeModelItersPersist'
-- flag, which means that 'TreeIter's can be cached while the row exists. Thus,
-- if access to a particular row is needed often, it is worth keeping the iter
-- around.
-- 

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----ListStore
-- @

-- * Types
  ListStore,
  ListStoreClass,
  castToListStore,
  toListStore,
  TMType(..),
  GenericValue(..),

-- * Constructors
  listStoreNew,

-- * Methods
  listStoreSetValue,
  listStoreRemove,
  listStoreInsert,
  listStoreInsertBefore,
  listStoreInsertAfter,
  listStorePrepend,
  listStoreAppend,
  listStoreClear,
#if GTK_CHECK_VERSION(2,2,0)
  listStoreReorder,
  listStoreSwap,
  listStoreMoveBefore,
  listStoreMoveAfter,
#endif
  ) where

import Control.Monad	(liftM, when)

import System.Glib.FFI
import System.Glib.GObject			(constructNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.TreeList.TreeModel#}
{#import Graphics.UI.Gtk.TreeList.TreeIter#}
import System.Glib.StoreValue			(TMType(..), GenericValue(..)
						,valueSetGenericValue)
{#import System.Glib.GValue#}			(GValue(GValue), allocaGValue)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Generate a new entity to store tree information.
--
listStoreNew :: [TMType] -> IO ListStore
listStoreNew types =
  constructNewGObject mkListStore $
  withArray (map (fromIntegral . fromEnum) types) $ \typesArr ->
  {# call unsafe list_store_newv #}
    ((fromIntegral . length) types)
    typesArr

--------------------
-- Methods

-- | Set the data of a specific node.
--
-- * The supplied value must match the type that was set for the column.
--
listStoreSetValue :: ListStoreClass self => self -> TreeIter
 -> Int
 -> GenericValue
 -> IO ()
listStoreSetValue self iter column value =
  with iter $ \iterPtr ->
  allocaGValue $ \gvalue -> do
  valueSetGenericValue gvalue value
  {# call unsafe list_store_set_value #}
    (toListStore self)
    iterPtr
    (fromIntegral column)
    gvalue

#if GTK_CHECK_VERSION(2,1,0)
-- | Remove a specific node.
--
-- * The 'TreeIter' will point to the entry following the one which
--   was just removed. The function returns @False@ if the
--   @ti@TreeIter does not point to a valid element (i.e. the
--   function just removed the bottom entry from the list).
--
-- * This function returned @()@ in Gtk version 2.0.X
--
listStoreRemove :: ListStoreClass self => self
 -> TreeIter
 -> IO Bool
listStoreRemove self iter =
  liftM toBool $
  with iter $ \iterPtr ->
  {# call list_store_remove #}
    (toListStore self)
    iterPtr

#else
-- | Remove a specific node.
--
-- * The 'TreeIter' will point to the entry following the one which
--   was just removed.
--
-- * This function returns @Bool@ in Gtk version 2.2.0 and later
--
listStoreRemove :: ListStoreClass self => self
 -> TreeIter
 -> IO ()
listStoreRemove self iter =
  with iter $ \iterPtr ->
  {# call list_store_remove #}
    (toListStore self)
    iterPtr
#endif

-- | Insert a new row into the list.
--
-- * The @pos@ parameter
-- determines the row number where the row should be inserted. If the position 
-- is greater or equal to the number of rows, the new row will be inserted
-- at the end.
--
listStoreInsert :: ListStoreClass self => self
 -> Int      -- ^ @position@ - position to insert the new row
 -> IO TreeIter
listStoreInsert self position =
  alloca $ \iterPtr -> do
  {# call list_store_insert #}
    (toListStore self)
    iterPtr
    (fromIntegral position)
  peek iterPtr

-- | Insert a row in front of the @sibling@ node.
--
listStoreInsertBefore :: ListStoreClass self => self
 -> TreeIter
 -> IO TreeIter
listStoreInsertBefore self sibling =
  alloca $ \iterPtr ->
  with sibling $ \siblingPtr -> do
  {# call list_store_insert_before #}
    (toListStore self)
    iterPtr
    siblingPtr
  peek iterPtr

-- | Insert a row behind the @sibling@ row.
--
listStoreInsertAfter :: ListStoreClass self => self
 -> TreeIter
 -> IO TreeIter
listStoreInsertAfter self sibling =
  alloca $ \iterPtr ->
  with sibling $ \siblingPtr -> do
  {# call list_store_insert_after #}
    (toListStore self)
    iterPtr
    siblingPtr
  peek iterPtr

-- | Insert a row in front of every other row.
--
-- * This is equivalent to 'listStoreInsert' @0@.
--
listStorePrepend :: ListStoreClass self => self
 -> IO TreeIter
listStorePrepend self =
  alloca $ \iterPtr -> do
  {# call list_store_prepend #}
    (toListStore self)
    iterPtr
  peek iterPtr

-- | Insert a row at the end of the table .
--
-- * This is equivalent to 'listStoreInsert' (-1).
--
listStoreAppend :: ListStoreClass self => self
 -> IO TreeIter
listStoreAppend self =
  alloca $ \iterPtr -> do
  {# call list_store_append #}
    (toListStore self)
    iterPtr
  peek iterPtr

-- | Removes all rows from the list store.
--
listStoreClear :: ListStoreClass self => self -> IO ()
listStoreClear self =
  {# call list_store_clear #}
    (toListStore self)

#if GTK_CHECK_VERSION(2,2,0)
-- | Reorders store to follow the order indicated by the mapping. The list
-- argument should be a mapping from the /new/ positions to the /old/
-- positions. That is @newOrder !! newPos = oldPos@
--
-- * Note that this function only works with unsorted stores.
--
-- * You must make sure the mapping is the right size for the store, use
-- @'treeModelIterNChildren' store Nothing@ to check.
--
listStoreReorder :: (ListStoreClass self) => self -> [Int] -> IO ()
listStoreReorder self newOrder = do
  --check newOrder is the right length or it'll overrun
  storeLength <- treeModelIterNChildren self Nothing
  when (storeLength /= length newOrder)
       (fail "ListStore.listStoreReorder: mapping wrong length for store")
  withArray (map fromIntegral newOrder) $ \newOrderArrPtr ->
    {# call list_store_reorder #}
    (toListStore self)
    newOrderArrPtr

-- | Swaps the two items in the store.
--
-- * Note that this function only works with unsorted stores.
--
-- * Available since Gtk+ version 2.2
--
listStoreSwap :: ListStoreClass self => self
 -> TreeIter
 -> TreeIter
 -> IO ()
listStoreSwap self a b =
  with b $ \aPtr ->
  with a $ \bPtr ->
  {# call list_store_swap #}
    (toListStore self)
    aPtr
    bPtr

-- | Moves the item in the store to before the given position. If the position
-- is @Nothing@ the item will be moved to then end of the list.
--
-- * Note that this function only works with unsorted stores.
--
-- * Available since Gtk+ version 2.2
--
listStoreMoveBefore :: ListStoreClass self => self
 -> TreeIter       -- ^ Iter for the item to be moved
 -> Maybe TreeIter -- ^ Iter for the position or @Nothing@.
 -> IO ()
listStoreMoveBefore self iter position =
  with iter $ \iterPtr ->
  maybeWith with position $ \positionPtr ->
  {# call list_store_move_before #}
    (toListStore self)
    iterPtr
    positionPtr

-- | Moves the item in the store to after the given position. If the position
-- is @Nothing@ the item will be moved to then start of the list.
--
-- * Note that this function only works with unsorted stores.
--
listStoreMoveAfter :: ListStoreClass self => self
 -> TreeIter       -- ^ Iter for the item to be moved
 -> Maybe TreeIter -- ^ Iter for the position or @Nothing@.
 -> IO ()
listStoreMoveAfter self iter position =
  with iter $ \iterPtr ->
  maybeWith with position $ \positionPtr ->
  {# call list_store_move_after #}
    (toListStore self)
    iterPtr
    positionPtr
#endif
