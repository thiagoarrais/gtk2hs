-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry TreeStore TreeModel@
--
--  Author : Axel Simon
--          
--  Created: 9 May 2001
--
--  Version $Revision: 1.7 $ from $Date: 2003/01/11 02:42:51 $
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
-- @description@ --------------------------------------------------------------
--
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

module TreeStore(
  TreeStore,
  TMType(..),
  GenericValue(..),
  treeStoreNew,
  treeStoreSetValue,
  treeStoreRemove,
  treeStoreInsert,
  treeStoreInsertBefore,
  treeStoreInsertAfter,
  treeStorePrepend,
  treeStoreAppend,
  treeStoreIsAncestor,
  treeStoreIterDepth
  ) where

import Monad	(liftM)
import Maybe	(fromMaybe)
import Foreign
import UTFCForeign
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import Signal#}
{#import TreeModel#}
import Structs	(treeIterSize, nullForeignPtr)
import StoreValue (TMType(..), GenericValue(..))
{#import GValue#} (GValue, valueUnset)
import GType	  (GType)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor treeStoreNew@ Generate a new entity to store tree information.
--
treeStoreNew :: [TMType] -> IO TreeStore
treeStoreNew cols = makeNewGObject mkTreeStore $ 
  withArray0 ((fromIntegral.fromEnum) TMinvalid) 
  (map (fromIntegral.fromEnum) cols) $
  {#call unsafe tree_store_newv#} ((fromIntegral.length) cols)

-- @method treeStoreSetValue@ Set the data of a specific node. The supplied
-- value must match the type that was set for the column.
--
treeStoreSetValue :: (TreeStoreClass ts) => ts -> TreeIter -> Int ->
                     GenericValue -> IO ()
treeStoreSetValue ts ti col val = with' val $ \vPtr -> do
  {#call unsafe tree_store_set_value#} (toTreeStore ts) ti 
    (fromIntegral col) vPtr
  valueUnset vPtr

-- @method treeStoreRemove@ Remove a specific node.
--
treeStoreRemove :: (TreeStoreClass ts) => ts -> TreeIter -> IO Bool
treeStoreRemove ts ti = liftM toBool $ {#call tree_store_remove#} (toTreeStore ts) ti

-- @method treeStoreInsert@ Insert a child node into the tree. If the parent
-- is Nothing the insert at the root of the tree. The pos parameter determines
-- the position with respect to other siblings. Set this to -1 to insert the
-- node as last node.
--
treeStoreInsert :: (TreeStoreClass ts) => ts -> Maybe TreeIter -> Int ->
                   IO TreeIter
treeStoreInsert ts parent pos = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call tree_store_insert#} (toTreeStore ts) iter 
    (fromMaybe (TreeIter nullForeignPtr) parent) (fromIntegral pos)
  return iter


-- @method treeStoreInsertBefore@ Insert a node in front of the
-- @ref arg sibling@ node on the same level.
--
treeStoreInsertBefore :: (TreeStoreClass ts) => ts -> TreeIter -> IO TreeIter
treeStoreInsertBefore ts sibling = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call tree_store_insert_before#} (toTreeStore ts) iter (TreeIter nullForeignPtr) sibling
  return iter

-- @method treeStoreInsertAfter@ Insert a node behind the @ref arg sibling@
-- node on the same level.
--
treeStoreInsertAfter :: (TreeStoreClass ts) => ts -> TreeIter -> IO TreeIter
treeStoreInsertAfter ts sibling = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call tree_store_insert_after#} (toTreeStore ts) iter (TreeIter nullForeignPtr) sibling
  return iter

-- @method treeStorePrepend@ Insert a child node in front of every other
-- sibling.
--
-- * This is equivalent to @ref method treeStoreInsert@ @literal parent 0@ .
--
treeStorePrepend :: (TreeStoreClass ts) => ts -> Maybe TreeIter -> IO TreeIter
treeStorePrepend ts parent = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call tree_store_prepend#} (toTreeStore ts) iter 
    (fromMaybe (TreeIter nullForeignPtr) parent)
  return iter

-- @method treeStoreAppend@ Insert a child node behind other siblings.
--
-- * This is equivalent to @ref method treeStoreInsert@ @literal parent (-1)@ .
--
treeStoreAppend :: (TreeStoreClass ts) => ts -> Maybe TreeIter -> IO TreeIter
treeStoreAppend ts parent = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call tree_store_append#} (toTreeStore ts) iter 
    (fromMaybe (TreeIter nullForeignPtr) parent)
  return iter

-- @method treeStoreIsAncestor@ Check if a node is in a parental relationship
-- with another node. Returns True even if parent is grandparent,... of child.
--
treeStoreIsAncestor :: (TreeStoreClass ts) => ts -> TreeIter -> TreeIter ->
                       IO Bool
treeStoreIsAncestor ts parent child = liftM toBool $
  {#call unsafe tree_store_is_ancestor#} (toTreeStore ts) parent child

-- @method treeStoreIterDepth@ Calculate the level of a node. Returns 1 for a
-- root node.
--
treeStoreIterDepth :: (TreeStoreClass ts) => ts -> TreeIter -> IO Int
treeStoreIterDepth ts iter = liftM fromIntegral $
  {#call unsafe tree_store_iter_depth#} (toTreeStore ts) iter

