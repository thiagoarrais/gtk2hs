-- -*-haskell-*-
--  GIMP Toolkit (GTK) CellRenderer TreeView
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.7 $ from $Date: 2004/05/23 16:16:43 $
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
-- |
--
-- A 'CellRenderer' is an object that determines how the cell of a
-- 'TreeView' widget is displayed. 
--
-- * Each 'TreeViewColumn' has exactly one accociated 'CellRenderer'.
--   The data supply for a cell is contained in a 'TreeStore' or a
--   'ListStore' (both subclasses of 'TreeModel'). Each 'CellRenderer'
--   may have several attributes. Each 'Attribute' is associated with 
--   one column of the 'TreeModel' database. Thus several columns of a 
--   'TreeModel' may be the supply for one 'TreeViewColumn'.
--

module CellRenderer(
  CellRenderer,
  CellRendererClass,
  Attribute(..),
  cellRendererSet,
  cellRendererGet
  ) where

import Hierarchy
import StoreValue	(GenericValue, TMType)
import Object		(objectSetProperty, objectGetProperty)

-- | Definition of the 'Attribute' data type.
--
-- * Each 'CellRenderer' defines a set of attributes. They are used
--   by the Mogul layer to generate columns in a 'TreeStore' or
--   'ListStore'.
--
data CellRendererClass cr => Attribute cr a = Attribute [String] [TMType]
					      (a -> IO [GenericValue]) 
					      ([GenericValue] -> IO a)

-- | Set a property statically.
--
-- * Instead of using a 'TreeStore' or 'ListStore' to set
--   properties of a 'CellRenderer' this method allows to set such
--   a property for the whole column.
--
cellRendererSet :: CellRendererClass cr => 
		   cr -> Attribute cr val -> val -> IO ()
cellRendererSet cr (Attribute names _ write _) val = do
  values <- write val
  mapM_ (uncurry $ objectSetProperty cr) (zip names values)

-- | Get a static property.
--
-- * See 'cellRendererSet'. Note that calling this function on a
--   property of a 'CellRenderer' object which retrieves its values
--   from a 'ListStore' or 'TreeStore' will result in an
--   abitrary value.
--
cellRendererGet :: CellRendererClass cr =>
		   cr -> Attribute cr val -> IO val
cellRendererGet cr (Attribute names _ _ read) = do
  values <- mapM (objectGetProperty cr) names
  read values
