-- -*-haskell-*-
--  GIMP Toolkit (GTK) CellRendererPixbuf
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.8 $ from $Date: 2004/05/23 16:16:43 $
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

module CellRendererPixbuf(
  CellRendererPixbuf,
  CellRendererPixbufClass,
  castToCellRendererPixbuf,
  cellRendererPixbufNew,
  cellPixbuf
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import CellRenderer (Attribute(..))
import Image	    (imageNewFromPixbuf, imageGetPixbuf)
import StoreValue   (GenericValue(..), TMType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new CellRendererPixbuf object.
--
cellRendererPixbufNew :: IO CellRendererPixbuf
cellRendererPixbufNew  = makeNewObject mkCellRendererPixbuf $ liftM castPtr $
  {#call unsafe cell_renderer_pixbuf_new#}

-- | Define the attribute that specifies the
-- 'Pixbuf' to be rendered.
--
cellPixbuf :: Attribute CellRendererPixbuf Image
cellPixbuf  = Attribute ["pixbuf"] [TMobject]
  (liftM ((\x -> [x]) . GVobject . toGObject) . imageGetPixbuf)
  (\[GVobject obj] -> imageNewFromPixbuf (fromGObject obj))
