--  GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Copyright (c) 2007 Peter Gavin
--
--  This library is free software: you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public License
--  as published by the Free Software Foundation, either version 3 of
--  the License, or (at your option) any later version.
--  
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--  
--  You should have received a copy of the GNU Lesser General Public
--  License along with this program.  If not, see
--  <http://www.gnu.org/licenses/>.
--  
--  GStreamer, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GStreamer documentation.
--  
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module Media.Streaming.GStreamer.Base (
  
  module Media.Streaming.GStreamer.Base.BaseSrc,
  module Media.Streaming.GStreamer.Base.BaseSink,
  module Media.Streaming.GStreamer.Base.BaseTransform,
  module Media.Streaming.GStreamer.Base.PushSrc,
  module Media.Streaming.GStreamer.Base.Adapter
  
  ) where

import Media.Streaming.GStreamer.Base.BaseSrc
import Media.Streaming.GStreamer.Base.BaseSink
import Media.Streaming.GStreamer.Base.BaseTransform
import Media.Streaming.GStreamer.Base.PushSrc
import Media.Streaming.GStreamer.Base.Adapter

