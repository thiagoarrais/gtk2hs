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
module Media.Streaming.GStreamer.Core.Pipeline (
  Pipeline,
  PipelineClass,
  castToPipeline,
  toPipeline,
  pipelineNew,
  pipelineGetBus,
  pipelineSetClock,
  pipelineUseClock,
  pipelineSetNewStreamTime,
  pipelineGetLastStreamTime,
  pipelineSetAutoFlushBus,
  pipelineGetAutoFlushBus,
  pipelineSetDelay,
  pipelineGetDelay,
  
  pipelineAutoFlushBus,
  pipelineDelay
  ) where

import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Core.Types#}
import System.Glib.UTFString
import System.Glib.FFI
import System.Glib.Attributes ( Attr
                              , newAttr )

{# context lib = "gstreamer" prefix = "gst" #}

pipelineNew :: String
            -> IO Element
pipelineNew name =
    withUTFString name {# call pipeline_new #} >>=
        takeObject

pipelineGetBus :: PipelineClass pipeline
               => pipeline
               -> IO Bus
pipelineGetBus pipeline =
    {# call pipeline_get_bus #} (toPipeline pipeline) >>= takeObject

pipelineSetClock :: (PipelineClass pipeline, ClockClass clock)
                 => pipeline
                 -> clock
                 -> IO Bool
pipelineSetClock pipeline clock =
    liftM toBool $
        {# call pipeline_set_clock #} (toPipeline pipeline) (toClock clock)

pipelineUseClock :: (PipelineClass pipeline, ClockClass clock)
                 => pipeline
                 -> clock
                 -> IO ()
pipelineUseClock pipeline clock =
    {# call pipeline_use_clock #} (toPipeline pipeline) (toClock clock)

pipelineSetNewStreamTime :: PipelineClass pipeline
                         => pipeline
                         -> ClockTime
                         -> IO ()
pipelineSetNewStreamTime pipeline time =
    {# call pipeline_set_new_stream_time #} (toPipeline pipeline)
                                            (fromIntegral time)

pipelineGetLastStreamTime :: PipelineClass pipeline
                          => pipeline
                          -> IO ClockTime
pipelineGetLastStreamTime pipeline =
    liftM fromIntegral $
        {# call pipeline_get_last_stream_time #} (toPipeline pipeline)

pipelineSetAutoFlushBus :: PipelineClass pipeline
                        => pipeline
                        -> Bool
                        -> IO ()
pipelineSetAutoFlushBus pipeline autoFlush =
    {# call pipeline_set_auto_flush_bus #} (toPipeline pipeline) $ fromBool autoFlush

pipelineGetAutoFlushBus :: PipelineClass pipeline
                        => pipeline
                        -> IO Bool
pipelineGetAutoFlushBus pipeline =
    liftM toBool $
        {# call pipeline_get_auto_flush_bus #} (toPipeline pipeline)

pipelineSetDelay :: PipelineClass pipeline
                 => pipeline
                 -> ClockTime
                 -> IO ()
pipelineSetDelay pipeline delay =
    {# call pipeline_set_delay #} (toPipeline pipeline)
                                  (fromIntegral delay)

pipelineGetDelay :: PipelineClass pipeline
                 => pipeline
                 -> IO ClockTime
pipelineGetDelay pipeline =
    liftM fromIntegral $
        {# call pipeline_get_delay #} (toPipeline pipeline)

pipelineAutoFlushBus :: PipelineClass pipelineT
                     => Attr pipelineT Bool
pipelineAutoFlushBus = newAttr
    pipelineGetAutoFlushBus
    pipelineSetAutoFlushBus

pipelineDelay :: PipelineClass pipelineT
              => Attr pipelineT ClockTime
pipelineDelay = newAttr
    pipelineGetDelay
    pipelineSetDelay
