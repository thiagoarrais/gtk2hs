-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Surfaces.PNG
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Reading and writing PNG images.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Surfaces.PNG where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

#ifdef ENABLE_CAIRO_PNG_FUNCTIONS

imageSurfaceCreateFromPNG :: FilePath -> IO Surface
imageSurfaceCreateFromPNG filename =
  withCString filename $ \filenamePtr ->
  {#call unsafe image_surface_create_from_png#} filenamePtr
  >>= mkSurface

{#fun surface_write_to_png as surfaceWriteToPNG { withSurface* `Surface', withCString* `FilePath' } -> `Status' cToEnum#}

#endif
