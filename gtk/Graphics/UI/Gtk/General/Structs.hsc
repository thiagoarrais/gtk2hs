{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS -cpp #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Structures
--
--  Author : Axel Simon
--
--  Created: 2 May 2001
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
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
module Graphics.UI.Gtk.General.Structs (
  Point,
  Rectangle(..),
  Color(..),
  GCValues(..),
  pokeGCValues,
  newGCValues,
  widgetGetState,
  widgetGetSavedState,
  Allocation,
  Requisition(..),
  treeIterSize,
  textIterSize,
  inputError,
  dialogGetUpper,
  dialogGetActionArea,
  fileSelectionGetButtons,
  ResponseId(..),
  fromResponse,
  toResponse,
  #if !defined(WIN32) || GTK_CHECK_VERSION(2,8,0)
  NativeWindowId,
  toNativeWindowId,
  fromNativeWindowId,
  nativeWindowIdNone,
  #endif
#ifndef DISABLE_DEPRECATED
  toolbarChildButton,
  toolbarChildToggleButton,
  toolbarChildRadioButton,
#endif
  IconSize(..),
#ifndef DISABLE_DEPRECATED
  comboGetList,
#endif
  widgetGetDrawWindow,
  widgetGetSize,
  layoutGetDrawWindow,
  styleGetForeground,
  styleGetBackground,
  styleGetLight,
  styleGetMiddle,
  styleGetDark,
  styleGetText,
  styleGetBase,
  styleGetAntiAliasing,
  colorSelectionDialogGetColor,
  colorSelectionDialogGetOkButton,
  colorSelectionDialogGetCancelButton,
  colorSelectionDialogGetHelpButton,
  dragContextGetActions,
  dragContextSetActions,
  dragContextGetSuggestedAction,
  dragContextSetSuggestedAction,
  dragContextGetAction,
  dragContextSetAction,
  SortColumnId,
  treeSortableDefaultSortColumnId,
  tagInvalid,
  selectionPrimary,
  selectionSecondary,
  selectionClipboard,
  targetString,
  selectionTypeAtom,
  selectionTypeInteger,
  selectionTypeString,
  selectionDataGetType,
  withTargetEntries
  ) where

import Control.Monad		(liftM)
import Data.IORef
#ifdef HAVE_NEW_CONTROL_EXCEPTION
import Control.OldException
#else
import Control.Exception
#endif

import System.Glib.FFI
import System.Glib.UTFString ( UTFCorrection, ofsToUTF )
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import System.Glib.GObject		(makeNewGObject)
import Graphics.UI.Gtk.Types
import Graphics.UI.Gtk.Gdk.Enums	(Function, Fill, SubwindowMode,
					 LineStyle, CapStyle, JoinStyle)
import Graphics.UI.Gtk.General.Enums	(StateType)
import Graphics.UI.Gtk.General.DNDTypes (InfoId, Atom(Atom) , SelectionTag,
                                         TargetTag, SelectionTypeTag)
-- | Represents the x and y coordinate of a point.
--
type Point = (Int, Int)

-- | Rectangle
--
-- * for Events
--
-- * Specifies x, y, width and height
--
data Rectangle = Rectangle Int Int Int Int deriving (Eq,Show)

instance Storable Rectangle where
  sizeOf _ = #{const sizeof(GdkRectangle)}
  alignment _ = alignment (undefined:: #gtk2hs_type gint)
  peek ptr = do
    (x_	     ::#gtk2hs_type gint)	<- #{peek GdkRectangle, x} ptr
    (y_	     ::#gtk2hs_type gint)	<- #{peek GdkRectangle, y} ptr
    (width_  ::#gtk2hs_type gint)	<- #{peek GdkRectangle, width} ptr
    (height_ ::#gtk2hs_type gint)	<- #{peek GdkRectangle, height} ptr
    return $ Rectangle (fromIntegral x_) (fromIntegral y_) 
		       (fromIntegral width_) (fromIntegral height_)
  poke ptr (Rectangle x y width height) = do
    #{poke GdkRectangle, x} ptr ((fromIntegral x)::#gtk2hs_type gint)
    #{poke GdkRectangle, y} ptr ((fromIntegral y)::#gtk2hs_type gint)
    #{poke GdkRectangle, width} ptr ((fromIntegral width)::#gtk2hs_type gint)
    #{poke GdkRectangle, height} ptr ((fromIntegral height)::#gtk2hs_type gint)

-- | Color
--
-- * Specifies a color with three integer values for red, green and blue.
--   All values range from 0 (least intense) to 65535 (highest intensity).
--
data Color = Color (#gtk2hs_type guint16) (#gtk2hs_type guint16) (#gtk2hs_type guint16)
	     deriving (Eq,Show)
instance Storable Color where
  sizeOf _ = #{const sizeof(GdkColor)}
  alignment _ = alignment (undefined::#gtk2hs_type guint32)
  peek ptr = do
    red	   <- #{peek GdkColor, red} ptr
    green  <- #{peek GdkColor, green} ptr
    blue   <- #{peek GdkColor, blue} ptr
    return $ Color red green blue
  poke ptr (Color red green blue) = do
    #{poke GdkColor, pixel} ptr (0::#{gtk2hs_type gint32})
    #{poke GdkColor, red}   ptr red
    #{poke GdkColor, green} ptr green
    #{poke GdkColor, blue}  ptr blue
    cPtr <- gdkColormapGetSystem
    gdkColormapAllocColor cPtr ptr 0 1
    return ()

type ColorMap = ()

foreign import ccall unsafe "gdk_colormap_get_system"
  gdkColormapGetSystem :: IO (Ptr ColorMap)

foreign import ccall unsafe "gdk_colormap_alloc_color"
  gdkColormapAllocColor :: Ptr ColorMap -> Ptr Color -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "gdk_colormap_query_color"
  gdkColormapQueryColor :: Ptr ColorMap -> CULong -> Ptr Color -> IO ()

-- entry GC

-- | Intermediate data structure for 'GC's.
--
-- * If @graphicsExposure@ is set then copying portions into a
--   drawable will generate an @\"exposure\"@ event, even if the
--   destination area is not currently visible.
--
data GCValues = GCValues {
  foreground :: Color,
  background :: Color,
  function   :: Function,
  fill       :: Fill,
  tile       :: Maybe Pixmap,
  stipple    :: Maybe Pixmap,
  clipMask   :: Maybe Pixmap,
  subwindowMode :: SubwindowMode,
  tsXOrigin  :: Int,
  tsYOrigin  :: Int,
  clipXOrigin:: Int,
  clipYOrigin:: Int,
  graphicsExposure :: Bool,
  lineWidth  :: Int,
  lineStyle  :: LineStyle,
  capStyle   :: CapStyle,
  joinStyle  :: JoinStyle
  }

instance Storable GCValues where
  sizeOf _ = #{const sizeof(GdkGCValues)}
  alignment _ = alignment (undefined::Color)
  peek ptr = do
    -- gdk_gc_get_values does not fill in the r,g,b members of the foreground
    -- and background colours (it only fills in the allocated pixel value),
    -- so we have to fill them in here:
    let foregroundPtr, backgroundPtr :: Ptr Color
        foregroundPtr = #{ptr GdkGCValues, foreground} ptr
        backgroundPtr = #{ptr GdkGCValues, background} ptr
    (foregroundPixelPtr :: CULong) <- #{peek GdkColor, pixel} foregroundPtr
    (backgroundPixelPtr :: CULong) <- #{peek GdkColor, pixel} backgroundPtr
    colormapPtr <- gdkColormapGetSystem    
    gdkColormapQueryColor colormapPtr foregroundPixelPtr foregroundPtr
    gdkColormapQueryColor colormapPtr backgroundPixelPtr backgroundPtr

    foreground_ <- peek (#{ptr GdkGCValues, foreground} ptr)
    background_ <- peek (#{ptr GdkGCValues, background} ptr)
    (function_	:: #{gtk2hs_type GdkFunction}) <- #{peek GdkGCValues, function} ptr
    (fill_	:: #{gtk2hs_type GdkFill}) <- #{peek GdkGCValues, fill} ptr
    tile_	<- do
		     pPtr <- #{peek GdkGCValues, tile} ptr
		     if (pPtr==nullPtr) then return Nothing else
		       liftM Just $ makeNewGObject mkPixmap $ return pPtr
    stipple_	<- do
		     pPtr <- #{peek GdkGCValues, stipple} ptr
		     if (pPtr==nullPtr) then return Nothing else
		       liftM Just $ makeNewGObject mkPixmap $ return pPtr
    clipMask_	<- do
		     pPtr <- #{peek GdkGCValues, clip_mask} ptr
		     if (pPtr==nullPtr) then return Nothing else
		       liftM Just $ makeNewGObject mkPixmap $ return pPtr
    (subwindow_	:: #{gtk2hs_type GdkSubwindowMode}) 
		<- #{peek GdkGCValues, subwindow_mode} ptr
    (tsXOrigin_	:: #{gtk2hs_type gint}) 
		<- #{peek GdkGCValues, ts_x_origin} ptr
    (tsYOrigin_	:: #{gtk2hs_type gint}) 
		<- #{peek GdkGCValues, ts_y_origin} ptr
    (clipXOrigin_:: #{gtk2hs_type gint}) 
		<- #{peek GdkGCValues, clip_x_origin} ptr
    (clipYOrigin_:: #{gtk2hs_type gint}) 
		<- #{peek GdkGCValues, clip_y_origin} ptr
    (graphics_	:: #{gtk2hs_type gint})
		<- #{peek GdkGCValues, graphics_exposures} ptr
    (lineWidth_	:: #{gtk2hs_type gint})
		<- #{peek GdkGCValues, line_width} ptr
    (lineStyle_	:: #{gtk2hs_type GdkLineStyle}) 
		<- #{peek GdkGCValues, line_style} ptr
    (capStyle_	:: #{gtk2hs_type GdkCapStyle}) 
		<- #{peek GdkGCValues, cap_style} ptr
    (joinStyle_	:: #{gtk2hs_type GdkJoinStyle}) 
		<- #{peek GdkGCValues, join_style} ptr
    return $ GCValues {
      foreground = foreground_,
      background = background_,
      function   = (toEnum.fromIntegral) function_,
      fill       = (toEnum.fromIntegral) fill_,
      tile       = tile_,
      stipple    = stipple_,
      clipMask   = clipMask_,
      subwindowMode = (toEnum.fromIntegral) subwindow_,
      tsXOrigin  = fromIntegral tsXOrigin_,
      tsYOrigin  = fromIntegral tsYOrigin_,
      clipXOrigin= fromIntegral clipXOrigin_,
      clipYOrigin= fromIntegral clipYOrigin_,
      graphicsExposure = toBool graphics_,
      lineWidth  = fromIntegral lineWidth_,
      lineStyle  = (toEnum.fromIntegral) lineStyle_,
      capStyle   = (toEnum.fromIntegral) capStyle_,
      joinStyle  = (toEnum.fromIntegral) joinStyle_
    }

pokeGCValues :: Ptr GCValues -> GCValues -> IO CInt
pokeGCValues ptr (GCValues {
    foreground = foreground_,
    background = background_,
    function   = function_,
    fill       = fill_,
    tile       = tile_,
    stipple    = stipple_,
    clipMask   = clipMask_,
    subwindowMode = subwindow_,
    tsXOrigin  = tsXOrigin_,
    tsYOrigin  = tsYOrigin_,
    clipXOrigin= clipXOrigin_,
    clipYOrigin= clipYOrigin_,
    graphicsExposure = graphics_,
    lineWidth  = lineWidth_,
    lineStyle  = lineStyle_,
    capStyle   = capStyle_,
    joinStyle  = joinStyle_
  }) = do
    r <- newIORef 0
    add r #{const GDK_GC_FOREGROUND } $ 
      poke (#{ptr GdkGCValues, foreground} ptr) foreground_
    add r #{const GDK_GC_BACKGROUND } $ 
      poke (#{ptr GdkGCValues, background} ptr) background_
    add r #{const GDK_GC_FUNCTION } $ 
      #{poke GdkGCValues, function} ptr 
      (fromIntegral (fromEnum function_):: #{gtk2hs_type GdkFunction})
    add r #{const GDK_GC_FILL } $
      #{poke GdkGCValues, fill} ptr 
      (fromIntegral (fromEnum fill_):: #{gtk2hs_type GdkFill})
    case tile_ of 
      Nothing -> return ()
      Just tile_ -> add r #{const GDK_GC_TILE} $
                    withForeignPtr (unPixmap tile_) $
                    #{poke GdkGCValues, tile} ptr
    case stipple_ of
      Nothing -> return ()
      Just stipple_ -> add r #{const GDK_GC_STIPPLE} $
                       withForeignPtr (unPixmap stipple_) $
                       #{poke GdkGCValues, stipple} ptr
    case clipMask_ of
      Nothing -> return ()
      Just clipMask_ -> add r #{const GDK_GC_CLIP_MASK } $
                        withForeignPtr (unPixmap clipMask_) $
                        #{poke GdkGCValues, clip_mask} ptr
    add r #{const GDK_GC_SUBWINDOW } $
      #{poke GdkGCValues, subwindow_mode} ptr
      (fromIntegral (fromEnum subwindow_):: #{gtk2hs_type GdkSubwindowMode})
    add r #{const GDK_GC_TS_X_ORIGIN } $
      #{poke GdkGCValues, ts_x_origin } ptr
      (fromIntegral tsXOrigin_:: #{gtk2hs_type gint})
    add r #{const GDK_GC_TS_Y_ORIGIN } $
      #{poke GdkGCValues, ts_y_origin } ptr
      (fromIntegral tsYOrigin_:: #{gtk2hs_type gint})
    add r #{const GDK_GC_CLIP_X_ORIGIN } $ 
      #{poke GdkGCValues, clip_x_origin } ptr
      (fromIntegral clipXOrigin_:: #{gtk2hs_type gint})
    add r #{const GDK_GC_CLIP_Y_ORIGIN } $
      #{poke GdkGCValues, clip_y_origin } ptr
      (fromIntegral clipYOrigin_:: #{gtk2hs_type gint})
    add r #{const GDK_GC_EXPOSURES } $
      #{poke GdkGCValues, graphics_exposures } ptr
      (fromBool graphics_:: #{gtk2hs_type gint})
    add r #{const GDK_GC_LINE_WIDTH } $
      #{poke GdkGCValues, line_width } ptr
      (fromIntegral lineWidth_:: #{gtk2hs_type gint})
    add r #{const GDK_GC_LINE_STYLE } $
      #{poke GdkGCValues, line_style } ptr
      (fromIntegral (fromEnum lineStyle_):: #{gtk2hs_type GdkLineStyle})
    add r #{const GDK_GC_CAP_STYLE } $ 
      #{poke GdkGCValues, cap_style } ptr
      (fromIntegral (fromEnum capStyle_):: #{gtk2hs_type GdkCapStyle})
    add r #{const GDK_GC_JOIN_STYLE } $ 
      #{poke GdkGCValues, join_style } ptr
      (fromIntegral (fromEnum joinStyle_):: #{gtk2hs_type GdkJoinStyle})
    readIORef r
  where
    add :: IORef CInt -> CInt -> IO () -> IO ()
    add r mVal act = handle (const $ return ()) $ do
      act
      modifyIORef r (\val -> val+mVal)

-- constant newGCValues An empty record of 'GCValues'.
--
-- * Use this value instead of the constructor to avoid compiler wanings
--   about uninitialized fields.
--
newGCValues :: GCValues
newGCValues = GCValues {
    foreground = undefined,
    background = undefined,
    function   = undefined,
    fill       = undefined,
    tile       = Nothing,
    stipple    = Nothing,
    clipMask   = Nothing,
    subwindowMode = undefined,
    tsXOrigin  = undefined,
    tsYOrigin  = undefined,
    clipXOrigin= undefined,
    clipYOrigin= undefined,
    graphicsExposure = undefined,
    lineWidth  = undefined,
    lineStyle  = undefined,
    capStyle   = undefined,
    joinStyle  = undefined
  }

-- Widget related methods

-- | Retrieve the current state of the widget.
--
-- * The state refers to different modes of user interaction, see
--   'StateType' for more information.
--
widgetGetState :: WidgetClass w => w -> IO StateType
widgetGetState w =
  liftM (\x -> toEnum (fromIntegral (x :: #gtk2hs_type guint8))) $
  withForeignPtr ((unWidget . toWidget) w) $ #{peek GtkWidget,state}

-- | Retrieve the current state of the widget.
--
-- * If a widget is turned insensitive, the previous state is stored in
--   a specific location. This function retrieves this previous state.
--
widgetGetSavedState :: WidgetClass w => w -> IO StateType
widgetGetSavedState w =
  liftM (\x -> toEnum (fromIntegral (x :: #gtk2hs_type guint8))) $
  withForeignPtr ((unWidget . toWidget) w) $ #{peek GtkWidget,saved_state}


-- | Allocation
--
-- * For Widget's 'Graphics.UI.Gtk.Abstract.Widget.sizeAllocate' signal.
--   The @x@ and @y@ values of the rectangle refer to the widgets position
--   relative to its parent window.
--
type Allocation = Rectangle


-- | Requisition
--
-- * For 'Graphics.UI.Gtk.Abstract.Widget.widgetSizeRequest'. The values
--   represent the desired width and height of the widget.
--
data Requisition = Requisition Int Int deriving (Eq,Show)

instance Storable Requisition where
  sizeOf _ = #{const sizeof(GtkRequisition)}
  alignment _ = alignment (undefined::#gtk2hs_type gint)
  peek ptr = do
    (width_  ::#gtk2hs_type gint)	<- #{peek GtkRequisition, width} ptr
    (height_ ::#gtk2hs_type gint)	<- #{peek GtkRequisition, height} ptr
    return $ Requisition (fromIntegral width_) (fromIntegral height_)
  poke ptr (Requisition width height) = do
    #{poke GtkRequisition, width} ptr ((fromIntegral width)::#gtk2hs_type gint)
    #{poke GtkRequisition, height} ptr ((fromIntegral height)::#gtk2hs_type gint)


-- SpinButton related mothods

-- If an invalid input has been put into a SpinButton the input function may
-- reject this value by returning this value.
inputError :: #{gtk2hs_type gint}
inputError = #{const GTK_INPUT_ERROR}


-- The TreeIter struct is not used by itself. But we have to allocate space
-- for it in module TreeModel.
treeIterSize :: Int
treeIterSize = #{const sizeof(GtkTreeIter)}


-- The TextIter struct can be a local variable in a C program. We have to
-- store it on the heap.
--
textIterSize :: Int
textIterSize = #{const sizeof(GtkTextIter)}

-- Dialog related methods

-- | Get the upper part of a dialog.
--
-- * The upper part of a dialog window consists of a 'VBox'.
--   Add the required widgets into this box.
--
dialogGetUpper :: DialogClass dc => dc -> IO VBox
dialogGetUpper dc = makeNewObject mkVBox $ liftM castPtr $
  withForeignPtr ((unDialog.toDialog) dc) #{peek GtkDialog, vbox}

-- | Extract the action area of a dialog box.
--
-- * This
-- is useful to add some special widgets that cannot be added with
-- dialogAddActionWidget.
--
dialogGetActionArea :: DialogClass dc => dc -> IO HBox
dialogGetActionArea dc = makeNewObject mkHBox $ liftM castPtr $
  withForeignPtr ((unDialog.toDialog) dc) #{peek GtkDialog, action_area} 

-- | Some constructors that can be used as response
-- numbers for dialogs.
--
data ResponseId

  -- | GTK returns this if a response widget has no @response_id@,
  --   or if the dialog gets programmatically hidden or destroyed.
  = ResponseNone

  -- | GTK won't return these unless you pass them in as
  --   the response for an action widget. They are for your convenience.
  | ResponseReject
  | ResponseAccept -- ^ (as above)

  -- | If the dialog is deleted.
  | ResponseDeleteEvent

  -- | \"Ok\" was pressed.
  --
  -- * This value is returned from the \"Ok\" stock dialog button.
  | ResponseOk

  -- | \"Cancel\" was pressed.
  --
  -- * These value is returned from the \"Cancel\" stock dialog button.
  | ResponseCancel

  -- | \"Close\" was pressed.
  --
  -- * This value is returned from the \"Close\" stock dialog button.
	| ResponseClose

  -- | \"Yes\" was pressed.
  --
  -- * This value is returned from the \"Yes\" stock dialog button.
  | ResponseYes

  -- | \"No\" was pressed.
  --
  -- * This value is returned from the \"No\" stock dialog button.
  | ResponseNo

  -- | \"Apply\" was pressed.
  --
  -- * This value is returned from the \"Apply\" stock dialog button.
	| ResponseApply

  -- |  \"Help\" was pressed.
  --
  -- * This value is returned from the \"Help\" stock dialog button.
  | ResponseHelp

  -- | A user-defined response
  --
  -- * This value is returned from a user defined button
  | ResponseUser Int
  deriving (Show, Eq)

fromResponse :: Integral a => ResponseId -> a
fromResponse ResponseNone = -1
fromResponse ResponseReject = -2
fromResponse ResponseAccept = -3
fromResponse ResponseDeleteEvent = -4
fromResponse ResponseOk = -5
fromResponse ResponseCancel = -6
fromResponse ResponseClose = -7
fromResponse ResponseYes = -8
fromResponse ResponseNo = -9
fromResponse ResponseApply = -10
fromResponse ResponseHelp = -11
fromResponse (ResponseUser i) | i > 0 = fromIntegral i

toResponse :: Integral a => a -> ResponseId
toResponse (-1) = ResponseNone
toResponse (-2) = ResponseReject
toResponse (-3) = ResponseAccept
toResponse (-4) = ResponseDeleteEvent
toResponse (-5) = ResponseOk
toResponse (-6) = ResponseCancel
toResponse (-7) = ResponseClose
toResponse (-8) = ResponseYes
toResponse (-9) = ResponseNo
toResponse (-10) = ResponseApply
toResponse (-11) = ResponseHelp
toResponse i | i >= 0  = ResponseUser $ fromIntegral i

#if !defined(WIN32) || GTK_CHECK_VERSION(2,8,0)
-- | The identifer of a window of the underlying windowing system.
--
#ifdef GDK_NATIVE_WINDOW_POINTER
newtype NativeWindowId = NativeWindowId (Ptr ()) deriving (Eq, Show)
unNativeWindowId :: NativeWindowId -> Ptr a
unNativeWindowId (NativeWindowId id) = castPtr id
toNativeWindowId :: Ptr a -> NativeWindowId
toNativeWindowId = NativeWindowId . castPtr
fromNativeWindowId :: NativeWindowId -> Ptr a
fromNativeWindowId = castPtr . unNativeWindowId
nativeWindowIdNone :: NativeWindowId
nativeWindowIdNone = NativeWindowId nullPtr
#else
newtype NativeWindowId = NativeWindowId #{gtk2hs_type GdkNativeWindow} deriving (Eq, Show)
unNativeWindowId :: Integral a => NativeWindowId -> a
unNativeWindowId (NativeWindowId id) = fromIntegral id
toNativeWindowId :: Integral a => a -> NativeWindowId
toNativeWindowId = NativeWindowId . fromIntegral
fromNativeWindowId :: Integral a => NativeWindowId -> a
fromNativeWindowId = fromIntegral . unNativeWindowId
nativeWindowIdNone :: NativeWindowId
nativeWindowIdNone = NativeWindowId 0
#endif
#endif

#ifndef DISABLE_DEPRECATED
-- Static values for different Toolbar widgets.
--
-- * c2hs and hsc should agree on types!
--
toolbarChildButton, toolbarChildToggleButton, toolbarChildRadioButton ::
  CInt -- \#gtk2hs_type GtkToolbarChildType
toolbarChildButton       = #const GTK_TOOLBAR_CHILD_BUTTON
toolbarChildToggleButton = #const GTK_TOOLBAR_CHILD_TOGGLEBUTTON
toolbarChildRadioButton  = #const GTK_TOOLBAR_CHILD_RADIOBUTTON
#endif

-- | The size of an icon in pixels.
--
-- * This enumeration contains one case that is not exported and which
--   is used when new sizes are registered using
--   'Graphics.UI.Gtk.General.IconFactory.iconSizeRegister'.
--
-- * Applying 'show' to this type will reveal the name of the size
--   that is registered with Gtk+.
--
data IconSize
  -- | Don't scale but use any of the available sizes.
  = IconSizeInvalid

  -- | Icon size to use in next to menu items in drop-down menus.
  | IconSizeMenu

  -- | Icon size for small toolbars.
  | IconSizeSmallToolbar

  -- | Icon size for larger toolbars.
  | IconSizeLargeToolbar

  -- | Icon size for icons in buttons, next to the label.
  | IconSizeButton

  -- | Icon size for icons in drag-and-drop.
  | IconSizeDnd

  -- | Icon size for icons next to dialog text.
  | IconSizeDialog
  
  | IconSizeUser Int
  deriving (Eq)

instance Enum IconSize where
  toEnum #{const GTK_ICON_SIZE_INVALID} = IconSizeInvalid
  toEnum #{const GTK_ICON_SIZE_MENU}    = IconSizeMenu
  toEnum #{const GTK_ICON_SIZE_SMALL_TOOLBAR} = IconSizeSmallToolbar
  toEnum #{const GTK_ICON_SIZE_LARGE_TOOLBAR} = IconSizeLargeToolbar
  toEnum #{const GTK_ICON_SIZE_BUTTON} = IconSizeButton
  toEnum #{const GTK_ICON_SIZE_DND} = IconSizeDnd
  toEnum #{const GTK_ICON_SIZE_DIALOG} = IconSizeDialog
  toEnum n = IconSizeUser n
  fromEnum IconSizeInvalid = #{const GTK_ICON_SIZE_INVALID}
  fromEnum IconSizeMenu = #{const GTK_ICON_SIZE_MENU}   
  fromEnum IconSizeSmallToolbar = #{const GTK_ICON_SIZE_SMALL_TOOLBAR}
  fromEnum IconSizeLargeToolbar = #{const GTK_ICON_SIZE_LARGE_TOOLBAR}
  fromEnum IconSizeButton = #{const GTK_ICON_SIZE_BUTTON}
  fromEnum IconSizeDnd = #{const GTK_ICON_SIZE_DND}
  fromEnum IconSizeDialog = #{const GTK_ICON_SIZE_DIALOG}
  fromEnum (IconSizeUser n) = n
  
-- entry Widget Combo

#ifndef DISABLE_DEPRECATED
-- | Extract the List container from a 'Combo' box.
--
comboGetList :: Combo -> IO List
comboGetList c = withForeignPtr (unCombo c) $ \cPtr ->
  makeNewObject mkList $ #{peek GtkCombo, list} cPtr
#endif

-- FileSelection related methods

-- | Extract the buttons of a fileselection.
--
fileSelectionGetButtons :: FileSelectionClass fsel => fsel -> 
			   IO (Button, Button)
fileSelectionGetButtons fsel =
    do
    ok <- butPtrToButton #{peek GtkFileSelection, ok_button}
    cancel <- butPtrToButton #{peek GtkFileSelection, cancel_button}
    return (ok,cancel)
  where
  butPtrToButton bp = makeNewObject mkButton $ liftM castPtr $
      withForeignPtr ((unFileSelection . toFileSelection) fsel) bp

-- DrawingArea related methods

-- | Retrieves the 'DrawWindow' that the widget draws onto.
--
-- This function thows an error if the widget has not yet been realized, since
-- a widget does not allocate its window resources until just before it is
-- displayed on the screen. You can use the
-- 'Graphics.UI.Gtk.Abstract.Widget.onRealize' signal to give you the
-- opportunity to use a widget's 'DrawWindow' as soon as it has been created
-- but before the widget is displayed.
--
widgetGetDrawWindow :: WidgetClass widget => widget -> IO DrawWindow
widgetGetDrawWindow da =
  withForeignPtr (unWidget.toWidget $ da) $ \da' -> do
  drawWindowPtr <- #{peek GtkWidget, window} da'
  if drawWindowPtr == nullPtr
    then fail "widgetGetDrawWindow: no DrawWindow available (the widget is probably not realized)"
    else makeNewGObject mkDrawWindow (return $ castPtr drawWindowPtr)

-- | Returns the current size.
--
-- * This information may be out of date if the user is resizing the window.
--
widgetGetSize :: WidgetClass widget => widget -> IO (Int, Int)
widgetGetSize da = withForeignPtr (unWidget.toWidget $ da) $ \wPtr -> do
    (width :: #{gtk2hs_type gint}) <- #{peek GtkAllocation, width} 
			       (#{ptr GtkWidget, allocation} wPtr)
    (height :: #{gtk2hs_type gint}) <- #{peek GtkAllocation, height}
				(#{ptr GtkWidget, allocation} wPtr)
    return (fromIntegral width, fromIntegral height)

-- Layout related methods

-- | Retrieves the 'Drawable' part.
--
layoutGetDrawWindow :: Layout -> IO DrawWindow
layoutGetDrawWindow lay = makeNewGObject mkDrawWindow $
  withForeignPtr (unLayout lay) $
  \lay' -> liftM castPtr $ #{peek GtkLayout, bin_window} lay'

-- Styles related methods

-- | Retrieve the the foreground color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetForeground :: Style -> StateType -> IO Color
styleGetForeground st ty =
  withForeignPtr (unStyle st) $ \stPtr -> do
    peekElemOff (#{ptr GtkStyle, fg} stPtr) (fromEnum ty)

-- | Retrieve the background color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetBackground :: Style -> StateType -> IO Color
styleGetBackground st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, bg} stPtr) (fromEnum ty)

-- | Retrieve a light color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetLight :: Style -> StateType -> IO Color
styleGetLight st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, light} stPtr) (fromEnum ty)

-- | Retrieve a middle color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetMiddle :: Style -> StateType -> IO Color
styleGetMiddle st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, mid} stPtr) (fromEnum ty)

-- | Retrieve a dark color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetDark :: Style -> StateType -> IO Color
styleGetDark st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, dark} stPtr) (fromEnum ty)

-- | Retrieve the text color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetText :: Style -> StateType -> IO Color
styleGetText st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, text} stPtr) (fromEnum ty)

-- | Retrieve the base color.
--
-- * The base color is the standard text background of a widget.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetBase :: Style -> StateType -> IO Color
styleGetBase st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, base} stPtr) (fromEnum ty)

-- | Retrieve the color for drawing anti-aliased text.
--
-- * The anti-aliasing color is the color which is used when the rendering
--   of a character does not make it clear if a certain pixel shoud be set
--   or not. This color is between the text and the base color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetAntiAliasing :: Style -> StateType -> IO Color
styleGetAntiAliasing st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff (#{ptr GtkStyle, text_aa} stPtr) (fromEnum ty)

-- | Retrieve the ColorSelection object contained within the dialog.
colorSelectionDialogGetColor :: ColorSelectionDialog -> IO ColorSelection
colorSelectionDialogGetColor cd =
  makeNewObject mkColorSelection $ liftM castPtr $
    withForeignPtr (unColorSelectionDialog cd)
      #{peek GtkColorSelectionDialog, colorsel}

-- | Retrieve the OK button widget contained within the dialog.
colorSelectionDialogGetOkButton :: ColorSelectionDialog -> IO Button
colorSelectionDialogGetOkButton cd =
  makeNewObject mkButton $ liftM castPtr $
    withForeignPtr (unColorSelectionDialog cd)
      #{peek GtkColorSelectionDialog, ok_button}

-- | Retrieve the Cancel button widget contained within the dialog.
colorSelectionDialogGetCancelButton :: ColorSelectionDialog -> IO Button
colorSelectionDialogGetCancelButton cd =
  makeNewObject mkButton $ liftM castPtr $
    withForeignPtr (unColorSelectionDialog cd)
      #{peek GtkColorSelectionDialog, cancel_button}

-- | Retrieve the Help button widget contained within the dialog.
colorSelectionDialogGetHelpButton :: ColorSelectionDialog -> IO Button
colorSelectionDialogGetHelpButton cd =
  makeNewObject mkButton $ liftM castPtr $
    withForeignPtr (unColorSelectionDialog cd)
      #{peek GtkColorSelectionDialog, help_button}

dragContextGetActions :: DragContext -> IO Int
dragContextGetActions dc = liftM (fromIntegral :: #{gtk2hs_type int} -> Int) $
  withForeignPtr (unDragContext dc) #{peek GdkDragContext, actions}

dragContextSetActions :: DragContext -> Int -> IO ()
dragContextSetActions dc val = withForeignPtr (unDragContext dc) $ \ptr ->
  #{poke GdkDragContext, actions} ptr (fromIntegral val :: #{gtk2hs_type int})

dragContextGetAction :: DragContext -> IO Int
dragContextGetAction dc = liftM (fromIntegral :: #{gtk2hs_type int} -> Int) $
  withForeignPtr (unDragContext dc) #{peek GdkDragContext, action}

dragContextSetAction :: DragContext -> Int -> IO ()
dragContextSetAction dc val = withForeignPtr (unDragContext dc) $ \ptr ->
  #{poke GdkDragContext, action} ptr (fromIntegral val :: #{gtk2hs_type int})

dragContextGetSuggestedAction :: DragContext -> IO Int
dragContextGetSuggestedAction dc = liftM (fromIntegral :: #{gtk2hs_type int} -> Int) $
  withForeignPtr (unDragContext dc) #{peek GdkDragContext, suggested_action}

dragContextSetSuggestedAction :: DragContext -> Int -> IO ()
dragContextSetSuggestedAction dc val = withForeignPtr (unDragContext dc) $ \ptr ->
  #{poke GdkDragContext, suggested_action} ptr (fromIntegral val :: #{gtk2hs_type int})

-- | ID number of a sort column.
--
-- * A 'SortColumnId' is a logical number to which a sorting function can
--   be associated. The number does not have to coincide with any column
--   number.
type SortColumnId = Int

-- | A special 'SortColumnId' to indicated that the default sorting function is used.
--
treeSortableDefaultSortColumnId :: SortColumnId
treeSortableDefaultSortColumnId = #{const GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID}

intToAtom :: Int -> Atom
intToAtom = Atom . plusPtr nullPtr

-- | An invalid 'TargetTag', 'SelectionTag', 'SelectionTypeTag' or 'PropertyTag'.
--
tagInvalid :: Atom
tagInvalid = intToAtom #{const GDK_NONE}

-- | The primary selection (the currently highlighted text in X11 that can
--   in many applications be pasted using the middle button).
selectionPrimary :: SelectionTag
selectionPrimary = intToAtom #{const GDK_SELECTION_PRIMARY}

-- | The secondary selection. Rarely used.
selectionSecondary :: SelectionTag
selectionSecondary = intToAtom #{const GDK_SELECTION_SECONDARY}

-- | The modern clipboard that is filled by copy or cut commands.
selectionClipboard :: SelectionTag
selectionClipboard = intToAtom #{const GDK_SELECTION_CLIPBOARD}

-- | If this target is provided by a selection, then the data is a string.
targetString :: TargetTag
targetString = intToAtom #{const GDK_TARGET_STRING}

-- | The type indicating that the associated data is itself a (list of)
-- 'Graphics.UI.Gtk.General.Selection.Atom's.
selectionTypeAtom :: SelectionTypeTag
selectionTypeAtom = intToAtom #{const GDK_SELECTION_TYPE_ATOM}

-- | The type indicating that the associated data consists of integers.
selectionTypeInteger :: SelectionTypeTag
selectionTypeInteger = intToAtom #{const GDK_SELECTION_TYPE_INTEGER}

-- | The type indicating that the associated data is a string without further
-- information on its encoding.
selectionTypeString :: SelectionTypeTag
selectionTypeString = intToAtom #{const GDK_SELECTION_TYPE_STRING}

-- | Extract the type field of SelectionData*. This should be in the
--   Selection modules but c2hs chokes on the 'type' field.
selectionDataGetType :: Ptr () -> IO SelectionTypeTag
selectionDataGetType selPtr =
  liftM intToAtom $ #{peek GtkSelectionData, type} selPtr

-- A type that identifies a target. This is needed to marshal arrays of
-- GtkTargetEntries.
data TargetEntry = TargetEntry (Ptr #{gtk2hs_type gchar}) InfoId

-- brain damaged API: the whole selection API doesn't need GtkTargetEntry
-- structure, but stupid Clipboard has two functions that only provide this
-- interface. Thus, convert the efficient Atoms back into strings, have
-- the clipboard functions convert them back to string before we get a
-- chance to free the freshly allocated strings.

withTargetEntries :: [(TargetTag, InfoId)] -> (Int -> Ptr () -> IO a) -> IO a
withTargetEntries tags fun = do
  ptrsInfo <- mapM (\(Atom tag, info) -> gdk_atom_name tag >>= \strPtr ->
                     return (TargetEntry strPtr info)) tags
  let len = length tags
  res <- withArrayLen ptrsInfo (\len ptr -> fun len (castPtr ptr))
  mapM_ (\(TargetEntry ptr _) -> g_free ptr) ptrsInfo
  return res

foreign import ccall unsafe "gdk_atom_name"
  gdk_atom_name :: Ptr () -> IO (Ptr #{gtk2hs_type gchar})

foreign import ccall unsafe "g_free"
  g_free :: Ptr #{gtk2hs_type gchar} -> IO ()

instance Storable TargetEntry where
  sizeOf _ = #{const sizeof(GtkTargetEntry)}
  alignment _ = alignment (undefined::#gtk2hs_type guint32)
  peek ptr = undefined
  poke ptr (TargetEntry cPtr info) = do
    #{poke GtkTargetEntry, target} ptr cPtr
    #{poke GtkTargetEntry, flags} ptr (0::#{gtk2hs_type guint})
    #{poke GtkTargetEntry, info} ptr info
