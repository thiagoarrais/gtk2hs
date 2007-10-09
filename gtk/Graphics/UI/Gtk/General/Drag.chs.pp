-- -*-haskell-*-
--  GIMP Toolkit (GTK) Drag-and-Drop functionality
--
--  Author : Axel Simon
--
--  Created: 26 March 2007
--
--  Copyright (C) 2007 Axel Simon
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
--
-- functions not bound:
-- dragBegin : necessary to implement custom widgets that may be the source of
-- drags. Would need to pass an event and an array of targets. The event needs
-- to have the following information:  Motion {
--  eventTime  :: TimeStamp,
--  eventModifier   :: [Modifier],
--  eventIsHint  (this needs to be True in order to avoid gdk_event_get_screen to be called (which causes havoc))
--  eventXRoot,
--  eventYRoot  :: Double } 
-- Button {
--  eventClick  :: Click,
--  eventTime  :: TimeStamp,
--  eventModifier  :: [Modifier],
-- Key {
--  eventTime  :: TimeStamp,
--  eventModifier  :: [Modifier],
-- Crossing {
--  eventTime  :: TimeStamp,
--  eventModifier  :: [Modifier]}
--
-- drag_set_icon_pixmap : colormaps are a pain, they migth be useful here
-- drag_set_default_icon : obsolete drag_source_set_icon : colormap problem
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Drag-and-Drop functionality.
--
-- GTK+ has a rich set of functions for doing inter-process communication via
-- the drag-and-drop metaphor. GTK+ can do drag-and-drop (DND) via multiple
-- protocols. The currently supported protocols are the Xdnd and Motif
-- protocols. As well as the functions listed here, applications may need to
-- use some facilities provided for 'Selection's. Also, the Drag and Drop API
-- makes use of signals in the 'Widget'  class.
--
module Graphics.UI.Gtk.General.Drag (

-- * Types
  DragContext,
  DragContextClass,
  castToDragContext,
  toDragContext,
  
-- * Methods
  dragContextActions,
  dragContextSuggestedAction,
  dragContextAction,
  
  dragDestSet,
  dragDestSetProxy,
  dragDestUnset,
  dragDestFindTarget,
  dragDestGetTargetList,
  dragDestSetTargetList,
#if GTK_CHECK_VERSION(2,6,0)
  dragDestAddTextTargets,
  dragDestAddImageTargets,
  dragDestAddURITargets,
#endif
  dragFinish,
  dragGetData,
  dragGetSourceWidget,
  dragHighlight,
  dragUnhighlight,
  dragSetIconWidget,
  dragSetIconPixbuf,
  dragSetIconStock,
#if GTK_CHECK_VERSION(2,8,0)
  dragSetIconName,
#endif
  dragSetIconDefault,
  dragCheckThreshold,
  dragSourceSet,
  dragSourceSetIconPixbuf,
  dragSourceSetIconStock,
#if GTK_CHECK_VERSION(2,8,0)
  dragSourceSetIconName,
#endif
  dragSourceUnset,
#if GTK_CHECK_VERSION(2,8,0)
  dragSourceSetTargetList,
  dragSourceGetTargetList,
#endif
#if GTK_CHECK_VERSION(2,6,0)
  dragSourceAddTextTargets,
  dragSourceAddImageTargets,
  dragSourceAddURITargets,
#endif

  -- * Signals
  dragBegin,
  dragDataDelete,
  dragDataGet,
  dragDataReceived,
  dragDrop,
  dragEnd,
#if GTK_CHECK_VERSION(2,12,0)
  dragFailed,
#endif
  dragLeave,
  dragMotion
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.Flags
import System.Glib.UTFString ( withUTFString )
import System.Glib.GObject		(makeNewGObject)
import System.Glib.Attributes ( Attr, newAttr )
import Graphics.UI.Gtk.General.StockItems ( StockId )
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.General.DNDTypes#}
{#import Graphics.UI.Gtk.General.Selection#} ( TargetList )
import Graphics.UI.Gtk.General.Enums ( DestDefaults(..), DragProtocol(..) )
import Graphics.UI.Gtk.Gdk.Events ( TimeStamp, Modifier )
import Graphics.UI.Gtk.General.Structs ( Point, 
  dragContextGetActions, dragContextSetActions,
  dragContextGetSuggestedAction, dragContextSetSuggestedAction,
  dragContextGetAction, dragContextSetAction )
import Graphics.UI.Gtk.Signals
import Control.Monad.Reader (runReaderT)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Types

-- | Used in 'DragContext' to indicate what the destination should do with the
-- dropped data.
--
-- * 'ActionDefault': Initialisation value, should not be used.
-- * 'ActionCopy': Copy the data.
-- * 'ActionMove': Move the data, i.e. first copy it, then delete it from the source using
--   the DELETE target of the X selection protocol.
-- * 'ActionLink':  Add a link to the data. Note that this is only useful if source and
--   destination agree on what it means.
-- * 'ActionPrivate': Special action which tells the source that the destination will do
--   something that the source doesn't understand.
-- * 'ActionAsk': Ask the user what to do with the data.

{#enum GdkDragAction as DragAction {underscoreToCase} deriving (Bounded) #} 

instance Flags DragAction
  
--------------------
-- Methods

-- | A set of actions that the source recommends to be taken. Only valid if
--   'dragContextSuggestedAction' is set to 'ActionAsk'.
--
dragContextActions :: Attr DragContext [DragAction]
dragContextActions = newAttr (liftM toFlags . dragContextGetActions)
                             (\o -> dragContextSetActions o . fromFlags)

-- | The action suggested by the source.
dragContextSuggestedAction :: Attr DragContext DragAction
dragContextSuggestedAction = newAttr (liftM toEnum . dragContextGetSuggestedAction)
                                     (\o -> dragContextSetSuggestedAction o . fromEnum)

-- | The action chosen by the destination.
dragContextAction :: Attr DragContext DragAction
dragContextAction = newAttr (liftM toEnum . dragContextGetAction)
                            (\o -> dragContextSetAction o . fromEnum)

-- %hash c:4ff5 d:af3f
-- | Sets a widget as a potential drop destination.
--
-- *  The 'DestDefaults' flags specify what actions GTK+ should take on behalf
--   of a widget for drops onto that widget. The given actions and any targets
--   set through 'dragDestSetTargetList' only are used if 'DestDefaultMotion'
--   or 'DestDefaultDrop' are given.
--
dragDestSet :: WidgetClass widget => widget -> [DestDefaults] -> [DragAction] -> IO ()
dragDestSet widget flags actions =
  {# call gtk_drag_dest_set #}
    (toWidget widget)
    ((fromIntegral . fromFlags) flags)
    nullPtr 0
    ((fromIntegral . fromFlags) actions)

-- %hash c:89d2 d:af3f
-- | Sets this widget as a proxy for drops to another window.
--
dragDestSetProxy :: WidgetClass widget => widget
  -> DrawWindow -- ^ The window to which to forward drag events.
  -> DragProtocol -- ^ The drag protocol which the 'DrawWindow' accepts.
  -> Bool -- ^ If @True@, send the same coordinates to the destination,
          -- because it is an embedded subwindow.
  -> IO ()
dragDestSetProxy widget proxyWindow protocol useCoordinates =
  {# call gtk_drag_dest_set_proxy #}
    (toWidget widget)
    proxyWindow
    ((fromIntegral . fromEnum) protocol)
    (fromBool useCoordinates)

-- %hash c:f319 d:af3f
-- | Clears information about a drop destination set with 'dragDestSet'. The
-- widget will no longer receive notification of drags.
--
dragDestUnset :: WidgetClass widget => widget -> IO ()
dragDestUnset widget =
  {# call gtk_drag_dest_unset #}
    (toWidget widget)

-- %hash c:db53 d:af3f
-- | Looks for a match between the targets mentioned in the context and the
-- 'TargetList', returning the first matching target, otherwise returning
-- @Nothing@. If @Nothing@ is given as target list, use the value from
-- 'destGetTargetList'. Some widgets may have different valid targets for
-- different parts of the widget; in that case, they will have to implement a
-- 'dragMotion' handler that passes the correct target list to this
-- function.
--
dragDestFindTarget :: (WidgetClass widget, DragContextClass context) =>
  widget -> context -> Maybe TargetList -> IO (Maybe TargetTag)
dragDestFindTarget widget context (Just targetList) = do
  ttPtr <-
    {# call gtk_drag_dest_find_target #}
    (toWidget widget)
    (toDragContext context)
    targetList
  if ttPtr==nullPtr then return Nothing else return (Just (TargetTag ttPtr))
dragDestFindTarget widget context Nothing = do
  ttPtr <-
    {# call gtk_drag_dest_find_target #}
    (toWidget widget)
    (toDragContext context)
    (TargetList nullForeignPtr)
  if ttPtr==nullPtr then return Nothing else return (Just (TargetTag ttPtr))
	
-- %hash c:41c7 d:af3f
-- | Returns the list of targets this widget can accept from drag-and-drop.
--
dragDestGetTargetList :: WidgetClass widget => widget -> IO (Maybe TargetList)
dragDestGetTargetList widget = do
  tlPtr <- {# call gtk_drag_dest_get_target_list #} (toWidget widget)
  if tlPtr==nullPtr then return Nothing else liftM Just (mkTargetList tlPtr)
  
-- %hash c:5c89 d:af3f
-- | Sets the target types that this widget can accept from drag-and-drop. The
-- widget must first be made into a drag destination with 'dragDestSet'.
--
dragDestSetTargetList :: WidgetClass widget => widget -> TargetList -> IO ()
dragDestSetTargetList widget targetList =
  {# call gtk_drag_dest_set_target_list #}
    (toWidget widget)
    targetList

#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:36c2 d:af3f
-- | Add the text targets supported by the selection mechanism to the target
-- list of the drag source. The targets are added with an 'InfoId' of 0. If
-- you need another value, use 'Graphics.UI.Gtk.General.Selection.targetListAddTextTargets' and
-- 'dragSourceSetTargetList'.
--
dragDestAddTextTargets :: WidgetClass widget => widget -> IO ()
dragDestAddTextTargets widget =
  {# call gtk_drag_dest_add_text_targets #}
    (toWidget widget)

-- %hash c:691c d:af3f
-- | Add image targets supported by the selection mechanism to the target list
-- of the drag source. The targets are added with an 'InfoId' of 0. If you
-- need another value, use
-- 'Graphics.UI.Gtk.General.Selection.targetListAddTextTargets' and
-- 'dragSourceSetTargetList'.
--
dragDestAddImageTargets :: WidgetClass widget => widget -> IO ()
dragDestAddImageTargets widget =
  {# call gtk_drag_dest_add_image_targets #}
    (toWidget widget)

-- %hash c:6f83 d:af3f
-- | Add URI targets supported by the selection mechanism to the target list
-- of the drag source. The targets are added with an 'InfoId' of 0. If you
-- need another value, use
-- 'Graphics.UI.Gtk.General.Selection.targetListAddTextTargets' and
-- 'dragSourceSetTargetList'.
--
dragDestAddURITargets :: WidgetClass widget => widget -> IO ()
dragDestAddURITargets widget =
  {# call gtk_drag_dest_add_uri_targets #}
    (toWidget widget)

#endif

-- %hash c:a91 d:af3f
-- | Informs the drag source that the drop is finished, and that the data of
-- the drag will no longer be required.
--
dragFinish :: DragContextClass context => context
  -> Bool -- ^ a flag indicating whether the drop was successful
  -> Bool -- ^ a flag indicating whether the source should delete the original data.
  -- (This should be @True@ for a move)
  -> TimeStamp -- ^ the timestamp from the 'dragDrop' signal.
  -> IO ()
dragFinish context success del time =
  {# call gtk_drag_finish #}
    (toDragContext context)
    (fromBool success)
    (fromBool del)
    (fromIntegral time)

-- %hash c:a37d d:af3f
-- | Gets the data associated with a drag. When the data is received or the
-- retrieval fails, GTK+ will emit a 'dragDataReceived' signal. Failure of
-- the retrieval is indicated by passing @Nothing@ in the 'selectionData' signal.
-- However, when 'dragGetData' is called
-- implicitely because the 'DestDefaultDrop' was set, then the widget will
-- not receive notification of failed drops.
--
dragGetData :: (WidgetClass widget, DragContextClass context) 
  => widget -- ^ The widget that will receive the 'dragDataReceived' signal.
  -> context 
  -> TargetTag -- ^ The target (form of the data) to retrieve.
  -> TimeStamp -- ^ A timestamp for retrieving the data. This will generally be
               -- the time received in a 'dragMotion' or 'dragDrop' signal.
  -> IO ()
dragGetData widget context (TargetTag target) time =
  {# call gtk_drag_get_data #}
    (toWidget widget)
    (toDragContext context)
    target
    (fromIntegral time)

-- %hash c:8c18 d:af3f
-- | Queries he source widget for a drag.
--
-- * If the drag is occurring within a single application, a pointer to the
--   source widget is returned. Otherwise the return value is @Nothing@.
--
dragGetSourceWidget :: DragContextClass context => context -> IO (Maybe Widget)
dragGetSourceWidget context =
  maybeNull (makeNewGObject mkWidget) $
    {# call gtk_drag_get_source_widget #}
    (toDragContext context)

-- %hash c:1765 d:af3f
-- | Draws a highlight around a widget. This will attach handlers to
-- the expose handlers, so the highlight will continue to be displayed
-- until 'dragUnhighlight' is called.
--
dragHighlight :: WidgetClass widget => widget -> IO ()
dragHighlight widget =
  {# call gtk_drag_highlight #}
    (toWidget widget)

-- %hash c:f00e d:af3f
-- | Removes a highlight set by 'dragHighlight' from a widget.
--
dragUnhighlight :: WidgetClass widget => widget -> IO ()
dragUnhighlight widget =
  {# call gtk_drag_unhighlight #}
    (toWidget widget)

-- %hash c:f20 d:af3f
-- | Changes the icon for a drag to a given widget. GTK+ will not destroy
-- the widget, so if you don't want it to persist, you should connect to the
-- 'dragEnd' signal and destroy it yourself.
--
-- * The function must be called with the context of the source side.
--
dragSetIconWidget :: (DragContextClass context, WidgetClass widget) =>
  context -> widget
  -> Int -- ^ x hot-spot
  -> Int -- ^ y hot-spot
  -> IO ()
dragSetIconWidget context widget hotX hotY =
  {# call gtk_drag_set_icon_widget #}
    (toDragContext context)
    (toWidget widget)
    (fromIntegral hotX)
    (fromIntegral hotY)

-- %hash c:69 d:af3f
-- | Set the given 'Pixbuf' as the icon for the given drag.
--
dragSetIconPixbuf :: DragContextClass context => context -> Pixbuf
  -> Int -- ^ x hot-spot
  -> Int -- ^ y hot-spot
  -> IO ()
dragSetIconPixbuf context pixbuf hotX hotY =
  {# call gtk_drag_set_icon_pixbuf #}
    (toDragContext context)
    pixbuf
    (fromIntegral hotX)
    (fromIntegral hotY)

-- %hash c:f73f d:af3f
-- | Sets the icon for a given drag from a stock ID.
--
dragSetIconStock :: DragContextClass context => context -> StockId 
  -> Int -- ^ x hot-spot
  -> Int -- ^ y hot-spot
  -> IO ()
dragSetIconStock context stockId hotX hotY =
  withUTFString stockId $ \stockIdPtr ->
  {# call gtk_drag_set_icon_stock #}
    (toDragContext context)
    stockIdPtr
    (fromIntegral hotX)
    (fromIntegral hotY)

#if GTK_CHECK_VERSION(2,8,0)
-- %hash c:1eba d:af3f
-- | Sets the icon for a given drag from a named themed icon. See the docs for
-- 'IconTheme' for more details. Note that the size of the icon depends on the
-- icon theme (the icon is loaded at the DND size), thus x and y hot-spots
-- have to be used with care. Since Gtk 2.8.
--
dragSetIconName :: DragContextClass context => context 
  -> String
  -> Int -- ^ x hot-spot
  -> Int -- ^ y hot-spot
  -> IO ()
dragSetIconName context iconName hotX hotY =
  withUTFString iconName $ \iconNamePtr ->
  {# call gtk_drag_set_icon_name #}
    (toDragContext context)
    iconNamePtr
    (fromIntegral hotX)
    (fromIntegral hotY)
#endif

-- %hash c:2beb d:af3f
-- | Sets the icon for a particular drag to the default icon. This function
-- must be called with a context for the source side of a drag
--
dragSetIconDefault :: DragContextClass context => context -> IO ()
dragSetIconDefault context =
  {# call gtk_drag_set_icon_default #}
    (toDragContext context)

-- %hash c:5785 d:af3f
-- | Checks to see if a mouse drag starting at @(startX, startY)@ and ending
-- at @(currentX, currenty)@ has passed the GTK+ drag threshold, and thus
-- should trigger the beginning of a drag-and-drop operation.
--
dragCheckThreshold :: WidgetClass widget => widget
                      -> Int -- ^ @startX@
                      -> Int -- ^ @startY@
                      -> Int -- ^ @currentX@
                      -> Int -- ^ @currentY@
                      -> IO Bool
dragCheckThreshold widget startX startY currentX currentY =
  liftM toBool $
  {# call gtk_drag_check_threshold #}
    (toWidget widget)
    (fromIntegral startX)
    (fromIntegral startY)
    (fromIntegral currentX)
    (fromIntegral currentY)

-- %hash c:ce13 d:af3f
-- | Sets up a widget so that GTK+ will start a drag operation when the user
-- clicks and drags on the widget. The widget must have a window. Note that a
-- set of possible targets have to be set for a drag to be successful.
--
dragSourceSet :: WidgetClass widget => widget -> [Modifier] -> [DragAction] -> IO ()
dragSourceSet widget startButtonMask actions =
  {# call gtk_drag_source_set #}
    (toWidget widget)
    ((fromIntegral . fromFlags) startButtonMask)
    nullPtr
    0
    ((fromIntegral . fromFlags) actions)

-- %hash c:63f5 d:af3f
-- | Sets the icon that will be used for drags from a particular widget from a
-- 'Pixbuf'. 
--
dragSourceSetIconPixbuf :: WidgetClass widget => widget -> Pixbuf -> IO ()
dragSourceSetIconPixbuf widget pixbuf =
  {# call gtk_drag_source_set_icon_pixbuf #}
    (toWidget widget)
    pixbuf

-- %hash c:b38b d:af3f
-- | Sets the icon that will be used for drags from a particular source to a
-- stock icon.
--
dragSourceSetIconStock :: WidgetClass widget => widget -> StockId -> IO ()
dragSourceSetIconStock widget stockId =
  withUTFString stockId $ \stockIdPtr ->
  {# call gtk_drag_source_set_icon_stock #}
    (toWidget widget)
    stockIdPtr

#if GTK_CHECK_VERSION(2,8,0)
-- %hash c:1786 d:af3f
-- | Sets the icon that will be used for drags from a particular source to a
-- themed icon. See the docs for 'IconTheme' for more details.
--
dragSourceSetIconName :: WidgetClass widget => widget -> String -> IO ()
dragSourceSetIconName widget iconName =
  withUTFString iconName $ \iconNamePtr ->
  {# call gtk_drag_source_set_icon_name #}
    (toWidget widget)
    iconNamePtr
#endif

-- %hash c:653c d:af3f
-- | Undoes the effects of 'dragSourceSet'.
--
dragSourceUnset :: WidgetClass widget => widget -> IO ()
dragSourceUnset widget =
  {# call gtk_drag_source_unset #}
    (toWidget widget)

#if GTK_CHECK_VERSION(2,8,0)
-- %hash c:facc d:af3f
-- | Changes the target types that this widget offers for drag-and-drop. The
-- widget must first be made into a drag source with 'dragSourceSet'.
--
-- * Since Gtk 2.4.
--
dragSourceSetTargetList :: WidgetClass widget => widget -> TargetList -> IO ()
dragSourceSetTargetList widget targetList =
  {# call gtk_drag_source_set_target_list #}
    (toWidget widget)
    targetList

-- %hash c:e9aa d:af3f
-- | Gets the list of targets this widget can provide for drag-and-drop.
--
-- * Since Gtk 2.4.
--
dragSourceGetTargetList :: WidgetClass widget => widget -> IO (Maybe TargetList)
dragSourceGetTargetList widget = do
  tlPtr <- {# call gtk_drag_source_get_target_list #}  (toWidget widget)
  if tlPtr==nullPtr then return Nothing else liftM Just (mkTargetList tlPtr)
#endif

#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:1f25 d:af3f
-- | Add the text targets supported by
-- 'Graphics.UI.Gtk.General.Selection.Selection' to the target list of
-- the drag source. The targets are added with @info = 0@. If you need
-- another value, use
-- 'Graphics.UI.Gtk.General.Selection.targetListAddTextTargets' and
-- 'dragSourceSetTargetList'.
--
-- * Since Gtk 2.6.
--
dragSourceAddTextTargets :: WidgetClass widget => widget -> IO ()
dragSourceAddTextTargets widget =
  {# call gtk_drag_source_add_text_targets #}
    (toWidget widget)

-- %hash c:44bf d:af3f
-- | Add the image targets supported by 'Selection' to the target list of the
-- drag source. The targets are added with @info = 0@. If you need another
-- value, use 'Graphics.UI.Gtk.General.Selection.targetListAddTextTargets' and 'dragSourceSetTargetList'.
--
-- * Since Gtk 2.6.
--
dragSourceAddImageTargets :: WidgetClass widget => widget -> IO ()
dragSourceAddImageTargets widget =
  {# call gtk_drag_source_add_image_targets #}
    (toWidget widget)

-- %hash c:4766 d:af3f
-- | Add the URI targets supported by 'Selection' to the target list of the
-- drag source. The targets are added with @info = 0@. If you need another
-- value, use 'Graphics.UI.Gtk.General.Selection.targetListAddTextTargets' and 'dragSourceSetTargetList'.
--
-- * Since Gtk 2.6.
--
dragSourceAddURITargets :: WidgetClass widget => widget -> IO ()
dragSourceAddURITargets widget =
  {# call gtk_drag_source_add_uri_targets #}
    (toWidget widget)
#endif

-- %hash c:fcf8 d:b945
-- | The 'dragBegin' signal is emitted on the drag source when a drag is
-- started. A typical reason to connect to this signal is to set up a custom
-- drag icon with 'dragSourceSetIcon'.
--
dragBegin :: WidgetClass self => Signal self (DragContext -> IO ())
dragBegin = Signal (connect_OBJECT__NONE "drag_begin")

-- %hash c:bfef d:a2ff
-- | The 'dragDataDelete' signal is emitted on the drag source when a drag
-- with the action 'ActionMove' is successfully completed. The signal handler
-- is responsible for deleting the data that has been dropped. What \"delete\"
-- means, depends on the context of the drag operation.
--
dragDataDelete :: WidgetClass self => Signal self (DragContext -> IO ())
dragDataDelete = Signal (connect_OBJECT__NONE "drag_data_delete")

-- %hash c:eb9c d:844c
-- | The ::drag-data-get signal is emitted on the drag source when the
-- drop site requests the data which is dragged. It is the
-- responsibility of the signal handler to set the selection data in
-- the format which is indicated by 'InfoId'. See
-- 'Graphics.UI.Gtk.General.Selection.selectionDataSet' and
-- 'Graphics.UI.Gtk.General.Selection.selectionDataSetText'.
--
dragDataGet :: WidgetClass self =>
  Signal self (DragContext -> InfoId -> TimeStamp -> SelectionDataM ())
dragDataGet = Signal (\after object handler -> do
      connect_OBJECT_PTR_WORD_WORD__NONE "drag_data_get" after object $
        \ctxt dataPtr info time -> do
        runReaderT (handler ctxt (fromIntegral info) (fromIntegral time)) dataPtr >> 
                    return ())

-- %hash c:9251 d:a6d8
-- | The 'dragDataReceived' signal is emitted on the drop site when the
-- dragged data has been received. If the data was received in order to
-- determine whether the drop will be accepted, the handler is expected to call
-- 'dragStatus' and /not/ finish the drag. If the data was received in response
-- to a 'dragDrop' signal (and this is the last target to be received), the
-- handler for this signal is expected to process the received data and then
-- call 'dragFinish', setting the @success@ parameter depending on whether the
-- data was processed successfully.
--
-- The handler may inspect and modify 'dragContextAction' before calling
-- 'dragFinish', e.g. to implement 'ActionAsk' as shown in the following
-- example:
--
dragDataReceived :: WidgetClass self =>
  Signal self (DragContext -> Point -> InfoId -> TimeStamp -> SelectionDataM ())
dragDataReceived = Signal (\after object handler -> do
  connect_OBJECT_INT_INT_PTR_WORD_WORD__NONE "drag_data_received" after object $
    \ctxt x y dataPtr info time -> do
    runReaderT (handler ctxt (fromIntegral x, fromIntegral y) (fromIntegral info)
               (fromIntegral time)) dataPtr >> return ())

-- %hash c:4ef4 d:f4b8
-- | The 'dragDrop' signal is emitted on the drop site when the user drops
-- the data onto the widget. The signal handler must determine whether the
-- cursor position is in a drop zone or not. If it is not in a drop zone, it
-- returns @False@ and no further processing is necessary. Otherwise, the
-- handler returns @True@. In this case, the handler must ensure that
-- 'dragFinish' is called to let the source know that the drop is done. The
-- call to 'dragFinish' can be done either directly or in a
-- 'dragDataReceived' handler which gets triggered by calling 'dropGetData'
-- to receive the data for one or more of the supported targets.
--
dragDrop :: WidgetClass self =>
  Signal self (DragContext -> Point -> TimeStamp -> IO Bool)
dragDrop = Signal (\after object handler ->
  connect_OBJECT_INT_INT_WORD__BOOL "drag_drop" after object $ \ctxt x y time ->
    handler ctxt (fromIntegral x, fromIntegral y) (fromIntegral time))

-- %hash c:9d4e d:a5ac
-- | The 'dragEnd' signal is emitted on the drag source when a drag is
-- finished. A typical reason to connect to this signal is to undo things done
-- in 'dragBegin'.
--
dragEnd :: WidgetClass self => Signal self (DragContext -> IO ())
dragEnd = Signal (connect_OBJECT__NONE "drag_end")

#if GTK_CHECK_VERSION(2,12,0)
dragFailed = error "dragFailed: not defined yet"
#endif

-- %hash c:4a85 d:6122
-- | The 'dragLeave' signal is emitted on the drop site when the cursor
-- leaves the widget. A typical reason to connect to this signal is to undo
-- things done in 'dragMotion', e.g. undo highlighting with 'dragUnhighlight'
--
dragLeave :: WidgetClass self => Signal self (DragContext -> TimeStamp -> IO ())
dragLeave = Signal (\after object handler ->
  connect_OBJECT_WORD__NONE "drag_leave" after object $ \ctxt time ->
    handler ctxt (fromIntegral time))

-- %hash c:53f7 d:176d
-- | The 'dragMotion' signal is emitted on the drop site when the user moves
-- the cursor over the widget during a drag. The signal handler must determine
-- whether the cursor position is in a drop zone or not. If it is not in a drop
-- zone, it returns @False@ and no further processing is necessary. Otherwise,
-- the handler returns @True@. In this case, the handler is responsible for
-- providing the necessary information for displaying feedback to the user, by
-- calling 'dragStatus'. If the decision whether the drop will be accepted or
-- rejected can't be made based solely on the cursor position and the type of
-- the data, the handler may inspect the dragged data by calling 'dragGetData'
-- and defer the 'dragStatus' call to the 'dragDataReceived' handler.
--
-- Note that there is no 'dragEnter' signal. The drag receiver has to keep
-- track of whether he has received any 'dragMotion' signals since the last
-- 'dragLeave' and if not, treat the 'dragMotion' signal as an \"enter\"
-- signal. Upon an \"enter\", the handler will typically highlight the drop
-- site with 'dragHighlight'.
--
dragMotion :: WidgetClass self =>
  Signal self (DragContext -> Point -> TimeStamp -> IO Bool)
dragMotion = Signal (\after object handler -> do
  connect_OBJECT_INT_INT_WORD__BOOL "drag_motion" after object $ \ctxt x y time ->
    handler ctxt (fromIntegral x, fromIntegral y) (fromIntegral time))

