-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Widget@
--
--  Author : Axel Simon
--          
--  Created: 27 April 2001
--
--  Version $Revision: 1.9 $ from $Date: 2002/11/08 10:39:21 $
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
--  * Widget is the base class of all widgets. It provides the methods to
--    attach and detach signals.
--
-- @documentation@ ------------------------------------------------------------
--
--  * This modules reexports everything a normal widget needs from GObject
--    and Object.
--
--- TODO ----------------------------------------------------------------------
--
--  * unimplemented methods that seem to be useful in user programs:
--      widgetSizeRequest, widgetAddAccelerator, widgetRemoveAccrelerator,
--	widgetAcceleratorSignal, widgetIntersect, widgetGrabDefault,
--	widgetGetPointer, widgetPath, widgetClassPath, getCompositeName,
--	widgetSetCompositeName,
--	widgetModifyStyle, widgetGetModifierStyle, widgetModifyFg,
--	widgetModifyBG, widgetModifyText, widgetModifyBase, widgetModifyFont,
--	widgetPango*, widgetSetAdjustments
--	
--
--  * implement the following methods in GtkWindow object:
--      widget_set_uposition, widget_set_usize
--
--  * implement the following methods in GtkDrawingArea object:
--      widgetQueueDrawArea, widgetSetDoubleBufferd, widgetRegionIntersect
--
module Widget(
  Widget,
  WidgetClass,
  castToWidget,
  Allocation,
  Requisition(..),
  Rectangle(..),
  widgetShow,			-- Showing and hiding a widget.
  widgetShowNow,
  widgetHide,
  widgetShowAll,
  widgetHideAll,
  widgetDestroy,
  widgetQueueDraw,		-- Functions to be used with DrawingArea.
  widgetHasIntersection,
  widgetActivate,		-- Manipulate widget state.
  widgetSetSensitivity,
  widgetSetSizeRequest,
  widgetIsFocus,
  widgetGrabFocus,
  widgetSetAppPaintable,
  widgetSetName,		-- Naming, Themes
  widgetGetName,
  widgetGetToplevel,		-- Widget browsing.
  widgetIsAncestor,
  widgetReparent,
  TextDirection(..),
  widgetSetDirection,		-- General Setup.
  widgetGetDirection,
--  widgetLockAccelerators,
--  widgetUnlockAccelerators,
  Event(..),
  onButtonPress,
  afterButtonPress,
  onButtonRelease,
  afterButtonRelease,
  onClient,
  afterClient,
  onConfigure,
  afterConfigure,
  onDelete,
  afterDelete,
  onDestroyEvent,		-- you probably want onDestroy
  afterDestroyEvent,
  onDirectionChanged,
  afterDirectionChanged,
  onEnterNotify,
  afterEnterNotify,
  onLeaveNotify,
  afterLeaveNotify,
  onExpose,
  afterExpose,
  onFocusIn,
  afterFocusIn,
  onFocusOut,
  afterFocusOut,
  onGrabFocus,
  afterGrabFocus,
  onDestroy,
  afterDestroy,
  onHide,
  afterHide,
  onHierarchyChanged,
  afterHierarchyChanged,
  onKeyPress,
  afterKeyPress,
  onKeyRelease,
  afterKeyRelease,
  onMnemonicActivate,
  afterMnemonicActivate,
  onMotionNotify,
  afterMotionNotify,
  onParentSet,
  afterParentSet,
  onPopupMenu,
  afterPopupMenu,
  onProximityIn,
  afterProximityIn,
  onProximityOut,
  afterProximityOut,
  onScroll,
  afterScroll,
  onShow,
  afterShow,
  onSizeAllocate,
  afterSizeAllocate,
  onSizeRequest,
  afterSizeRequest,
  StateType(..),
  onStateChanged,
  afterStateChanged,
  onUnmap,
  afterUnmap,
  onUnrealize,
  afterUnrealize,
  onVisibilityNotify,
  afterVisibilityNotify,
  onWindowState,
  afterWindowState
  ) where

import Monad	(liftM, unless)
import UTFCForeign
import Foreign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import GdkEnums
import Structs	(Allocation, Rectangle(..), Requisition(..))
import Events	(Event(..), marshalEvent)
import Enums	(StateType(..), TextDirection(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @method widgetShow@ Queue a show request.
--
-- * Flags a widget to be displayed. Any widget that isn't shown will not
--   appear on the screen. If you want to show all the widgets in a container,
--   it's easier to call @ref method widgetShowAll@ on the container, instead
--   of individually showing the widgets. Note that you have to show the
--   containers containing a widget, in addition to the widget itself, before
--   it will appear onscreen. When a toplevel container is shown, it is
--   immediately realized and mapped; other shown widgets are realized and
--   mapped when their toplevel container is realized and mapped.
--
widgetShow :: WidgetClass w => w -> IO ()
widgetShow  = {#call widget_show#}.toWidget

-- @method widgetShowNow@ Queue a show event and wait for it to be executed.
--
-- * If the widget is an unmapped toplevel widget (i.e. a @ref data Window@
--   that has not yet been shown), enter the main loop and wait for the window
--   to actually be mapped. Be careful; because the main loop is running,
--   anything can happen during this function.
--
widgetShowNow :: WidgetClass w => w -> IO ()
widgetShowNow  = {#call widget_show_now#}.toWidget

-- @method widgetHide@ Queue a hide request.
--
-- * Reverses the effects of @ref method widgetShow@, causing the widget to be
--   hidden (make invisible to the user).
--
widgetHide :: WidgetClass w => w -> IO ()
widgetHide  = {#call widget_hide#}.toWidget

-- @method widgetShowAll@ Show this and all child widgets.
--
widgetShowAll :: WidgetClass w => w -> IO ()
widgetShowAll  = {#call widget_show_all#}.toWidget

-- @method widgetHideAll@ Hide this and all child widgets.
--
widgetHideAll :: WidgetClass w => w -> IO ()
widgetHideAll  = {#call widget_hide_all#}.toWidget

-- @method widgetDestroy@ Destroy a widget.
--
-- * The @ref method widgetDestroy@ function is used to shutdown an object,
--   i.e. a widget will be removed from the screen and unrealized. Resources
--   will be freed when all references are released.
--
widgetDestroy :: WidgetClass obj => obj -> IO ()
widgetDestroy  = {#call widget_destroy#}.toWidget

-- Functions to be used with DrawingArea.

-- @method widgetQueueDraw@ Send a redraw request to a widget.
--
widgetQueueDraw :: WidgetClass w => w -> IO ()
widgetQueueDraw  = {#call widget_queue_draw#}.toWidget

-- @method widgetHasIntersection@ Check if the widget intersects with a given
-- area.
--
widgetHasIntersection :: WidgetClass w => w -> Rectangle -> IO Bool
widgetHasIntersection w r = 
  liftM toBool $
  withObject r $ \r' ->
  {#call unsafe widget_intersect#} (toWidget w) (castPtr r') (castPtr nullPtr)

-- Manipulate widget state.

-- @method widgetActivate@ Activate the widget (e.g. clicking a button).
--
widgetActivate :: WidgetClass w => w -> IO Bool
widgetActivate w = liftM toBool $ {#call widget_activate#} (toWidget w)

-- @method widgetSetSensitivity@ Set the widgets sensitivity (Grayed or
-- Usable).
--
widgetSetSensitivity :: WidgetClass w => w -> Bool -> IO ()
widgetSetSensitivity w b = 
  {#call widget_set_sensitive#} (toWidget w) (fromBool b)

-- @method widgetSetSizeRequest@ Sets the minimum size of a widget.
--
widgetSetSizeRequest :: WidgetClass w => w -> Int -> Int -> IO ()
widgetSetSizeRequest w width height =
  {#call widget_set_size_request#} (toWidget w) (fromIntegral width) (fromIntegral height)

-- @method widgetIsFocus@ Set and query the input focus of a widget.
--
widgetIsFocus :: WidgetClass w => w -> IO Bool
widgetIsFocus w = liftM toBool $ 
  {#call unsafe widget_is_focus#} (toWidget w)

widgetGrabFocus :: WidgetClass w => w -> IO ()
widgetGrabFocus = {#call widget_grab_focus#}.toWidget

-- @method widgetSetAppPaintable@ Sets some weired flag in the widget.
--
widgetSetAppPaintable :: WidgetClass w => w -> Bool -> IO ()
widgetSetAppPaintable w p = 
  {#call widget_set_app_paintable#} (toWidget w) (fromBool p)

-- @method widgetSetName@ Set the name of a widget.
--
widgetSetName :: WidgetClass w => w -> String -> IO ()
widgetSetName w name = 
  withCString name ({#call widget_set_name#} (toWidget w))

-- @method widgetGetName@ Get the name of a widget.
--
widgetGetName :: WidgetClass w => w -> IO String
widgetGetName w = {#call unsafe widget_get_name#} (toWidget w) >>= 
		  peekCString

-- @method widgetAddEvents@ Enable event signals.
--
widgetAddEvents :: WidgetClass w => w -> [EventMask] -> IO ()
widgetAddEvents w em = 
  {#call widget_add_events#} (toWidget w) (fromIntegral $ fromFlags em)

-- @method widgetGetEvents@ Get enabled event signals.
--
widgetGetEvents :: WidgetClass w => w -> IO [EventMask]
widgetGetEvents w = liftM (toFlags.fromIntegral) $ 
  {#call unsafe widget_get_events#} (toWidget w)

-- @method widgetSetExtensionEvents@ Set extension events.
--
widgetSetExtensionEvents :: WidgetClass w => w -> [ExtensionMode] -> IO ()
widgetSetExtensionEvents w em = 
  {#call widget_set_extension_events#} (toWidget w) 
    (fromIntegral $ fromFlags em)

-- @method widgetGetExtensionEvents@ Get extension events.
--
widgetGetExtensionEvents :: WidgetClass w => w -> IO [ExtensionMode]
widgetGetExtensionEvents w = liftM (toFlags.fromIntegral) $ 
  {#call widget_get_extension_events#} (toWidget w)

-- Widget browsing.

-- @method widgetGetToplevel@ Retrieves the topmost widget in this tree.
--
widgetGetToplevel :: WidgetClass w => w -> IO Widget
widgetGetToplevel w = makeNewObject mkWidget $
  {#call unsafe widget_get_toplevel#} (toWidget w)

-- @method widgetIsAncestor@ Return True if the second widget is (possibly
-- indirectly) held by the first.
--
widgetIsAncestor :: (WidgetClass w, WidgetClass anc) => anc -> w -> IO Bool
widgetIsAncestor anc w = liftM toBool $  
 {#call unsafe widget_is_ancestor#} (toWidget w) (toWidget anc)

-- @method widgetReparent@ Move a widget to a new parent.
--
widgetReparent :: (WidgetClass w, WidgetClass par) => w -> par -> IO ()
widgetReparent w par = 
  {#call widget_reparent#} (toWidget w) (toWidget par)

-- @method widgetSetDirection@ Setting packaging and writing direction.
--
widgetSetDirection :: WidgetClass w => w -> TextDirection -> IO ()
widgetSetDirection w td = 
  {#call widget_set_direction#} (toWidget w) ((fromIntegral.fromEnum) td)

-- @method widgetGetDirection@ Retrieve the default direction of text writing.
--
widgetGetDirection :: WidgetClass w => w -> IO TextDirection
widgetGetDirection w = liftM (toEnum.fromIntegral) $ 
  {#call widget_get_direction#} (toWidget w)

-- Accelerator handling.

-- Lock accelerators.
-- *  
--widgetLockAccelerators :: WidgetClass w => w -> IO ()
--widgetLockAccelerators = {#call unsafe widget_lock_accelerators#}.toWidget


-- Unlock accelerators.
-- * 
--widgetUnlockAccelerators :: WidgetClass w => w -> IO ()
--widgetUnlockAccelerators = {#call widget_unlock_accelerators#}.toWidget




-- signals

-- Because there are so many similar signals (those that take an Event and
-- return a Bool) we will abstract out the skeleton. As some of these events
-- are emitted at a high rate often a bit has to be set to enable emission.
event :: WidgetClass w => SignalName -> [EventMask] ->
  ConnectAfter -> w -> (Event -> IO Bool) -> IO (ConnectId w)
event name eMask after obj fun = do
  id <- connect_BOXED__BOOL name marshalEvent after obj fun
  widgetAddEvents obj eMask
  return id

-- @signal connectToButtonPress@ A Button was pressed.
--
-- * This widget is part of a button which was just pressed. The event passed
--   to the user function is a @ref data Button@ event.
--
onButtonPress, afterButtonPress :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onButtonPress = event "button_press_event" [ButtonPressMask] False
afterButtonPress = event "button_press_event" [ButtonPressMask] True

-- @signal connectToButtonRelease@ A Butten was released.
--
onButtonRelease, afterButtonRelease :: WidgetClass w => w ->
                                       (Event -> IO Bool) -> IO (ConnectId w)
onButtonRelease = event "button_release_event" [ButtonReleaseMask] False
afterButtonRelease = event "button_release_event" [ButtonReleaseMask] True

-- @signal connectToClient@ 
--
onClient, afterClient :: WidgetClass w => w -> (Event -> IO Bool) ->
                         IO (ConnectId w)
onClient = event "client_event" [] False
afterClient = event "client_event" [] True

-- @signal connectToConfigure@ The widget's status has changed.
--
onConfigure, afterConfigure :: WidgetClass w => w -> (Event -> IO Bool) ->
                               IO (ConnectId w)
onConfigure = event "configure_event" []  False
afterConfigure = event "configure_event" []  True

-- @signal connectToDelete@ This signal is emitted when the close icon on the
-- surrounding window is pressed. The default action is to emit the
-- @ref signal destroy@ signal.
--
onDelete, afterDelete :: WidgetClass w => w -> (Event -> IO Bool) ->
                         IO (ConnectId w)
onDelete = event "delete_event" [] False
afterDelete = event "delete_event" [] True

-- @signal connectToDestroyEvent@ The widget will be destroyed.
--
-- * The widget received a destroy event from the window manager.
--
onDestroyEvent, afterDestroyEvent :: WidgetClass w => 
				     w -> (Event -> IO Bool) ->
				     IO (ConnectId w)
onDestroyEvent = event "destroy_event" [] False
afterDestroyEvent = event "destroy_event" [] True

-- @signal connectToDirectionChanged@ The default text direction was changed.
--
onDirectionChanged, afterDirectionChanged :: WidgetClass w => w ->
                                             (Event -> IO Bool) ->
                                             IO (ConnectId w)
onDirectionChanged = event "direction_changed" [] False
afterDirectionChanged = event "direction_changed" [] True

-- @signal connectToEnterNotify@ Mouse cursor entered widget.
--
onEnterNotify, afterEnterNotify :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onEnterNotify = event "enter_notify_event" [EnterNotifyMask] False
afterEnterNotify = event "enter_notify_event" [EnterNotifyMask] True

-- @signal connectToLeaveNotify@ Mouse cursor leaves widget.
--
onLeaveNotify, afterLeaveNotify :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onLeaveNotify = event "leave_notify_event" [LeaveNotifyMask] False
afterLeaveNotify = event "leave_notify_event" [LeaveNotifyMask] True

-- @signal connectToExpose@ Instructs the widget to redraw.
--
onExpose, afterExpose :: WidgetClass w => w -> (Event -> IO Bool) ->
                         IO (ConnectId w)
onExpose = event "expose_event" [] False
afterExpose = event "expose_event" [] True

-- @signal connectToFocusIn@ Widget gains input focus.
--
onFocusIn, afterFocusIn :: WidgetClass w => w -> (Event -> IO Bool) ->
                           IO (ConnectId w)
onFocusIn = event "focus_in_event" [FocusChangeMask] False
afterFocusIn = event "focus_in_event" [FocusChangeMask] True

-- @signal connectToFocusOut@ Widget looses input focus.
--
onFocusOut, afterFocusOut :: WidgetClass w => w -> (Event -> IO Bool) ->
                             IO (ConnectId w)
onFocusOut = event "focus_out_event" [FocusChangeMask] False
afterFocusOut = event "focus_out_event" [FocusChangeMask] True

-- @signal connectToGrabFocus@ The widget is about to receive all events.
--
-- * It is possible to redirect all input events to one widget to force the
--   user to use only this widget. Such a situation is initiated by
--   @ref method addGrab@.
--
onGrabFocus, afterGrabFocus :: WidgetClass w => w -> IO () ->
                               IO (ConnectId w)
onGrabFocus = connect_NONE__NONE  "grab_focus" False
afterGrabFocus = connect_NONE__NONE "grab_focus" True

-- @signal connectToDestroy@ The widget will be destroyed.
--
-- * This is the last signal this widget will receive.
--
onDestroy, afterDestroy :: WidgetClass w => w -> (IO ()) ->
                           IO (ConnectId w)
onDestroy = connect_NONE__NONE "destroy" False
afterDestroy = connect_NONE__NONE "destroy" True

-- @signal connectToHide@ The widget was asked to hide itself.
--
-- * This signal is emitted each time @ref method widgetHide@ is called. Use
--   @ref method connectToUnmap@ when your application needs to be informed
--   when the widget is actually removed from screen.
--
onHide, afterHide :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onHide = connect_NONE__NONE "hide" False
afterHide = connect_NONE__NONE "hide" True

-- @signal connectToHierarchyChanged@ The toplevel window changed.
--
-- * When a subtree of widgets is removed or added from a tree with a toplevel
--   window this signal is emitted. It is emitted on each widget in the
--   detached or attached subtree.
--
onHierarchyChanged, afterHierarchyChanged :: WidgetClass w => w -> IO () ->
                                             IO (ConnectId w)
onHierarchyChanged = connect_NONE__NONE "hierarchy_changed" False
afterHierarchyChanged = connect_NONE__NONE "hierarchy_changed" True

-- @signal connectToKeyPress@ A key was pressed.
--
onKeyPress, afterKeyPress :: WidgetClass w => w -> (Event -> IO Bool) ->
                             IO (ConnectId w)
onKeyPress = event "key_press_event" [KeyPressMask] False
afterKeyPress = event "key_press_event" [KeyPressMask] True

-- @signal connectToKeyRelease@ A key was released.
--
onKeyRelease, afterKeyRelease :: WidgetClass w => w -> (Event -> IO Bool) ->
                                 IO (ConnectId w)
onKeyRelease = event "key_release_event" [KeyReleaseMask] False
afterKeyRelease = event "key_release_event" [KeyReleaseMask] True

-- @signal connectToMnemonicActivate@ 
--
onMnemonicActivate, afterMnemonicActivate :: WidgetClass w => w ->
                                             (Bool -> IO Bool) ->
                                             IO (ConnectId w)
onMnemonicActivate = connect_BOOL__BOOL "mnemonic_activate" False
afterMnemonicActivate = connect_BOOL__BOOL "mnemonic_activate" True

-- @signal connectToMotionNotify@ Track mouse movements.
--
-- * If @ref arg hint@ is False, a callback for every movement of the mouse is
--   generated. To avoid a backlog of mouse messages, it is usually sufficient
--   to sent @ref arg hint@ to True, generating only one event. The
--   application now has to state that it is ready for the next message by
--   calling @ref method drawWindowGetPointer@.
--
onMotionNotify, afterMotionNotify :: WidgetClass w => w -> Bool ->
                                     (Event -> IO Bool) -> 
                                     IO (ConnectId w)
onMotionNotify w hint = event "motion_notify_event" 
  (if hint then [PointerMotionHintMask] else [PointerMotionMask]) False w
afterMotionNotify w hint = event "motion_notify_event" 
  (if hint then [PointerMotionHintMask] else [PointerMotionMask]) True w

-- @signal connectToParentSet@ 
--
onParentSet, afterParentSet :: (WidgetClass w, WidgetClass old) => w ->
                               (old -> IO ()) -> IO (ConnectId w)
onParentSet = connect_OBJECT__NONE "parent_set"  False
afterParentSet = connect_OBJECT__NONE "parent_set"  True

-- @signal connectToPopupMenu@ 
--
onPopupMenu, afterPopupMenu :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onPopupMenu = connect_NONE__NONE "popup_menu" False
afterPopupMenu = connect_NONE__NONE "popup_menu" True

-- @signal connectToProximityIn@ The input device became active.
--
-- * This event indicates that a pen of a graphics tablet or similar device is
--   now touching the tablet.
--
onProximityIn, afterProximityIn :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onProximityIn = event "proximity_in_event" [ProximityInMask] False
afterProximityIn = event "proximity_in_event" [ProximityInMask] True

-- @signal connectToProximityOut@ The input device became inactive.
--
-- * The pen was removed from the graphics tablet's surface.
--
onProximityOut, afterProximityOut :: WidgetClass w => w ->
                                     (Event -> IO Bool) -> IO (ConnectId w)
onProximityOut = event "proximity_out_event" [ProximityOutMask] False
afterProximityOut = event "proximity_out_event" [ProximityOutMask] True

-- @signal connectToScroll@ The mouse wheel has turned.
--
-- * The @ref data Event@ is always @ref variant Scroll@.
--
onScroll, afterScroll :: WidgetClass w => w -> (Event -> IO Bool) ->
                         IO (ConnectId w)
onScroll = event "scroll_event" [ScrollMask] False
afterScroll = event "scroll_event" [ScrollMask] True

-- @signal connectToShow@ The widget was asked to show itself.
--
-- * This signal is emitted each time @ref method widgetShow@ is called. Use
--   @ref method connectToMap@ when your application needs to be informed when
--   the widget is actually shown.
--
onShow, afterShow :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onShow = connect_NONE__NONE "show" False
afterShow = connect_NONE__NONE "show" True

-- @signal connectToSizeAllocate@ Inform widget about the size it has.
--
-- * After querying a widget for the size it wants to have (through emitting
--   the @ref signal sizeRequest@ signal) a container will emit this signal to
--   inform the widget about the real size it should occupy.
--
onSizeAllocate, afterSizeAllocate :: WidgetClass w => w ->
                                     (Allocation -> IO ()) -> IO (ConnectId w)
onSizeAllocate = connect_BOXED__NONE "size_allocate" peek False
afterSizeAllocate = connect_BOXED__NONE "size_allocate" peek True

-- @signal connectToSizeRequest@ Query the widget for the size it likes to
-- have.
--
-- * A parent container emits this signal to its child to query the needed
--   height and width of the child. There is not guarantee that the widget
--   will actually get this area.
--
onSizeRequest, afterSizeRequest :: WidgetClass w => w -> (IO Requisition) ->
                                   IO (ConnectId w)
onSizeRequest w fun = connect_PTR__NONE "size_request" False w (\rqPtr -> do
  req <- fun
  unless (rqPtr==nullPtr) $ poke rqPtr req)
afterSizeRequest w fun = connect_PTR__NONE "size_request" True w (\rqPtr -> do
  req <- fun
  unless (rqPtr==nullPtr) $ poke rqPtr req) 

-- @signal connectToStateChanged@ 
--
onStateChanged, afterStateChanged :: WidgetClass w => w ->
                                     (StateType -> IO ()) -> IO (ConnectId w)
onStateChanged = connect_ENUM__NONE "state_changed" False
afterStateChanged = connect_ENUM__NONE "state_changed" True

-- @signal connectToUnmap@ The widget was removed from screen.
--
onUnmap, afterUnmap :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onUnmap = connect_NONE__NONE "unmap" False
afterUnmap = connect_NONE__NONE "unmap" True

-- @signal connectToUnrealize@ This widget's drawing area is about to be
-- destroyed.
--
onUnrealize, afterUnrealize :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onUnrealize = connect_NONE__NONE "unrealize" False
afterUnrealize = connect_NONE__NONE "unrealize" True

-- @signal connectToVisibilityNotify@ 
--
onVisibilityNotify, afterVisibilityNotify :: WidgetClass w => w ->
                                             (Event -> IO Bool) ->
                                             IO (ConnectId w)
onVisibilityNotify = 
  event "visibility_notify_event" [VisibilityNotifyMask] False
afterVisibilityNotify = 
  event "visibility_notify_event" [VisibilityNotifyMask] True

-- @signal connectToWindowState@ 
--
onWindowState, afterWindowState :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onWindowState = event "window_state_event" [] False
afterWindowState = event "window_state_event" [] True

