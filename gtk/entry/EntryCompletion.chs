{-# OPTIONS -cpp #-}
-- |GIMP Toolkit (GTK) Binding for Haskell: binding to GConf   -*-haskell-*-
--    for storing and retrieving configuartion information
--
--  Author : Duncan Coutts
--  Created: 24 April 2004
--
--  Copyright (c) 2004 Duncan Coutts
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
--- Description ---------------------------------------------------------------
--
--  Language: Haskell 98 Binding Module
--
-- Completion functionality for the Entry widget.
--
-- Added in GTK+ 2.4
--

module EntryCompletion (
  EntryCompletion,
  EntryCompletionClass,
  entryCompletionNew,
  entryCompletionGetEntry,
  entryCompletionSetModel,
  entryCompletionGetModel,
  entryCompletionSetMatchFunc,
  entryCompletionSetMinimumKeyLength,
  entryCompletionGetMinimumKeyLength,
  entryCompletionComplete,
  entryCompletionInsertActionText,
  entryCompletionInsertActionMarkup,
  entryCompletionDeleteAction,
  entryCompletionSetTextColumn
) where

import Monad	(liftM)
import FFI
import LocalData (newIORef, readIORef, writeIORef)

import GObject (makeNewGObject)
import Object  (makeNewObject)
{#import Hierarchy#}
{#import Signal#}
{#import TreeModel#} (TreeIter, createTreeIter)

{# context lib="gtk" prefix="gtk" #}

entryCompletionNew :: IO EntryCompletion
entryCompletionNew =
  makeNewGObject mkEntryCompletion $ liftM castPtr $
  {# call gtk_entry_completion_new #}

entryCompletionGetEntry :: EntryCompletion -> IO (Maybe Entry)
entryCompletionGetEntry ec = do
  entryPtr <- {# call gtk_entry_completion_get_entry #} ec
  if entryPtr == nullPtr
    then return Nothing
    else liftM Just $ makeNewObject mkEntry $ return (castPtr entryPtr)

entryCompletionSetModel :: EntryCompletion ->  TreeModel -> IO ()
entryCompletionSetModel ec tm =
  {# call gtk_entry_completion_set_model #} ec tm

entryCompletionGetModel :: EntryCompletion -> IO TreeModel
entryCompletionGetModel ec =
  makeNewGObject mkTreeModel $
  {# call gtk_entry_completion_get_model #} ec

entryCompletionSetMatchFunc :: EntryCompletion -> (String -> TreeIter -> IO ()) -> IO ()
entryCompletionSetMatchFunc ec handler =
  connect_GtkEntryCompletionMatchFunc ec handler

entryCompletionSetMinimumKeyLength :: EntryCompletion -> Int -> IO ()
entryCompletionSetMinimumKeyLength ec minLength =
  {# call gtk_entry_completion_set_minimum_key_length #} ec
    (fromIntegral minLength)

entryCompletionGetMinimumKeyLength :: EntryCompletion -> IO Int
entryCompletionGetMinimumKeyLength ec =
  liftM fromIntegral $
  {# call gtk_entry_completion_get_minimum_key_length #} ec

entryCompletionComplete :: EntryCompletion -> IO ()
entryCompletionComplete ec =
  {# call gtk_entry_completion_complete #} ec

entryCompletionInsertActionText :: EntryCompletion -> Int -> String -> IO ()
entryCompletionInsertActionText ec index text =
  withUTFString text $ \strPtr ->
  {# call gtk_entry_completion_insert_action_text #} ec
    (fromIntegral index) strPtr

entryCompletionInsertActionMarkup :: EntryCompletion -> Int -> String -> IO ()
entryCompletionInsertActionMarkup ec index markup =
  withUTFString markup $ \strPtr ->
  {# call gtk_entry_completion_insert_action_markup #} ec
    (fromIntegral index) strPtr 

entryCompletionDeleteAction :: EntryCompletion -> Int -> IO ()
entryCompletionDeleteAction ec index =
  {# call gtk_entry_completion_delete_action #} ec (fromIntegral index)

entryCompletionSetTextColumn :: EntryCompletion -> Int -> IO ()
entryCompletionSetTextColumn ec column =
  {# call gtk_entry_completion_set_text_column #} ec (fromIntegral column)


-------------------------------------------------
-- Callback stuff for entryCompletionSetMatchFunc
--
{#pointer GDestroyNotify#}

#if __GLASGOW_HASKELL__>=504
foreign import ccall "wrapper" mkDestructor :: IO () -> IO GDestroyNotify
#else
foreign export dynamic mkDestructor :: IO () -> IO GDestroyNotify
#endif

type GtkEntryCompletionMatchFunc =
  Ptr EntryCompletion -> --GtkEntryCompletion *completion
  Ptr CChar ->           --const gchar *key
  Ptr TreeIter ->        --GtkTreeIter *iter
  Ptr () ->              --gpointer user_data
  IO ()

#if __GLASGOW_HASKELL__>=504
foreign import ccall "wrapper" mkHandler_GtkEntryCompletionMatchFunc ::
  GtkEntryCompletionMatchFunc -> 
  IO (FunPtr GtkEntryCompletionMatchFunc)
#else
foreign export dynamic mkHandler_GtkEntryCompletionMatchFunc ::
  GtkEntryCompletionMatchFunc -> 
  IO (FunPtr GtkEntryCompletionMatchFunc)
#endif

connect_GtkEntryCompletionMatchFunc :: EntryCompletion ->
                                       (String -> TreeIter -> IO ()) ->
                                       IO ()
connect_GtkEntryCompletionMatchFunc ec user = do
  hPtr <- mkHandler_GtkEntryCompletionMatchFunc
    (\_ keyPtr iterPtr _ -> do key <- peekUTFString keyPtr
                               iter <- createTreeIter iterPtr
                               user key iter)
  dRef <- newIORef nullFunPtr
  dPtr <- mkDestructor $ do
    freeHaskellFunPtr hPtr
    dPtr <- readIORef dRef
    freeHaskellFunPtr dPtr
  writeIORef dRef dPtr
  {# call gtk_entry_completion_set_match_func #} ec
    (castFunPtr hPtr) nullPtr dPtr
