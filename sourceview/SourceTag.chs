-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceTag
--
--  Author : Duncan Coutts
--  derived from GtkTextView bindings by Axel Simon
--          
--  Created: 22 October 2003
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
module SourceTag (
  SourceTag,
  syntaxTagNew,
  patternTagNew,
  keywordListTagNew,
  blockCommentTagNew,
  lineCommentTagNew,
  stringTagNew,
  sourceTagGetStyle,
  sourceTagSetStyle
  ) where

import Monad	(liftM)
import FFI
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import SourceViewType#}
import SourceTagStyle
import GList   (toGSList, fromGSList)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new 'SourceTag'
--
syntaxTagNew :: String -> String -> String -> String -> IO SourceTag
syntaxTagNew id name patternStart patternEnd =
  makeNewGObject mkSourceTag $ liftM castPtr $
  withCString id           $ \strPtr1 -> 
  withCString name         $ \strPtr2 -> 
  withCString patternStart $ \strPtr3 -> 
  withCString patternEnd   $ \strPtr4 -> 
  {#call syntax_tag_new#} strPtr1 strPtr2 strPtr3 strPtr4

-- | Create a new 'SourceTag'
--
patternTagNew :: String -> String -> String -> IO SourceTag
patternTagNew id name pattern =
  makeNewGObject mkSourceTag $ liftM castPtr $
  withCString id      $ \strPtr1 -> 
  withCString name    $ \strPtr2 -> 
  withCString pattern $ \strPtr3 -> 
  {#call unsafe pattern_tag_new#} strPtr1 strPtr2 strPtr3


-- | Create a new 'SourceTag'.
--
keywordListTagNew :: String -> String -> [String] -> Bool -> Bool -> Bool ->
		     String -> String -> IO SourceTag
keywordListTagNew id name keywords
                  caseSensitive
                  matchEmptyStringAtBeginning
                  matchEmptyStringAtEnd
                  beginningRegex
                  endRegex = do
  keywordPtrs <- mapM newUTFString keywords
  keywordList <- toGSList keywordPtrs
  obj <- makeNewGObject mkSourceTag $ liftM castPtr $
	 withCString  id      $ \strPtr1 -> 
	 withCString  name    $ \strPtr2 -> 
	 withCString  beginningRegex $ \strPtr3 -> 
	 withCString  endRegex $ \strPtr4 -> {#call unsafe keyword_list_tag_new#}
	   strPtr1 strPtr2 keywordList (fromBool caseSensitive)
	   (fromBool matchEmptyStringAtBeginning) (fromBool matchEmptyStringAtEnd)
	   strPtr3 strPtr4
  -- destory the list
  fromGSList keywordList
  -- destory the elements
  mapM_ free keywordPtrs
  return obj

-- | Create a new 'SourceTag'
--
blockCommentTagNew :: String -> String -> String -> String -> IO SourceTag
blockCommentTagNew = syntaxTagNew --in the C header this is just a macro

-- | Create a new 'SourceTag'
--
lineCommentTagNew :: String -> String -> String -> IO SourceTag
lineCommentTagNew id name pattern =
  makeNewGObject mkSourceTag $ liftM castPtr $
  withCString id      $ \strPtr1 ->
  withCString name    $ \strPtr2 ->
  withCString pattern $ \strPtr3 ->
  {#call unsafe line_comment_tag_new#} strPtr1 strPtr2 strPtr3

-- | Create a new 'SourceTag'
--
stringTagNew :: String -> String -> String -> String -> Bool -> IO SourceTag
stringTagNew id name patternStart patternEnd endAtLineEnd =
  makeNewGObject mkSourceTag $ liftM castPtr $
  withCString id           $ \strPtr1 -> 
  withCString name         $ \strPtr2 -> 
  withCString patternStart $ \strPtr3 -> 
  withCString patternEnd   $ \strPtr4 -> 
  {#call unsafe string_tag_new#} strPtr1 strPtr2 strPtr3 strPtr4 (fromBool endAtLineEnd)


-- | 
-- 
sourceTagGetStyle :: SourceTag -> IO SourceTagStyle
sourceTagGetStyle tag = do
  tsPtr <- {#call unsafe source_tag_get_style#} tag
  ts <- peek (castPtr tsPtr)
  {#call unsafe g_free#} tsPtr
  return ts

-- | 
-- 
sourceTagSetStyle :: SourceTag -> SourceTagStyle -> IO ()
sourceTagSetStyle tag ts = alloca $ \tsPtr -> do
  poke tsPtr ts
  {#call unsafe source_tag_set_style#} tag (castPtr tsPtr)

