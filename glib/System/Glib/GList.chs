-- -*-haskell-*-
--  GIMP Toolkit (GTK)
--
--  Author : Axel Simon
--
--  Created: 19 March 2002
--
--  Version $Revision: 1.4 $ from $Date: 2005/04/19 02:15:32 $
--
--  Copyright (C) 2002 Axel Simon
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
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Defines functions to extract data from a GList and to produce a GList from
-- a list of pointers.
--
-- * The same for GSList.
--
module System.Glib.GList (
  GList,
  readGList,
  fromGList,
  toGList,
  GSList,
  readGSList,
  fromGSList,
  fromGSListRev,
  toGSList
  ) where

import Monad	(liftM)
import Foreign

{# context lib="glib" prefix="g" #}

{#pointer * GList#}
{#pointer * GSList#}

-- methods

-- Turn a GList into a list of pointers but don't destroy the list.
--
readGList :: GList -> IO [Ptr a]
readGList glist
  | glist==nullPtr = return []
  | otherwise	    = do
    x <- {#get GList->data#} glist
    glist' <- {#get GList->next#} glist
    xs <- readGList glist'
    return (castPtr x:xs)

-- Turn a GList into a list of pointers (freeing the GList in the process).
--
fromGList :: GList -> IO [Ptr a]
fromGList glist = do
    glist' <- {#call unsafe list_reverse#} glist
    extractList glist' []
  where
    extractList gl xs
      | gl==nullPtr = return xs
      | otherwise   = do
	x <- {#get GList.data#} gl
	gl' <- {#call unsafe list_delete_link#} gl gl
	extractList gl' (castPtr x:xs)

-- Turn a GSList into a list of pointers but don't destroy the list.
--
readGSList :: GSList -> IO [Ptr a]
readGSList gslist
  | gslist==nullPtr = return []
  | otherwise	    = do
    x <- {#get GSList->data#} gslist
    gslist' <- {#get GSList->next#} gslist
    xs <- readGSList gslist'
    return (castPtr x:xs)

-- Turn a GSList into a list of pointers (freeing the GSList in the process).
--
fromGSList :: GSList -> IO [Ptr a]
fromGSList gslist
  | gslist==nullPtr = return []
  | otherwise	    = do
    x <- {#get GSList->data#} gslist
    gslist' <- {#call unsafe slist_delete_link#} gslist gslist
    xs <- fromGSList gslist'
    return (castPtr x:xs)

-- Turn a GSList into a list of pointers and reverse it.
--
fromGSListRev :: GSList -> IO [Ptr a]
fromGSListRev gslist =
  extractList gslist []
  where
    extractList gslist xs
      | gslist==nullPtr = return xs
      | otherwise	= do
	x <- {#get GSList->data#} gslist
	gslist' <- {#call unsafe slist_delete_link#} gslist gslist
	extractList gslist' (castPtr x:xs)

-- Turn a list of something into a GList.
--
toGList :: [Ptr a] -> IO GList
toGList xs = makeList nullPtr xs
  where
    -- makeList :: GList -> [Ptr a] -> IO GList
    makeList current (x:xs) = do
      newHead <- {#call unsafe list_prepend#} current (castPtr x)
      makeList newHead xs
    makeList current [] = return current

-- Turn a list of something into a GSList.
--
toGSList :: [Ptr a] -> IO GSList
toGSList xs = makeList nullPtr xs
  where
    -- makeList :: GSList -> [Ptr a] -> IO GSList
    makeList current (x:xs) = do
      newHead <- {#call unsafe slist_prepend#} current (castPtr x)
      makeList newHead xs
    makeList current [] = return current

