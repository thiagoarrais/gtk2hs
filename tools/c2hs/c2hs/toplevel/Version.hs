module Version (version, copyright, disclaimer)
where

-- version number is major.minor.patchlvl; don't change the format of the
-- `versnum' line as it is `grep'ed for by a Makefile
--
idstr      = "$Id: Version.hs,v 1.1 2004/11/13 16:42:41 duncan_coutts Exp $"
name       = "C->Haskell Compiler"
versnum    = "0.13.4"
versnick   = "\"Pressing Forward\""
date	   = "17 Oct 2004"
version    = name ++ ", version " ++ versnum ++ " " ++ versnick ++ ", " ++ date
copyright  = "Copyright (c) [1999..2004] Manuel M T Chakravarty"
disclaimer = "This software is distributed under the \
	     \terms of the GNU Public Licence.\n\
	     \NO WARRANTY WHATSOEVER IS PROVIDED. \
	     \See the details in the documentation."
