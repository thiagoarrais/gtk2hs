--  C -> Haskell Compiler: management of switches
--
--  Author : Manuel M T Chakravarty
--  Created: 6 March 99
--
--  Version $Revision: 1.2 $ from $Date: 2005/05/31 18:17:37 $
--
--  Copyright (c) [1999..2004] Manuel M T Chakravarty
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
--- DESCRIPTION ---------------------------------------------------------------
--
--  This module manages C2HS's compiler switches. It exports the data types
--  used to store the switches and operations on them.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  Overview over the switches:
--
--  * The cpp options specify the options passed to the C preprocessor.
--
--  * The cpp filename gives the name of the executable of the C preprocessor.
--
--  * The `hpaths' switch lists all directories that should be considered when
--    searching for a header file.
--
--  * The `keep' flag says whether the intermediate file produced by the C
--    pre-processor should be retained or not.
--
--  * Traces specify which trace information should be output by the compiler.
--    Currently the following trace information is supported:
--
--    - information about phase activation and phase completion
--
--  * After processing the compiler options, `outputSB' contains the base name
--    for the generated Haskell, C header, and .chi files.  However, during
--    processing compiler options, `outputSB' contains arguments to the
--    `--output' option and `outDirSB' contains arguments to the
--    `--output-dir' option.
--
--  * The pre-compiled header switch is unset if no pre-compiled header should
--    be read or generated. If the option is set and a header file is given
--    a concise version of the header will be written to the FilePath. If
--    a binding file is given, the pre-compiled header is used to expand the
--    module unless the binding file contains itself C declarations.
--
--- TODO ----------------------------------------------------------------------
--

module Switches (
  SwitchBoard(..), Traces(..), initialSwitchBoard
) where


-- the switch board contains all toolkit switches
-- ----------------------------------------------

-- all switches of the toolkit (EXPORTED)
--
data SwitchBoard = SwitchBoard {
		     cppOptsSB :: String,	-- cpp options
		     cppSB     :: FilePath,	-- cpp executable
		     hpathsSB  :: [FilePath],	-- header file directories
		       -- since 0.11.1 `hpathsSB' isn't really needed anymore..
		       -- ..remove from 0.12 series
		     keepSB    :: Bool,		-- keep intermediate file
		     tracesSB  :: Traces,	-- trace flags
		     outputSB  :: FilePath,	-- basename of generated files
		     outDirSB  :: FilePath,	-- dir where generated files go
		     headerSB  :: FilePath,	-- generated header file
		     preCompSB :: Maybe FilePath,-- optional binary header r/w
                     oldParsSB :: Bool,         -- use the old slow lexer/parser
		     oldFFI    :: Bool,		-- GHC 4.XX compatible code
		     chiPathSB :: [FilePath]	-- .chi file directories
		   }

-- switch states on startup (EXPORTED)
--
initialSwitchBoard :: SwitchBoard
initialSwitchBoard  = SwitchBoard {
			cppOptsSB = "",
			cppSB     = "cpp",
			hpathsSB  = [],
			keepSB	  = False,
		        tracesSB  = initialTraces,
			outputSB  = "",
			outDirSB  = "",
			headerSB  = "",
			preCompSB = Nothing,
                        oldParsSB = False,
			oldFFI	  = False,
			chiPathSB = ["."]
		      }


-- traces
-- ------

-- different kinds of traces possible (EXPORTED)
--
data Traces = Traces {
	        tracePhasesSW  :: Bool,
	        traceGenBindSW :: Bool,
	        traceCTravSW   :: Bool,
		dumpCHSSW      :: Bool,
                dumpCASTSW     :: Bool
	      }

-- trace setting on startup
--
-- * all traces are initially off
--
initialTraces :: Traces
initialTraces  = Traces {
		   tracePhasesSW  = False,
		   traceGenBindSW = False,
		   traceCTravSW   = False,
		   dumpCHSSW	  = False,
                   dumpCASTSW     = False
		 }
