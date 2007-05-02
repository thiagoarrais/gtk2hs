--  C -> Haskell Compiler: main module
--
--  Author : Manuel M T Chakravarty
--  Derived: 12 August 99
--
--  Version $Revision: 1.6 $ from $Date: 2005/07/03 14:58:16 $
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
--  This is the main module of the compiler.  It sets the version, processes
--  the command line arguments, and controls the compilation process.
--
--  Originally, derived from `Main.hs' of the Nepal Compiler.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  Usage:
--  ------
--
--    c2hs [ option... ] [header-file] binding-file
--
--  The compiler is supposed to emit a Haskell program that expands all hooks
--  in the given binding file.
--
--  File name suffix:
--  -----------------
--
--  Note: These also depend on suffixes defined in the compiler proper.
--
--  .h   C header file
--  .i   pre-processeed C header file
--  .hs	 Haskell file
--  .chs Haskell file with C->Haskell hooks (binding file)
--  .chi C->Haskell interface file
--
--  Options:
--  --------
--
--  -C CPPOPTS
--  --cppopts=CPPOPTS
--        Pass the additional options CPPOPTS to the C preprocessor.
--
--        Repeated occurences accumulate.
--
--  -c CPP
--  --cpp=CPP
--        Use the executable CPP to invoke CPP.
--
--        In the case of repeated occurences, the last takes effect.
--
--  -d TYPE
--  --dump=TYPE
--        Dump intermediate representation:
--
--	  + if TYPE is `trace', trace the compiler phases (to stderr)
--	  + if TYPE is `genbind', trace binding generation (to stderr)
--	  + if TYPE is `ctrav', trace C declaration traversal (to stderr)
--	  + if TYPE is `chs', dump the binding file (insert `.dump' into the
--	    file name to avoid overwriting the original file)
--
--  -h, -?
--  --help
--        Dump brief usage information to stderr.
--
--  -i DIRS
--  --include=DIRS
--        Search the colon separated list of directories DIRS when searching
--	  for .chi files.
--
--  -k
--  --keep
--        Keep the intermediate file that contains the pre-processed C header
--        (it carries the suffix `.i').
--
--  -o FILE
--  --output=FILE
--        Place output in file FILE.
--
--        If `-o' is not specified, the default is to put the output for
--	  `source.chs' in `source.hs' in the same directory that contains the
--	  binding file.  If specified, the emitted C header file is put into
--	  the same directory as the output file.  The same holds for
--	  C->Haskell interface file.  All generated files also share the
--	  basename. 
--
--  -t PATH
--  --output-dir=PATH
--        Place generated files in the directory PATH.
--
--        If this option as well as the `-o' option is given, the basename of
--        the file specified with `-o' is put in the directory specified with
--        `-t'. 
--
--  -v,
--  --version
--        Print (on standard error output) the version and copyright
--	  information of the compiler (before doing anything else).
--
--  -p FILE
--  --precomp=FILE
--        Use or generate a precompiled header. If a header file is
--        given write a condensed version of the header file into
--        FILE. If a binding file is given that does not contain any C
--        declarations itself, use the condensed information in FILE
--        to generate the binding. Using a precompiled header file will
--        significantly speed up the translation of a binding module.
--
--  --old-ffi [=yes|=no]
--	  Generate hooks using pre-standard FFI libraries.  This currently
--	  affects only call hooks where instead of `Addr' types 
--	  `Ptr <someOtherType>' is used.
--
--- TODO ----------------------------------------------------------------------
--

module Main (main)
where

-- standard libraries
import List	  (isPrefixOf)
import IO	  ()
import Monad      (when, unless, mapM)
import Maybe      (fromJust)

-- base libraries
import System.Console.GetOpt     
		  (ArgOrder(..), OptDescr(..), ArgDescr(..), usageInfo, getOpt)
import FNameOps   (suffix, basename, dirname, stripSuffix, addPath)
import Errors	  (interr)
import UNames     (saveRootNameSupply, restoreRootNameSupply)
import Binary	  (Binary(..), putBinFileWithDict, getBinFileWithDict)

-- c2hs modules
import C2HSState  (CST, nop, runC2HS, fatal, fatalsHandledBy, getId,
		   ExitCode(..), stderr, IOMode(..), putStrCIO, hPutStrCIO,
		   hPutStrLnCIO, exitWithCIO, getArgsCIO, getProgNameCIO,
		   ioeGetErrorString, ioeGetFileName, removeFileCIO, liftIO,
		   systemCIO, fileFindInCIO, mktempCIO, openFileCIO, hCloseCIO,
		   SwitchBoard(..), Traces(..), setTraces,
		   traceSet, setSwitch, getSwitch, putTraceStr)
import C	  (AttrC, hsuffix, isuffix, loadAttrC)
import CHS	  (CHSModule, loadCHS, dumpCHS, hssuffix, chssuffix, dumpCHI)
import GenHeader  (genHeader)
import GenBind	  (expandHooks)
import Version    (version, copyright, disclaimer)
import C2HSConfig (cpp, cppopts, hpaths, tmpdir)


-- wrapper running the compiler
-- ============================

main :: IO ()
main  = runC2HS (version, copyright, disclaimer) compile


-- option handling
-- ===============

-- header is output in case of help, before the descriptions of the options;
-- errTrailer is output after an error message
--
header :: String -> String -> String -> String
header version copyright disclaimer  = 
  version ++ "\n" ++ copyright ++ "\n" ++ disclaimer
  ++ "\n\nUsage: c2hs [ option... ] [header-file] binding-file\n"

trailer, errTrailer :: String
trailer    = "\n\
	     \The header file must be a C header file matching the given \
	     \binding file.\n\
	     \The dump TYPE can be\n\
	     \  trace   -- trace compiler phases\n\
	     \  genbind -- trace binding generation\n\
	     \  ctrav   -- trace C declaration traversal\n\
	     \  chs     -- dump the binding file (adds `.dump' to the name)\n"
errTrailer = "Try the option `--help' on its own for more information.\n"

-- supported option types
--
data Flag = CPPOpts String      -- additional options for C preprocessor
	  | CPP     String      -- program name of C preprocessor
	  | Dump    DumpType    -- dump internal information
	  | Help	        -- print brief usage information
	  | Keep	        -- keep the .i file
	  | Include String	-- list of directories to search .chi files
	  | Output  String      -- file where the generated file should go
	  | OutDir  String      -- directory where generates files should go
	  | PreComp String      -- write or read a precompiled header
	  | Version	        -- print version information on stderr
	  | Error   String      -- error occured during processing of options
	  deriving Eq

data DumpType = Trace	      -- compiler trace
	      | GenBind	      -- trace `GenBind'
	      | CTrav	      -- trace `CTrav'
	      | CHS	      -- dump binding file
	      deriving Eq

-- option description suitable for `GetOpt'
--
options :: [OptDescr Flag]
options  = [
  Option ['C'] 
	 ["cppopts"] 
	 (ReqArg CPPOpts "CPPOPTS") 
	 "pass CPPOPTS to the C preprocessor",
  Option ['c'] 
	 ["cpp"] 
	 (ReqArg CPP "CPP") 
	 "use executable CPP to invoke C preprocessor",
  Option ['d'] 
	 ["dump"] 
	 (ReqArg dumpArg "TYPE") 
	 "dump internal information (for debugging)",
  Option ['h', '?'] 
	 ["help"] 
	 (NoArg Help) 
	 "brief help (the present message)",
  Option ['i']
	 ["include"]
	 (ReqArg Include "INCLUDE")
	 "include paths for .chi files",
  Option ['k'] 
	 ["keep"] 
	 (NoArg Keep) 
	 "keep pre-processed C header",
  Option ['o'] 
	 ["output"] 
	 (ReqArg Output "FILE") 
	 "output result to FILE (should end in .hs)",
  Option ['t'] 
	 ["output-dir"] 
	 (ReqArg OutDir "PATH") 
	 "place generated files in PATH",
  Option ['p']
         ["precomp"]
         (ReqArg PreComp "FILE")
         "generate or read precompiled header file FILE",
  Option ['v'] 
	 ["version"] 
	 (NoArg Version) 
	 "show version information"]

-- convert argument of `Dump' option
--
dumpArg           :: String -> Flag
dumpArg "trace"    = Dump Trace
dumpArg "genbind"  = Dump GenBind
dumpArg "ctrav"    = Dump CTrav
dumpArg "chs"      = Dump CHS
dumpArg _          = Error "Illegal dump type."

-- main process (set up base configuration, analyse command line, and execute
-- compilation process)
--
-- * Exceptions are caught and reported
--
compile :: CST s ()
compile  = 
  do
    setup
    cmdLine <- getArgsCIO
    case getOpt RequireOrder options cmdLine of
      ([Help]   , []  , []) -> doExecute [Help]    []
      ([Version], []  , []) -> doExecute [Version] []
      (opts     , args, []) 
        | properArgs (hasPreCompFlag opts) args -> doExecute opts args
        | otherwise                             -> raiseErrs [wrongNoOfArgsErr]
      (_   , _   , errs)    -> raiseErrs errs
  where
    properArgs preComp [file]         = suffix file == chssuffix ||
					suffix file == hsuffix && preComp
    properArgs preComp [file1, file2] = suffix file1 == hsuffix 
					&& suffix file2 == chssuffix 
    properArgs _       _              = False
    --
    hasPreCompFlag (PreComp _:fs) = True
    hasPreCompFlag (f:fs) = hasPreCompFlag fs
    hasPreCompFlag [] = False
    --
    doExecute opts args = do
			    execute opts args
			      `fatalsHandledBy` failureHandler
			    exitWithCIO ExitSuccess
    --
    wrongNoOfArgsErr = 
      "Supply the header file followed by the binding file.\n\
      \The header file can be omitted if it is supplied in the binding file.\n\
      \The binding file can be omitted if the --precomp flag is given.\n"
    --
    -- exception handler
    --
    failureHandler err =
      do
	let msg   = ioeGetErrorString err
	    fnMsg = case ioeGetFileName err of
		       Nothing -> ""
		       Just s  -> " (file: `" ++ s ++ "')"
	hPutStrLnCIO stderr (msg ++ fnMsg)
	exitWithCIO $ ExitFailure 1

-- set up base configuration
--
setup :: CST s ()
setup  = do
	   setCPP     cpp
	   addCPPOpts cppopts
	   addHPaths  hpaths

-- output error message
--
raiseErrs      :: [String] -> CST s a
raiseErrs errs = do
		   hPutStrCIO stderr (concat errs)
		   hPutStrCIO stderr errTrailer
		   exitWithCIO $ ExitFailure 1

-- Process tasks
-- -------------

-- execute the compilation task
--
-- * if `Help' is present, emit the help message and ignore the rest
-- * if `Version' is present, do it first (and only once)
-- * actual compilation is only invoked if we have one or two extra arguments
--   (otherwise, it is just skipped)
--
execute :: [Flag] -> [FilePath] -> CST s ()
execute opts args | Help `elem` opts = help
		  | otherwise	     = 
  do
    let vs      = filter (== Version) opts
	opts'   = filter (/= Version) opts
    mapM_ processOpt (atMostOne vs ++ opts')

    let (headerFile, bndFile) = determineFileTypes args

    preCompFile <- getSwitch preCompSB

    unless (preCompFile==Nothing || null headerFile) $
      preCompileHeader headerFile (fromJust preCompFile)
        `fatalsHandledBy` ioErrorHandler

    let bndFileWithoutSuffix  = stripSuffix bndFile
    unless (null bndFile) $ do
      computeOutputName bndFileWithoutSuffix
      if preCompFile==Nothing
	then process headerFile bndFileWithoutSuffix
	       `fatalsHandledBy` ioErrorHandler
        else do
	     containsHeaderInfo <-
	       processPreComp (fromJust preCompFile) bndFileWithoutSuffix
	     when containsHeaderInfo $ process headerFile bndFileWithoutSuffix
	  `fatalsHandledBy` ioErrorHandler
  where
    atMostOne = (foldl (\_ x -> [x]) [])

    determineFileTypes [hfile, bfile]                = (hfile, bfile)
    determineFileTypes [file] | suffix file==hsuffix = (file, "")
			      | otherwise            = ("", file)
    determineFileTypes []                            = ("", "")

    ioErrorHandler ioerr = do
			     name <- getProgNameCIO
			     putStrCIO $
			       name ++ ": " ++ ioeGetErrorString ioerr ++ "\n"
			     exitWithCIO $ ExitFailure 1

-- emit help message
--
help :: CST s ()
help  = do
	  (version, copyright, disclaimer) <- getId
	  putStrCIO (usageInfo (header version copyright disclaimer) options)
	  putStrCIO trailer

-- process an option
--
-- * `Help' cannot occur 
--
processOpt                   :: Flag -> CST s ()
processOpt (CPPOpts cppopts)  = addCPPOpts cppopts
processOpt (CPP     cpp    )  = setCPP     cpp
processOpt (Dump    dt     )  = setDump    dt
processOpt (Keep           )  = setKeep
processOpt (Include dirs   )  = setInclude dirs
processOpt (Output  fname  )  = setOutput  fname
processOpt (OutDir  fname  )  = setOutDir  fname
processOpt (PreComp fname  )  = setPreComp fname
processOpt Version            = do
			          (version, _, _) <- getId 
			          putStrCIO (version ++ "\n")
processOpt (Error   msg    )  = abort      msg

-- emit error message and raise an error
--
abort     :: String -> CST s ()
abort msg  = do
	       hPutStrLnCIO stderr msg
	       hPutStrCIO stderr errTrailer
	       fatal "Error in command line options"

-- Compute the base name for all generated files (Haskell, C header, and .chi
-- file)
--
-- * The result is available from the `outputSB' switch
--
computeOutputName :: FilePath -> CST s ()
computeOutputName bndFileNoSuffix =
  do
    output <- getSwitch outputSB
    outDir <- getSwitch outDirSB
    let dir  = if      null outDir && null output then dirname bndFileNoSuffix
	       else if null outDir		  then dirname output
	       else				       outDir
    let base = if null output then basename bndFileNoSuffix
	       else                basename output
    setSwitch $ \sb -> sb {
		         outputSB = dir `addPath` base,
			 outDirSB = dir
		       }


-- set switches
-- ------------

-- set the options for the C proprocessor
--
-- * any header search path that is set with `-IDIR' is also added to
--   `hpathsSB'
--
addCPPOpts      :: String -> CST s ()
addCPPOpts opts  = 
  do
    let iopts = [opt | opt <- words opts, "-I" `isPrefixOf` opt, "-I-" /= opt]
    addHPaths . map (drop 2) $ iopts
    addOpts opts
  where
    addOpts opts  = setSwitch $ 
		      \sb -> sb {cppOptsSB = cppOptsSB sb ++ (' ':opts)}

-- set the program name of the C proprocessor
--
setCPP       :: FilePath -> CST s ()
setCPP fname  = setSwitch $ \sb -> sb {cppSB = fname}

-- add header file search paths
--
addHPaths       :: [FilePath] -> CST s ()
addHPaths paths  = setSwitch $ \sb -> sb {hpathsSB = paths ++ hpathsSB sb}

-- set the given dump option
--
setDump         :: DumpType -> CST s ()
setDump Trace    = setTraces $ \ts -> ts {tracePhasesSW  = True}
setDump GenBind  = setTraces $ \ts -> ts {traceGenBindSW = True}
setDump CTrav    = setTraces $ \ts -> ts {traceCTravSW   = True}
setDump CHS      = setTraces $ \ts -> ts {dumpCHSSW	 = True}

-- set flag to keep the pre-processed header file
--
setKeep :: CST s ()
setKeep  = setSwitch $ \sb -> sb {keepSB = True}

-- set the search directories for .chi files
--
-- * Several -i flags are accumulated. Later paths have higher priority.
--
-- * The current directory is always searched last because it is the
--   standard value in the compiler state.
--
setInclude :: String -> CST s ()
setInclude str = do
  let fp = makePath str ""
  setSwitch $ \sb -> sb {chiPathSB = fp ++ (chiPathSB sb)}
  where
    makePath ('\\':r:em)   path = makePath em (path ++ ['\\',r])
    makePath (' ':rem)	   path = makePath rem path
    makePath (':':rem)     ""   = makePath rem ""
    makePath (':':rem)	   path = path : makePath rem ""
    makePath ('/':':':rem) path = path : makePath rem ""
    makePath (r:emain)	   path = makePath emain (path ++ [r])
    makePath ""		   ""   = []
    makePath ""		   path = [path]

-- set the output file name
--
setOutput       :: FilePath -> CST s ()
setOutput fname  = do
		     when (suffix fname /= hssuffix) $
		       raiseErrs ["Output file should end in .hs!\n"]
		     setSwitch $ \sb -> sb {outputSB = stripSuffix fname}

-- set the output directory
--
setOutDir       :: FilePath -> CST s ()
setOutDir fname  = setSwitch $ \sb -> sb {outDirSB = fname}

-- set the name of the generated header file
--
setHeader       :: FilePath -> CST s ()
setHeader fname  = setSwitch $ \sb -> sb {headerSB = fname}

-- set the file name in which the precompiled header ends up
--
setPreComp      :: FilePath -> CST s ()
setPreComp fname = setSwitch $ \sb -> sb { preCompSB = Just fname }


-- compilation process
-- -------------------

-- read the binding module, construct a header, run it through CPP, read it,
-- and finally generate the Haskell target
--
-- * the header file name (first argument) may be empty; otherwise, it already
--   contains the right suffix
--
-- * the binding file name has been stripped of the .chs suffix
--
process                    :: FilePath -> FilePath -> CST s ()
process headerFile bndFile  =
  do
    -- load the Haskell binding module
    --
    (chsMod , warnmsgs) <- loadCHS bndFile
    putStrCIO warnmsgs
    traceCHSDump chsMod
    --
    -- extract CPP and inline-C embedded in the .chs file (all CPP and
    -- inline-C fragments are removed from the .chs tree and conditionals are
    -- replaced by structured conditionals)
    --
    (header, strippedCHSMod, warnmsgs) <- genHeader chsMod
    putStrCIO warnmsgs
    --
    -- create new header file, make it #include `headerFile', and emit
    -- CPP and inline-C of .chs file into the new header
    --
    outFName <- getSwitch outputSB
    let newHeaderFile = outFName ++ hsuffix
	preprocFile   = basename newHeaderFile ++ isuffix
    newHeader <- openFileCIO newHeaderFile WriteMode
    unless (null headerFile) $
      hPutStrLnCIO newHeader $ "#include \"" ++ headerFile ++ "\""
    mapM (hPutStrCIO newHeader) header
    hCloseCIO newHeader
    setHeader newHeaderFile
    --
    -- run C preprocessor over the header
    --
    cpp     <- getSwitch cppSB
    cppOpts <- getSwitch cppOptsSB
    let cmd  = unwords [cpp, cppOpts, newHeaderFile, ">" ++ preprocFile]
    tracePreproc cmd
    exitCode <- systemCIO cmd
    case exitCode of 
      ExitFailure _ -> fatal "Error during preprocessing custom header file"
      _		    -> nop
    --
    -- load and analyse the C header file
    --
    (cheader, warnmsgs) <- loadAttrC preprocFile
    putStrCIO warnmsgs
    --
    -- remove the custom header and the pre-processed header
    --
    keep <- getSwitch keepSB
    unless keep $
      removeFileCIO preprocFile
    --
    -- expand binding hooks into plain Haskell
    --
    (hsMod, chi, warnmsgs) <- expandHooks cheader strippedCHSMod
    putStrCIO warnmsgs
    --
    -- output the result
    --
    dumpCHS outFName hsMod True
    dumpCHI outFName chi		-- different suffix will be appended
  where
    tracePreproc cmd = putTraceStr tracePhasesSW $
		         "Invoking cpp as `" ++ cmd ++ "'...\n"
    traceCHSDump mod = do
			 flag <- traceSet dumpCHSSW
			 when flag $
			   (do
			      putStrCIO ("...dumping CHS to `" ++ chsName 
					 ++ "'...\n")
			      dumpCHS chsName mod False)

    chsName = basename bndFile ++ ".dump"

preCompileHeader :: FilePath -> FilePath -> CST s ()
preCompileHeader headerFile preCompFile =
  do
    let preprocFile  = basename headerFile ++ isuffix
    hpaths          <- getSwitch hpathsSB
    realHeaderFile  <- headerFile `fileFindInCIO` hpaths

    --
    -- run C preprocessor over the header
    --
    cpp     <- getSwitch cppSB
    cppOpts <- getSwitch cppOptsSB
    let cmd  = unwords [cpp, cppOpts, realHeaderFile, ">" ++ preprocFile]
    tracePreproc cmd
    exitCode <- systemCIO cmd
    case exitCode of
      ExitFailure _ -> fatal "Error during preprocessing"
      _             -> nop

    --
    -- load and analyse the C header file
    --
    (cheader, warnmsgs) <- loadAttrC preprocFile
    putStrCIO warnmsgs
    
    --
    -- save the attributed C to disk
    --
    liftIO $ putBinFileWithDict preCompFile (WithNameSupply cheader)

    --
    -- remove the pre-processed header
    --
    keep <- getSwitch keepSB
    unless keep $
      removeFileCIO preprocFile
 
    return ()
  where
    tracePreproc cmd = putTraceStr tracePhasesSW $
                         "Invoking cpp as `" ++ cmd ++ "'...\n"

processPreComp :: FilePath -> FilePath -> CST s Bool
processPreComp preCompFile bndFile = do
    
    -- load the Haskell binding module
    --
    (chsMod , warnmsgs) <- loadCHS bndFile
    putStrCIO warnmsgs
    traceCHSDump chsMod
    --
    -- extract CPP and inline-C embedded in the .chs file (all CPP and
    -- inline-C fragments are removed from the .chs tree and conditionals are
    -- replaced by structured conditionals)
    --
    (header, strippedCHSMod, warnmsgs) <- genHeader chsMod
    if not (null header) then return True else do
      putStrCIO warnmsgs
      --
      -- load and analyse the C header file
      --
      WithNameSupply cheader <- liftIO $ getBinFileWithDict preCompFile

      --
      -- expand binding hooks into plain Haskell
      --
      (hsMod, chi, warnmsgs) <- expandHooks cheader strippedCHSMod
      putStrCIO warnmsgs
      --
      -- output the result
      --
      outFName <- getSwitch outputSB
      let hsFile  = if null outFName then basename bndFile else outFName
      dumpCHS hsFile hsMod True
      dumpCHI hsFile chi		-- different suffix will be appended

      -- CHS file did not contain C declarations, so return False
      return False
  where
    traceCHSDump mod = do
			 flag <- traceSet dumpCHSSW
			 when flag $
			   (do
			      putStrCIO ("Reading CHS for `" ++ chsName 
					 ++ "'...\n")
			      dumpCHS chsName mod False)

    chsName = basename bndFile ++ ".dump"

-- dummy type so we can save and restore the name supply
data WithNameSupply a = WithNameSupply a

instance Binary a => Binary (WithNameSupply a) where
  put_ bh (WithNameSupply x) = do
    put_ bh x
    nameSupply <- saveRootNameSupply
    put_ bh nameSupply
  get bh = do
    x <- get bh
    nameSupply <- get bh
    restoreRootNameSupply nameSupply
    return (WithNameSupply x)
