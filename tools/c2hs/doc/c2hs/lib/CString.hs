-- (c) The FFI task force, 2000
--
-- C-specific marshalling support: string marshaling

module CString (

  -- representation of strings in C
  --
  CString,           -- = Ptr CChar
  CStringLen,        -- = (CString, Int)

  -- conversion of C strings into Haskell strings
  --
  peekCString,       -- :: CString    -> IO String
  peekCStringLen,    -- :: CStringLen -> IO String

  -- conversion of Haskell strings into C strings
  --
  newCString,        -- :: String -> IO CString
  newCStringLen,     -- :: String -> IO CStringLen

  -- conversion of Haskell strings into C strings using temporary storage
  --
  withCString,       -- :: String -> (CString    -> IO a) -> IO a
  withCStringLen,    -- :: String -> (CStringLen -> IO a) -> IO a

  -- conversion between Haskell and C characters *ignoring* the encoding
  --
  castCharToCChar,   -- :: Char -> CChar
  castCCharToChar,   -- :: CChar -> Char

  -- UnsafeCString: these might be more efficient than CStrings when
  -- passing the string to an "unsafe" foreign import.  NOTE: this
  -- feature might be removed in favour of a more general approach in
  -- the future.
  --
  UnsafeCString,     -- data UnsafeCString
  withUnsafeCString  -- :: String -> (UnsafeCString -> IO a) -> IO a
) where

import Monad
import Char

import MarshalArray
import MarshalAlloc
import Ptr
import NewStorable
import CTypes
import Int


-- representation of strings in C
-- ------------------------------

type CString    = Ptr CChar		-- conventional NUL terminates strings
type CStringLen = (CString, Int)	-- strings with explicit length


-- exported functions
-- ------------------
--
-- * the following routines apply the default conversion when converting the
--   C-land character encoding into the Haskell-land character encoding
--
--   ** NOTE: The current implementation doesn't handle conversions yet! **
--
-- * the routines using an explicit length tolerate NUL characters in the
--   middle of a string
--

-- marshal a NUL terminated C string into a Haskell string 
--
peekCString    :: CString -> IO String
peekCString cp  = liftM cCharsToChars $ peekArray0 nUL cp

-- marshal a C string with explicit length into a Haskell string 
--
peekCStringLen           :: CStringLen -> IO String
peekCStringLen (cp, len)  = liftM cCharsToChars $ peekArray len cp

-- marshal a Haskell string into a NUL terminated C strings
--
-- * the Haskell string may *not* contain any NUL characters
--
-- * new storage is allocated for the C string and must be explicitly freed
--
newCString :: String -> IO CString
newCString  = newArray0 nUL . charsToCChars

-- marshal a Haskell string into a C string (ie, character array) with
-- explicit length information
--
-- * new storage is allocated for the C string and must be explicitly freed
--
newCStringLen     :: String -> IO CStringLen
newCStringLen str  = liftM (pairLength str) $ newArray (charsToCChars str)

-- marshal a Haskell string into a NUL terminated C strings using temporary
-- storage
--
-- * the Haskell string may *not* contain any NUL characters
--
-- * see the lifetime constraints of `MarshalAlloc.alloca'
--
withCString :: String -> (CString -> IO a) -> IO a
withCString  = withArray0 nUL . charsToCChars

-- marshal a Haskell string into a NUL terminated C strings using temporary
-- storage
--
-- * the Haskell string may *not* contain any NUL characters
--
-- * see the lifetime constraints of `MarshalAlloc.alloca'
--
withCStringLen         :: String -> (CStringLen -> IO a) -> IO a
withCStringLen str act  = withArray (charsToCChars str) $ act . pairLength str

-- auxilliary definitions
-- ----------------------

-- C's end of string character
--
nUL :: CChar
nUL  = 0

-- pair a C string with the length of the given Haskell string
--
pairLength :: String -> CString -> CStringLen
pairLength  = flip (,) . length

-- cast [CChar] to [Char]
--
cCharsToChars :: [CChar] -> [Char]
cCharsToChars  = map castCCharToChar

-- cast [Char] to [CChar]
--
charsToCChars :: [Char] -> [CChar]
charsToCChars  = map castCharToCChar

castCCharToChar :: CChar -> Char
castCCharToChar  = chr . fromIntegral

castCharToCChar :: Char -> CChar
castCharToCChar  = fromIntegral . ord


-- unsafe CStrings
-- ---------------

newtype UnsafeCString = UnsafeCString (Ptr CChar)

withUnsafeCString s f = withCString s (\p -> f (UnsafeCString p))
