--  C->Haskell Compiler: traversals of C structure tree
--
--  Author : Manuel M. T. Chakravarty
--  Created: 16 October 99
--
--  Version $Revision: 1.1 $ from $Date: 2004/11/21 21:05:27 $
--
--  Copyright (c) [1999..2001] Manuel M. T. Chakravarty
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
--  This modules provides for traversals of C structure trees.  The C
--  traversal monad supports traversals that need convenient access to the
--  attributes of an attributed C structure tree.  The monads state can still
--  be extended.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  Handling of redefined tag values
--  --------------------------------
--
--  Structures allow both
--
--    struct s {...} ...;
--    struct s       ...;
--
--  and
--
--    struct s       ...;	/* this is called a forward reference */
--    struct s {...} ...;
--
--  In contrast enumerations only allow (in ANSI C)
--
--    enum e {...} ...;
--    enum e       ...;
--
--  The function `defTag' handles both types and establishes an object
--  association from the tag identifier in the empty declaration (ie, the one
--  without `{...}') to the actually definition of the structure of
--  enumeration.  This implies that when looking for the details of a
--  structure or enumeration, possibly a chain of references on tag
--  identifiers has to be chased.  Note that the object association attribute
--  is _not_defined_ when the `{...}'  part is present in a declaration.
--
--- TODO ----------------------------------------------------------------------
--
-- * `extractStruct' doesn't account for forward declarations that have no
--   full declaration yet; if `extractStruct' is called on such a declaration, 
--   we have a user error, but currently an internal error is raised
--

module CTrav (CT, readCT, transCT, getCHeaderCT, runCT, throwCTExc, ifCTExc,
	      raiseErrorCTExc,
	      enter, enterObjs, leave, leaveObjs, defObj, findObj,
	      findObjShadow, defTag, findTag, findTagShadow,
	      applyPrefixToNameSpaces, getDefOf, refersToDef, refersToNewDef,
	      getDeclOf, findTypeObjMaybe, findTypeObj, findValueObj,
	      findFunObj,
	      --
	      -- C structure tree query functions
	      --
	      isTypedef, simplifyDecl, declrFromDecl, declrNamed,
	      declaredDeclr, declaredName, structMembers, expandDecl,
	      structName, enumName, tagName, isArrDeclr, isPtrDeclr, dropPtrDeclr,
	      isPtrDecl, isFunDeclr, structFromDecl, funResultAndArgs,
	      chaseDecl, findAndChaseDecl, checkForAlias,
	      checkForOneAliasName, lookupEnum, lookupStructUnion,
	      lookupDeclOrTag)
where

import List       (find)
import Maybe	  (fromMaybe)
import Monad	  (liftM)
import Control.Exception (assert)

import Position   (Position, Pos(..), nopos)
import Errors	  (interr)
import Idents	  (Ident, dumpIdent, identToLexeme)
import Attributes (Attr(..), newAttrsOnlyPos)

import C2HSState  (CST, nop, readCST, transCST, runCST, raiseError, catchExc,
		   throwExc, Traces(..), putTraceStr)
import CAST
import CAttrs     (AttrC, getCHeader, enterNewRangeC, enterNewObjRangeC,
		   leaveRangeC, leaveObjRangeC, addDefObjC, lookupDefObjC,
		   lookupDefObjCShadow, addDefTagC, lookupDefTagC,
		   lookupDefTagCShadow, applyPrefix, getDefOfIdentC,
		   setDefOfIdentC, updDefOfIdentC, CObj(..), CTag(..),
		   CDef(..)) 


-- the C traversal monad
-- ---------------------

-- C traversal monad (EXPORTED ABSTRACTLY)
--
type CState s    = (AttrC, s)
type CT     s a  = CST (CState s) a

-- read attributed struture tree
--
readAttrCCT        :: (AttrC -> a) -> CT s a
readAttrCCT reader  = readCST $ \(ac, _) -> reader ac

-- transform attributed structure tree
--
transAttrCCT       :: (AttrC -> (AttrC, a)) -> CT s a
transAttrCCT trans  = transCST $ \(ac, s) -> let
					       (ac', r) = trans ac
					     in
					     ((ac', s), r)

-- access to the user-defined state
--

-- read user-defined state (EXPORTED)
--
readCT        :: (s -> a) -> CT s a
readCT reader  = readCST $ \(_, s) -> reader s

-- transform user-defined state (EXPORTED)
--
transCT       :: (s -> (s, a)) -> CT s a
transCT trans  = transCST $ \(ac, s) -> let
					  (s', r) = trans s
					in
					((ac, s'), r)

-- usage of a traversal monad
--

-- get the raw C header from the monad (EXPORTED)
--
getCHeaderCT :: CT s CHeader
getCHeaderCT  = readAttrCCT getCHeader

-- execute a traversal monad (EXPORTED)
--
-- * given a traversal monad, an attribute structure tree, and a user
--   state, the transformed structure tree and monads result are returned
--
runCT        :: CT s a -> AttrC -> s -> CST t (AttrC, a)
runCT m ac s  = runCST m' (ac, s)
		where
		  m' = do
			 r <- m
			 (ac, _) <- readCST id
			 return (ac, r)


-- exception handling
-- ------------------

-- exception identifier
--
ctExc :: String
ctExc  = "ctExc"

-- throw an exception  (EXPORTED)
--
throwCTExc :: CT s a
throwCTExc  = throwExc ctExc "Error during traversal of a C structure tree"

-- catch a `ctExc'  (EXPORTED)
--
ifCTExc           :: CT s a -> CT s a -> CT s a
ifCTExc m handler  = m `catchExc` (ctExc, const handler)

-- raise an error followed by throwing a CT exception (EXPORTED)
--
raiseErrorCTExc          :: Position -> [String] -> CT s a
raiseErrorCTExc pos errs  = raiseError pos errs >> throwCTExc


-- attribute manipulation
-- ----------------------

-- name spaces
--

-- enter a new local range (EXPORTED)
--
enter :: CT s ()
enter  = transAttrCCT $ \ac -> (enterNewRangeC ac, ())

-- enter a new local range, only for objects (EXPORTED)
--
enterObjs :: CT s ()
enterObjs  = transAttrCCT $ \ac -> (enterNewObjRangeC ac, ())

-- leave the current local range (EXPORTED)
--
leave :: CT s ()
leave  = transAttrCCT $ \ac -> (leaveRangeC ac, ())

-- leave the current local range, only for objects (EXPORTED)
--
leaveObjs :: CT s ()
leaveObjs  = transAttrCCT $ \ac -> (leaveObjRangeC ac, ())

-- enter an object definition into the object name space (EXPORTED)
--
-- * if a definition of the same name was already present, it is returned 
--
defObj         :: Ident -> CObj -> CT s (Maybe CObj)
defObj ide obj  = transAttrCCT $ \ac -> addDefObjC ac ide obj

-- find a definition in the object name space (EXPORTED)
--
findObj     :: Ident -> CT s (Maybe CObj)
findObj ide  = readAttrCCT $ \ac -> lookupDefObjC ac ide

-- find a definition in the object name space; if nothing found, try 
-- whether there is a shadow identifier that matches (EXPORTED)
--
findObjShadow     :: Ident -> CT s (Maybe (CObj, Ident))
findObjShadow ide  = readAttrCCT $ \ac -> lookupDefObjCShadow ac ide

-- enter a tag definition into the tag name space (EXPORTED)
--
-- * empty definitions of structures get overwritten with complete ones and a
--   forward reference is added to their tag identifier; furthermore, both
--   structures and enums may be referenced using an empty definition when
--   there was a full definition earlier and in this case there is also an
--   object association added; otherwise, if a definition of the same name was
--   already present, it is returned (see DOCU section)
--
-- * it is checked that the first occurence of an enumeration tag is
--   accompanied by a full definition of the enumeration
--
defTag         :: Ident -> CTag -> CT s (Maybe CTag)
defTag ide tag  = 
  do
    otag <- transAttrCCT $ \ac -> addDefTagC ac ide tag
    case otag of
      Nothing      -> do
		        assertIfEnumThenFull tag
		        return Nothing			-- no collision
      Just prevTag -> case isRefinedOrUse prevTag tag of
			 Nothing                 -> return otag
			 Just (fullTag, foreIde) -> do
			   transAttrCCT $ \ac -> addDefTagC ac ide fullTag
			   foreIde `refersToDef` TagCD fullTag
			   return Nothing		-- transparent for env
  where
    -- compute whether we have the case of a non-conflicting redefined tag
    -- definition, and if so, return the full definition and the foreward 
    -- definition's tag identifier
    --
    -- * the first argument contains the _previous_ definition
    --
    -- * in the case of a structure, a foreward definition after a full
    --   definition is allowed, so we have to handle this case; enumerations
    --   don't allow foreward definitions
    --
    -- * there may also be multiple foreward definition; if we have two of
    --   them here, one is arbitrarily selected to take the role of the full
    --   definition 
    --
    isRefinedOrUse     (StructUnionCT (CStruct _ (Just ide) [] _))
		   tag@(StructUnionCT (CStruct _ (Just _  ) _  _)) = 
      Just (tag, ide)
    isRefinedOrUse tag@(StructUnionCT (CStruct _ (Just _  ) _  _))
		       (StructUnionCT (CStruct _ (Just ide) [] _)) = 
      Just (tag, ide)
    isRefinedOrUse tag@(EnumCT        (CEnum (Just _  ) _  _))
		       (EnumCT        (CEnum (Just ide) [] _))     = 
      Just (tag, ide)
    isRefinedOrUse _ _					           = Nothing

-- find an definition in the tag name space (EXPORTED)
--
findTag     :: Ident -> CT s (Maybe CTag)
findTag ide  = readAttrCCT $ \ac -> lookupDefTagC ac ide

-- find an definition in the tag name space; if nothing found, try 
-- whether there is a shadow identifier that matches (EXPORTED)
--
findTagShadow     :: Ident -> CT s (Maybe (CTag, Ident))
findTagShadow ide  = readAttrCCT $ \ac -> lookupDefTagCShadow ac ide

-- enrich the object and tag name space with identifiers obtained by dropping
-- the given prefix from the identifiers already in the name space (EXPORTED)
--
-- * if a new identifier would collides with an existing one, the new one is
--   discarded, ie, all associations that existed before the transformation
--   started are still in effect after the transformation
-- 
applyPrefixToNameSpaces        :: String -> CT s ()
applyPrefixToNameSpaces prefix  = 
  transAttrCCT $ \ac -> (applyPrefix ac prefix, ())

-- definition attribute
--

-- get the definition of an identifier (EXPORTED) 
--
-- * the attribute must be defined, ie, a definition must be associated with
--   the given identifier
--
getDefOf     :: Ident -> CT s CDef
getDefOf ide  = do
		  def <- readAttrCCT $ \ac -> getDefOfIdentC ac ide
		  assert (not . isUndef $ def) $
		    return def

-- set the definition of an identifier (EXPORTED) 
--
refersToDef         :: Ident -> CDef -> CT s ()
refersToDef ide def  = transAttrCCT $ \akl -> (setDefOfIdentC akl ide def, ())

-- update the definition of an identifier (EXPORTED) 
--
refersToNewDef         :: Ident -> CDef -> CT s ()
refersToNewDef ide def  = 
  transAttrCCT $ \akl -> (updDefOfIdentC akl ide def, ())

-- get the declarator of an identifier (EXPORTED)
--
getDeclOf     :: Ident -> CT s CDecl
getDeclOf ide  = 
  do
    traceEnter
    def <- getDefOf ide
    case def of
      UndefCD    -> interr "CTrav.getDeclOf: Undefined!"
      DontCareCD -> interr "CTrav.getDeclOf: Don't care!"
      TagCD _    -> interr "CTrav.getDeclOf: Illegal tag!"
      ObjCD obj  -> case obj of
		      TypeCO    decl -> traceTypeCO >>
					return decl
		      ObjCO     decl -> traceObjCO >>
					return decl
		      EnumCO    _ _  -> illegalEnum
		      BuiltinCO	     -> illegalBuiltin
  where
    illegalEnum    = interr "CTrav.getDeclOf: Illegal enum!"
    illegalBuiltin = interr "CTrav.getDeclOf: Attempted to get declarator of \
			    \builtin entity!"
		     -- if the latter ever becomes necessary, we have to
		     -- change the representation of builtins and give them
		     -- some dummy declarator
    traceEnter  = traceCTrav $ 
		    "Entering `getDeclOf' for `" ++ identToLexeme ide 
		    ++ "'...\n"
    traceTypeCO = traceCTrav $ 
		    "...found a type object.\n"
    traceObjCO  = traceCTrav $ 
		    "...found a vanilla object.\n"


-- convenience functions
--

-- find a type object in the object name space; returns `nothing' if the
-- identifier is not defined (EXPORTED)
--
-- * if the second argument is `True', use `findObjShadow'
--
findTypeObjMaybe                :: Ident -> Bool -> CT s (Maybe (CObj, Ident))
findTypeObjMaybe ide useShadows  = 
  do
    oobj <- if useShadows 
	    then findObjShadow ide 
	    else liftM (fmap (\obj -> (obj, ide))) $ findObj ide
    case oobj of
      Just obj@(TypeCO _ , _) -> return $ Just obj
      Just obj@(BuiltinCO, _) -> return $ Just obj
      Just _                  -> typedefExpectedErr ide
      Nothing		      -> return $ Nothing

-- find a type object in the object name space; raises an error and exception
-- if the identifier is not defined (EXPORTED)
--
-- * if the second argument is `True', use `findObjShadow'
--
findTypeObj                :: Ident -> Bool -> CT s (CObj, Ident)
findTypeObj ide useShadows  = do
  oobj <- findTypeObjMaybe ide useShadows
  case oobj of
    Nothing  -> unknownObjErr ide
    Just obj -> return obj

-- find an object, function, or enumerator in the object name space; raises an
-- error and exception if the identifier is not defined (EXPORTED)
--
-- * if the second argument is `True', use `findObjShadow'
--
findValueObj                :: Ident -> Bool -> CT s (CObj, Ident)
findValueObj ide useShadows  = 
  do
    oobj <- if useShadows 
	    then findObjShadow ide 
	    else liftM (fmap (\obj -> (obj, ide))) $ findObj ide
    case oobj of
      Just obj@(ObjCO  _  , _) -> return obj
      Just obj@(EnumCO _ _, _) -> return obj
      Just _                   -> unexpectedTypedefErr (posOf ide)
      Nothing		       -> unknownObjErr ide

-- find a function in the object name space; raises an error and exception if
-- the identifier is not defined (EXPORTED) 
--
-- * if the second argument is `True', use `findObjShadow'
--
findFunObj               :: Ident -> Bool -> CT s  (CObj, Ident)
findFunObj ide useShadows = 
  do
    (obj, ide') <- findValueObj ide useShadows
    case obj of
      EnumCO _ _  -> funExpectedErr (posOf ide)
      ObjCO  decl -> do
		       let declr = ide' `declrFromDecl` decl
		       assertFunDeclr (posOf ide) declr
		       return (obj, ide')


-- C structure tree query routines
-- -------------------------------

-- test if this is a type definition specification (EXPORTED)
--
isTypedef                   :: CDecl -> Bool
isTypedef (CDecl specs _ _)  = 
  not . null $ [() | CStorageSpec (CTypedef _) <- specs]

-- discard all declarators but the one declaring the given identifier
-- (EXPORTED) 
--
-- * the declaration must contain the identifier
--
simplifyDecl :: Ident -> CDecl -> CDecl
ide `simplifyDecl` (CDecl specs declrs at) =
  case find (`declrPlusNamed` ide) declrs of
    Nothing    -> err
    Just declr -> CDecl specs [declr] at
  where
    (Just declr, _, _) `declrPlusNamed` ide = declr `declrNamed` ide
    _		       `declrPlusNamed` _   = False
    --
    err = interr $ "CTrav.simplifyDecl: Wrong C object!\n\
		   \  Looking for `" ++ identToLexeme ide ++ "' in decl \
		   \at " ++ show (posOf at)

-- extract the declarator that declares the given identifier (EXPORTED)
--
-- * the declaration must contain the identifier
--
declrFromDecl            :: Ident -> CDecl -> CDeclr
ide `declrFromDecl` decl  = 
  let CDecl _ [(Just declr, _, _)] _ = ide `simplifyDecl` decl
  in
  declr

-- tests whether the given declarator has the given name (EXPORTED)
--
declrNamed             :: CDeclr -> Ident -> Bool
declr `declrNamed` ide  = declrName declr == Just ide

-- get the declarator of a declaration that has at most one declarator
-- (EXPORTED) 
--
declaredDeclr                              :: CDecl -> Maybe CDeclr
declaredDeclr (CDecl _ []               _)  = Nothing
declaredDeclr (CDecl _ [(odeclr, _, _)] _)  = odeclr
declaredDeclr decl		            = 
  interr $ "CTrav.declaredDeclr: Too many declarators!\n\
	   \  Declaration at " ++ show (posOf decl)

-- get the name declared by a declaration that has exactly one declarator
-- (EXPORTED) 
--
declaredName      :: CDecl -> Maybe Ident
declaredName decl  = declaredDeclr decl >>= declrName

-- obtains the member definitions and the tag of a struct (EXPORTED)
--
-- * member definitions are expanded
--
structMembers :: CStructUnion -> ([CDecl], CStructTag)
structMembers (CStruct tag _ members _) = (concat . map expandDecl $ members,
					   tag)

-- expand declarators declaring more than one identifier into multiple
-- declarators, eg, `int x, y;' becomes `int x; int y;' (EXPORTED)
--
expandDecl			  :: CDecl -> [CDecl]
expandDecl (CDecl specs decls at)  = 
  map (\decl -> CDecl specs [decl] at) decls

-- get a struct's name (EXPORTED)
--
structName		        :: CStructUnion -> Maybe Ident
structName (CStruct _ oide _ _)  = oide

-- get an enum's name (EXPORTED)
--
enumName	          :: CEnum -> Maybe Ident
enumName (CEnum oide _ _)  = oide

-- get a tag's name (EXPORTED)
--
-- * fail if the tag is anonymous
--
tagName     :: CTag -> Ident
tagName tag  =
  case tag of
   StructUnionCT struct -> maybe err id $ structName struct
   EnumCT	 enum   -> maybe err id $ enumName   enum
  where
    err = interr "CTrav.tagName: Anonymous tag definition"

-- checks whether the given declarator defines an object that is a pointer to
-- some other type (EXPORTED)
--
-- * as far as parameter passing is concerned, arrays are also pointer
--
isPtrDeclr                                 :: CDeclr -> Bool
isPtrDeclr (CPtrDeclr _ (CVarDeclr _ _)   _)  = True
isPtrDeclr (CPtrDeclr _ declr             _)  = isPtrDeclr declr
isPtrDeclr (CArrDeclr (CVarDeclr _ _) _ _ _)  = True
isPtrDeclr (CArrDeclr declr _ _           _)  = isPtrDeclr declr
isPtrDeclr (CFunDeclr declr _ _           _)  = isPtrDeclr declr
isPtrDeclr _                                  = False

-- checks whether the given declarator defines an object that is an array of
-- some other type (EXPORTED)
--
-- * difference between arrays and pure pointers is important for size
--   calculations
--
isArrDeclr                                 :: CDeclr -> Bool
isArrDeclr (CArrDeclr declr _ _         _)  = True
isArrDeclr _                                = False

-- drops the first pointer level from the given declarator (EXPORTED)
--
-- * the declarator must declare a pointer object
--
-- FIXME: this implementation isn't nice, because we retain the `CVarDeclr'
--	  unchanged; as the declarator is changed, we should maybe make this
--	  into an anonymous declarator and also change its attributes
--
dropPtrDeclr                                          :: CDeclr -> CDeclr
dropPtrDeclr (CPtrDeclr qs declr@(CVarDeclr _ _) ats)  = declr
dropPtrDeclr (CPtrDeclr qs  declr                ats)  = 
  let declr' = dropPtrDeclr declr
  in
  CPtrDeclr qs declr' ats
dropPtrDeclr (CArrDeclr declr@(CVarDeclr _ _) _ _ _)   = declr
dropPtrDeclr (CArrDeclr declr                tq e ats) =
  let declr' = dropPtrDeclr declr
  in
  CArrDeclr declr' tq e ats
dropPtrDeclr (CFunDeclr declr args vari         ats)   =
  let declr' = dropPtrDeclr declr
  in
  CFunDeclr declr' args vari ats
dropPtrDeclr _                                         =
  interr "CTrav.dropPtrDeclr: No pointer!"

-- checks whether the given declaration defines a pointer object (EXPORTED)
--
-- * there may only be a single declarator in the declaration
--
isPtrDecl                                  :: CDecl -> Bool
isPtrDecl (CDecl _ []                   _)  = False
isPtrDecl (CDecl _ [(Just declr, _, _)] _)  = isPtrDeclr declr
isPtrDecl _				    =
  interr "CTrav.isPtrDecl: There was more than one declarator!"

-- checks whether the given declarator defines a function object (EXPORTED)
--
isFunDeclr                                   :: CDeclr -> Bool
isFunDeclr (CPtrDeclr _ declr             _)  = isFunDeclr declr
isFunDeclr (CArrDeclr declr _ _           _)  = isFunDeclr declr
isFunDeclr (CFunDeclr (CVarDeclr _ _) _ _ _)  = True
isFunDeclr (CFunDeclr declr _ _           _)  = isFunDeclr declr
isFunDeclr _				      = False

-- extract the structure from the type specifiers of a declaration (EXPORTED)
--
structFromDecl                       :: Position -> CDecl -> CT s CStructUnion
structFromDecl pos (CDecl specs _ _)  =
  case head [ts | CTypeSpec ts <- specs] of
    CSUType su _ -> extractStruct pos (StructUnionCT su)
    _            -> structExpectedErr pos

-- extracts the arguments from a function declaration (must be a unique
-- declarator) and constructs a declaration for the result of the function
-- (EXPORTED) 
--
-- * the boolean result indicates whether the function is variadic
--
funResultAndArgs :: CDecl -> ([CDecl], CDecl, Bool)
funResultAndArgs (CDecl specs [(Just declr, _, _)] _) =
  let (args, declr', variadic) = funArgs declr
      result	               = CDecl specs [(Just declr', Nothing, Nothing)]
				       (newAttrsOnlyPos nopos)
  in
  (args, result, variadic)
  where
    funArgs (CFunDeclr var@(CVarDeclr _ _) args variadic  _) = 
      (args, var, variadic)
    funArgs (CPtrDeclr qs declr                          at) = 
      let (args, declr', variadic) = funArgs declr
      in
      (args, CPtrDeclr qs declr' at, variadic)
    funArgs (CArrDeclr declr tqs oe                      at) =
      let (args, declr', variadic) = funArgs declr
      in
      (args, CArrDeclr declr' tqs oe at, variadic)
    funArgs (CFunDeclr declr args var                    at) =
      let (args, declr', variadic) = funArgs declr
      in
      (args, CFunDeclr declr' args var at, variadic)
    funArgs _                                           =
      interr "CTrav.funResultAndArgs: Illegal declarator!"

-- name chasing
--

-- find the declarator identified by the given identifier; if the declarator
-- is itself only a `typedef'ed name, the operation recursively searches for
-- the declarator associated with that name (this is called ``typedef
-- chasing'') (EXPORTED)
--
-- * if `ind = True', we have to hop over one indirection
--
-- * remove all declarators except the one we just looked up
--
chaseDecl         :: Ident -> Bool -> CT s CDecl
--
-- * cycles are no issue, as they cannot occur in a correct C header (we would 
--   have spotted the problem during name analysis)
--
chaseDecl ide ind  = 
  do
    traceEnter
    cdecl     <- getDeclOf ide
    let sdecl  = ide `simplifyDecl` cdecl
    case extractAlias sdecl ind of
      Just    (ide', ind') -> chaseDecl ide' ind'
      Nothing              -> return sdecl
  where
    traceEnter = traceCTrav $ 
		   "Entering `chaseDecl' for `" ++ identToLexeme ide 
		   ++ "' " ++ (if ind then "" else "not ") 
		   ++ "following indirections...\n"

-- find type object in object name space and then chase it (EXPORTED)
--
-- * see also `chaseDecl'
--
-- * also create an object association from the given identifier to the object
--   that it _directly_ represents
--
-- * if the third argument is `True', use `findObjShadow'
--
findAndChaseDecl                    :: Ident -> Bool -> Bool -> CT s CDecl
findAndChaseDecl ide ind useShadows  =
  do
    (obj, ide') <- findTypeObj ide useShadows   -- is there an object def?
    ide  `refersToNewDef` ObjCD obj
    ide' `refersToNewDef` ObjCD obj	        -- assoc needed for chasing
    chaseDecl ide' ind

-- given a declaration (which must have exactly one declarator), if the
-- declarator is an alias, chase it to the actual declaration (EXPORTED)
--
checkForAlias      :: CDecl -> CT s (Maybe CDecl)
checkForAlias decl  =
  case extractAlias decl False of
    Nothing        -> return Nothing
    Just (ide', _) -> liftM Just $ chaseDecl ide' False

-- given a declaration (which must have exactly one declarator), if the
-- declarator is an alias, yield the alias name; *no* chasing (EXPORTED)
--
checkForOneAliasName      :: CDecl -> Maybe Ident
checkForOneAliasName decl  = fmap fst $ extractAlias decl False


-- smart lookup
--

-- for the given identifier, either find an enumeration in the tag name space
-- or a type definition referring to an enumeration in the object name space;
-- raises an error and exception if the identifier is not defined (EXPORTED)
--
-- * if the second argument is `True', use `findTagShadow'
--
lookupEnum               :: Ident -> Bool -> CT s CEnum
lookupEnum ide useShadows =
  do
    otag <- if useShadows 
	    then liftM (fmap fst) $ findTagShadow ide
	    else findTag ide
    case otag of
      Just (StructUnionCT _   ) -> enumExpectedErr ide  -- wrong tag definition
      Just (EnumCT        enum) -> return enum	        -- enum tag definition
      Nothing                   -> do			-- no tag definition
	(CDecl specs _ _) <- findAndChaseDecl ide False useShadows
	case head [ts | CTypeSpec ts <- specs] of
	  CEnumType enum _ -> return enum
	  _                -> enumExpectedErr ide

-- for the given identifier, either find a struct/union in the tag name space
-- or a type definition referring to a struct/union in the object name space;
-- raises an error and exception if the identifier is not defined (EXPORTED)
--
-- * if `ind = True', the identifier names a reference type to the searched
--   for struct/union
--
-- * typedef chasing is used only if there is no tag of the same name or an
--   indirection (ie, `ind = True') is explicitly required
--
-- * if the third argument is `True', use `findTagShadow'
--
-- * when finding a forward definition of a tag, follow it to the real
--   definition
--
lookupStructUnion :: Ident -> Bool -> Bool -> CT s CStructUnion
lookupStructUnion ide ind useShadows
  | ind       = chase
  | otherwise =
    do
      otag <- if useShadows 
	      then liftM (fmap fst) $ findTagShadow ide
	      else findTag ide
      maybe chase (extractStruct (posOf ide)) otag  -- `chase' if `Nothing'
  where
    chase =
      do
	decl <- findAndChaseDecl ide ind useShadows
	structFromDecl (posOf ide) decl

-- for the given identifier, check for the existance of both a type definition
-- or a struct, union, or enum definition (EXPORTED)
--
-- * if a typedef and a tag exists, the typedef takes precedence
--
-- * typedefs are chased
--
-- * if the second argument is `True', look for shadows, too
--
lookupDeclOrTag		       :: Ident -> Bool -> CT s (Either CDecl CTag)
lookupDeclOrTag ide useShadows  = do
  oobj <- findTypeObjMaybe ide useShadows
  case oobj of
    Just (_, ide) -> liftM Left $ findAndChaseDecl ide False False 
					           -- already did check shadows
    Nothing       -> do
                       otag <- if useShadows 
			       then liftM (fmap fst) $ findTagShadow ide
			       else findTag ide
		       case otag of
			 Nothing  -> unknownObjErr ide
			 Just tag -> return $ Right tag


-- auxiliary routines (internal)
--

-- if the given declaration (which may have at most one declarator) is a
-- `typedef' alias, yield the referenced name
--
-- * a `typedef' alias has one of the following forms
--
--     <specs> at  x, ...;
--     <specs> at *x, ...;
--
--   where `at' is the alias type, which has been defined by a `typedef', and
--   <specs> are arbitrary specifiers and qualifiers.  Note that `x' may be a
--   variable, a type name (if `typedef' is in <specs>), or be entirely
--   omitted.
--
-- * if `ind = True', the alias may be via an indirection
--
-- * if `ind = True' and the alias is _not_ over an indirection, yield `True'; 
--   otherwise `False' (ie, the ability to hop over an indirection is consumed)
--
-- * this may be an anonymous declaration, ie, the name in `CVarDeclr' may be
--   omitted or there may be no declarator at all
--
extractAlias :: CDecl -> Bool -> Maybe (Ident, Bool)
extractAlias decl@(CDecl specs _ _) ind =
  case [ts | CTypeSpec ts <- specs] of
    [CTypeDef ide' _] ->	      		-- type spec is aliased ident
      case declaredDeclr decl of
	Nothing				       -> Just (ide', ind)
	Just (CVarDeclr _ _                  ) -> Just (ide', ind)
	Just (CPtrDeclr [_] (CVarDeclr _ _) _)
	  | ind                                -> Just (ide', False)
	  | otherwise			       -> Nothing
	_                                      -> Nothing
    _                 -> Nothing

-- if the given tag is a forward declaration of a structure, follow the
-- reference to the full declaration
--
-- * the recursive call is not dangerous as there can't be any cycles
--
extractStruct                        :: Position -> CTag -> CT s CStructUnion
extractStruct pos (EnumCT        _ )  = structExpectedErr pos
extractStruct pos (StructUnionCT su)  =
  case su of
    CStruct _ (Just ide') [] _ -> do		-- found forward definition
				    def <- getDefOf ide'
				    case def of
				      TagCD tag -> extractStruct pos tag
				      _	        -> err
    _			       -> return su
  where
    err = interr "CTrav.extractStruct: Illegal reference!"

-- yield the name declared by a declarator if any
--
declrName                          :: CDeclr -> Maybe Ident
declrName (CVarDeclr oide       _)  = oide
declrName (CPtrDeclr _ declr    _)  = declrName declr
declrName (CArrDeclr declr  _ _ _)  = declrName declr
declrName (CFunDeclr declr  _ _ _)  = declrName declr

-- raise an error if the given declarator does not declare a C function or if
-- the function is supposed to return an array (the latter is illegal in C)
--
assertFunDeclr :: Position -> CDeclr -> CT s ()
assertFunDeclr pos (CArrDeclr (CFunDeclr (CVarDeclr _ _) _ _ _) _ _ _) =
  illegalFunResultErr pos
assertFunDeclr pos            (CFunDeclr (CVarDeclr _ _) _ _ _)        =
  nop -- everything is ok
assertFunDeclr pos            (CFunDeclr declr           _ _ _)        =
  assertFunDeclr pos declr
assertFunDeclr pos            (CPtrDeclr _ declr             _)        =
  assertFunDeclr pos declr
assertFunDeclr pos            (CArrDeclr declr           _ _ _)        =
  assertFunDeclr pos declr
assertFunDeclr pos _					             = 
  funExpectedErr pos

-- raise an error if the given tag defines an enumeration, but does not fully
-- define it
--
assertIfEnumThenFull                          :: CTag -> CT s ()
assertIfEnumThenFull (EnumCT (CEnum _ [] at))  = enumForwardErr (posOf at)
assertIfEnumThenFull _			       = nop

-- trace for this module
--
traceCTrav :: String -> CT s ()
traceCTrav  = putTraceStr traceCTravSW


-- error messages
-- --------------

unknownObjErr     :: Ident -> CT s a
unknownObjErr ide  =
  raiseErrorCTExc (posOf ide) 
    ["Unknown identifier!",
     "Cannot find a definition for `" ++ identToLexeme ide ++ "' in the \
     \header file."]

typedefExpectedErr      :: Ident -> CT s a
typedefExpectedErr ide  =   
  raiseErrorCTExc (posOf ide) 
    ["Expected type definition!",
     "The identifier `" ++ identToLexeme ide ++ "' needs to be a C type name."]

unexpectedTypedefErr     :: Position -> CT s a
unexpectedTypedefErr pos  =   
  raiseErrorCTExc pos 
    ["Unexpected type name!",
     "An object, function, or enum constant is required here."]

illegalFunResultErr      :: Position -> CT s a
illegalFunResultErr pos  =
  raiseErrorCTExc pos ["Function cannot return an array!",
		       "ANSI C does not allow functions to return an array."]

funExpectedErr      :: Position -> CT s a
funExpectedErr pos  =
  raiseErrorCTExc pos 
    ["Function expected!",
     "A function is needed here, but this declarator does not declare",
     "a function."]

enumExpectedErr     :: Ident -> CT s a
enumExpectedErr ide  =
  raiseErrorCTExc (posOf ide) 
    ["Expected enum!",
     "Expected `" ++ identToLexeme ide ++ "' to denote an enum; instead found",
     "a struct, union, or object."]

structExpectedErr     :: Position -> CT s a
structExpectedErr pos  =
  raiseErrorCTExc pos
    ["Expected a struct!",
     "Expected a structure or union; instead found an enum or basic type."]

enumForwardErr     :: Position -> CT s a
enumForwardErr pos  =
  raiseErrorCTExc pos 
    ["Forward definition of enumeration!",
     "ANSI C does not permit foreward definitions of enumerations!"]
