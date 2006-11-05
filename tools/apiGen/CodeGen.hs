module CodeGen (
  genModuleBody,
  genTodoItems,
  makeKnownSymbolsMap
  ) where

import Module       (Module(..), Decl(..), DeclBody(..), isAttr)
import qualified Api
import Docs         (ParamDoc(..), DocParaSpan(..))
import FormatDocs   (genModuleDocumentation, haddocFormatDescription, 
                     haddocFormatDeclaration, cParamNameToHsName,
                     cFuncNameToHsName, changeIllegalNames, mungeWord,
                     haddocFormatSpan)
import Marshal      (CSymbol(..), ParameterKind(..), EnumKind(..),
                     KnownSymbols, genMarshalParameter, genMarshalResult,
                     genMarshalOutParameter, genCall, genMarshalProperty,
                     convertSignalType)
import StringUtils hiding (comment)
import Utils hiding (cat)
import MarshalFixup (maybeNullParameter, maybeNullResult, leafClass,
                     nukeParameterDocumentation)

import Prelude hiding (Enum, lines)
import Data.List    (groupBy, sortBy, partition)
import Data.Maybe   (fromMaybe, catMaybes)
import qualified Data.Map as Map
import Data.Version

import Debug.Trace (trace)

-------------------------------------------------------------------------------
-- Now lets actually generate some code fragments based on the api info
-------------------------------------------------------------------------------

genDecl :: KnownSymbols -> Decl -> Doc
genDecl knownSymbols decl =
     formattedDoc
  $$ text (genDeclCode knownSymbols decl "")
  $$ deprecatedNote

  where
    formattedDoc =
      case decl_doc decl of
        Nothing  -> empty
        Just doc -> haddocFormatDeclaration knownSymbols docNullsAllFixed doc
    docNullsAllFixed =
      case decl_body decl of
        Method {
          method_cname = cname,
          method_parameters = params
        } -> maybeNullResult cname
          || or [ maybeNullParameter cname (cParamNameToHsName pname)
                | Api.Parameter { Api.parameter_name = pname } <- params ]
        _ -> False                                
    deprecatedNote
      | decl_deprecated decl
                  = deprecated (text $ decl_name decl)
                               (text $ decl_deprecated_comment decl)
      | otherwise = empty

deprecated name comment =
  pragma $ text "DEPRECATED" <+> name <+> doubleQuotes comment

pragma d = text "{-#" <+> d <+> text "#-}"
c2hsHook name d = text "{#" <+> text name <+> d <+> text "#}"

genDeclCode :: KnownSymbols -> Decl -> ShowS
genDeclCode knownSymbols Decl{ decl_body = method@(Method {}) } =
  ss functionName. ss " :: ". functionType. nl.
  ss functionName. sc ' '. formattedParamNames. sc '='.
  indent 1. codebody

  where functionName = cFuncNameToHsName (method_cname method)
	(classConstraints', paramTypes', paramMarshalers) =
	  unzip3 [ case genMarshalParameter knownSymbols (method_cname method)
                          (changeIllegalNames (cParamNameToHsName (Api.parameter_name p)))
	                  (Api.parameter_type p) of
                     (c, ty, m) -> (c, (ty, Api.parameter_name p), m)
		 | p <- method_parameters method ]
	classConstraints = [ c | Just c <- classConstraints' ]
	inParamTypes = [ (paramType, lookup name paramDocMap)
                     | (InParam paramType, name) <- paramTypes' ]
	inParamNames = [ changeIllegalNames (cParamNameToHsName (Api.parameter_name p))
		     | ((InParam _, _), p) <- zip paramTypes' (method_parameters method) ]
	outParamTypes = [ (paramType, lookup name paramDocMap)
                        | (OutParam paramType, name) <- paramTypes' ]
        formattedParamNames = cat (map (\name -> ss name.sc ' ') inParamNames)
	(returnType', returnMarshaler) =
		genMarshalResult knownSymbols (method_cname method)
                                  (method_is_constructor method) (method_return_type method)
        returnType | null outParamTypes  = ("IO " ++ returnType', lookup "Returns" paramDocMap)
		   | otherwise = case unzip outParamTypes of
                                   (types', docs') ->
				     let types | returnType' == "()" = types'
				               | otherwise           = returnType' : types'
					 docs = mergeParamDocs (lookup "Returns" paramDocMap) docs'
				      in (case types of
				            [t] -> "IO " ++ t
					    _   -> "IO (" ++ sepBy ", " types "" ++ ")"
					 ,docs)
	(outParamMarshalersBefore, outParamMarshalersAfter, returnOutParamFragments) =
             unzip3 [ genMarshalOutParameter outParamType (changeIllegalNames (cParamNameToHsName name))
                    | (OutParam outParamType, name) <- paramTypes' ]
        returnOutParams body | null outParamTypes = body
                             | otherwise = body
                                         . indent 1. ss "return (". sepBy' ", " returnOutParamFragments. ss ")"
        functionType = (case classConstraints of
	                  []  -> id
			  [c] -> ss c. ss " => "
			  _   -> sc '('. sepBy ", " classConstraints. ss ") => ").
                       formatParamTypes (inParamTypes ++ [returnType])
	codebody = foldl (\body marshaler -> marshaler body)
                     call (paramMarshalers
                       ++ [ (\body -> frag. body) | frag <- reverse outParamMarshalersBefore ]
                       ++ [ (\body -> body. frag) | frag <- outParamMarshalersAfter ]
                       ++ [returnMarshaler,returnOutParams])
	call = ss (genCall (fromMaybe (method_cname method) (method_shortcname method))
                           (method_is_unsafe_ffi method))
        docNullsAllFixed = maybeNullResult (method_cname method)
                        || or [ maybeNullParameter (method_cname method) (cParamNameToHsName (Api.parameter_name p))
                              | p <- method_parameters method ]
        paramDocMap = [ (paramdoc_name paramdoc
                        ,(if paramdoc_name paramdoc == "Returns"
                           then [DocText "returns "]
                           else [DocArg (paramdoc_name paramdoc)
                                ,DocText " - "]
                         ) ++ paramdoc_paragraph paramdoc)
                      | paramdoc <- method_param_docs method
		      , not $ nukeParameterDocumentation
                                (method_cname method)
                                (cParamNameToHsName (paramdoc_name paramdoc)) ]
        
        formatParamTypes :: [(String, Maybe [DocParaSpan])] -> ShowS
        formatParamTypes paramTypes = format True False paramTypes
                                             -- True to indicate first elem
                                             -- False to mean previous param had no doc
          where format _    _ []                   = id
                format True _ ((t,Nothing)    :ts) =               ss t.
                                                     format False False ts
                format True _ ((t,Just doc)   :ts) = ss "\n    ". ss t.
                                                     ss (replicate (columnIndent - length t) ' ').
                                                     ss " -- ^ ". formatDoc doc.
                                                     format False True  ts
                format _ True  ((t, Nothing)  :ts) = ss "\n -> ". ss t.
                                                     format False False ts
                format _ False ((t, Nothing)  :ts) = ss   " -> ". ss t.
                                                     format False False ts
                format _ _     ((t, Just doc) :ts) = ss "\n -> ". ss t.
                                                     ss (replicate (columnIndent - length t) ' ').
                                                     ss " -- ^ ". formatDoc doc.
                                                     format False True  ts
                formatDoc :: [DocParaSpan] -> ShowS
                formatDoc =
                    sepBy' ("\n" ++ replicate (columnIndent+5) ' ' ++  "-- ")
                  . map (sepBy " ")
                  . wrapText 3 (80 - columnIndent - 8)
                  . map (mungeWord knownSymbols docNullsAllFixed)
                  . words
                  . concatMap (haddocFormatSpan knownSymbols docNullsAllFixed)
                columnIndent = maximum [ length parmType | (parmType, _) <- paramTypes ]

genDeclCode knownSymbols decl@(Decl{ decl_body = attr@(AttributeProp { attribute_is_child = False }) }) =
  genAtter decl propertyName classConstraint getterType setterType (Right body)
  where propertyName = decl_name decl
        (propertyType, gvalueKind) = genMarshalProperty knownSymbols (attribute_type attr)
        body = ss attrType. ss "AttrFrom". ss gvalueKind. ss "Property \"". ss (attribute_cname attr). ss "\""
          where attrType | attribute_readable attr
                        && attribute_writeable attr = "new"
                         | attribute_readable  attr = "read"
                         | attribute_writeable attr = "write"
        getterType | attribute_readable attr  = Just propertyType 
                   | otherwise                = Nothing
        (setterType, classConstraint)
                   | attribute_writeable attr 
                  && gvalueKind == "Object"  =
                    if leafClass (attribute_type attr)
                      then (Just propertyType, Nothing)
                      else let typeVar = lowerCaseFirstChar propertyType
                               classConstraint' = propertyType ++ "Class " ++ typeVar
                            in (Just typeVar, Just classConstraint')
                   | attribute_writeable attr = (Just propertyType, Nothing)
                   | otherwise                = (Nothing, Nothing)


genDeclCode knownSymbols decl@(Decl{ decl_body = attr@(AttributeProp { attribute_is_child = True }) }) =
  genChildAtter decl propertyName classConstraint getterType setterType (Right body)
  where propertyName = decl_name decl
        (propertyType, gvalueKind) = genMarshalProperty knownSymbols (attribute_type attr)
        body = ss attrType. ss "AttrFromContainerChild". ss gvalueKind. ss "Property \"". ss (attribute_cname attr). ss "\""
          where attrType | attribute_readable attr
                        && attribute_writeable attr = "new"
                         | attribute_readable  attr = "read"
                         | attribute_writeable attr = "write"
        getterType | attribute_readable attr  = Just propertyType 
                   | otherwise                = Nothing
        (setterType, classConstraint)
                   | attribute_writeable attr 
                  && gvalueKind == "Object"   =
                    if leafClass (attribute_type attr)
                      then (Just propertyType, Nothing)
                      else let typeVar = lowerCaseFirstChar propertyType
                               classConstraint' = propertyType ++ "Class " ++ typeVar ++ ", WidgetClass child"
                            in (Just typeVar, Just classConstraint')
                   | attribute_writeable attr = (Just propertyType, Just "WidgetClass child")
                   | otherwise                = (Nothing, Just "WidgetClass child")

genDeclCode knownSymbols decl@(Decl{ decl_body = attr@(AttributeGetSet {}) }) =
  genAtter decl propertyName classConstraint
           (Just getterType) (Just setterType)
           (Left (ss (decl_name getter), ss (decl_name setter)))
  where propertyName = decl_name decl
        (getterType, _) = genMarshalResult knownSymbols (method_cname getter_body) False
                              (method_return_type getter_body)
        (classConstraint, setterType) =
          case let param = method_parameters setter_body !! 1
                   paramName = changeIllegalNames (cParamNameToHsName (Api.parameter_name param))
                   paramType = Api.parameter_type param
                in genMarshalParameter knownSymbols (method_cname setter_body) paramName paramType of
            (classConstraint', InParam setterType', _) -> (classConstraint', setterType')
            (_, OutParam _, _)  -> (Nothing, "{- FIXME: should be in param -}")
        getter@Decl { decl_body = getter_body } = attribute_getter attr
        setter@Decl { decl_body = setter_body } = attribute_setter attr

genDeclCode knownSymbols Decl{ decl_module = module_,
                               decl_name = signalName,
                               decl_body = signal@Module.Signal {} }
  | signal_is_old_style signal =
          ss signalName. ss " :: ". oldSignalType.
          ss signalName. ss " = connect_". connectCall. sc ' '. signalCName. sc ' '. shows (signal_is_after signal)

  | otherwise =
      ss (lowerCaseFirstChar signalName). ss " :: ". signalType. nl.
      ss (lowerCaseFirstChar signalName). ss " = Signal (connect_". connectCall. sc ' '. signalCName. sc ')'

  where connectCall = let paramCategories' = if null paramCategories then ["NONE"] else paramCategories
                       in sepBy "_" paramCategories' . ss "__" . ss returnCategory
        -- strip off the object arg to the signal handler
        params = case Module.signal_parameters signal of
                   (param:params') | Api.parameter_type param
                                  == (module_cname module_) ++ "*" -> params'
                   params' -> params'
        (paramCategories, paramTypes) = unzip [ convertSignalType knownSymbols (Api.parameter_type parameter)
                                              | parameter <- params ]
        (returnCategory, returnType) = convertSignalType knownSymbols (Module.signal_return_type signal)
        signalType = ss (module_name module_). ss "Class self => Signal self (". callbackType. sc ')'
        oldSignalType = ss (module_name module_). ss "Class self => self\n".
                     ss " -> ". callbackType.
                     ss "\n -> IO (ConnectId self)\n"
        callbackType | null paramTypes = ss "IO ". ss returnType
                     | otherwise = sc '('. sepBy " -> " (paramTypes ++ ["IO " ++ returnType]). sc ')'
        signalCName = sc '"'. ss (Module.signal_cname signal). sc '"'

genDeclCode _
  Decl { decl_body = Instance { instance_class_name = className,
                                instance_type_name  = typeName }} =
  ss "instance ".ss className. sc ' '. ss typeName


mergeParamDocs :: Maybe [DocParaSpan] -> [Maybe [DocParaSpan]] -> Maybe [DocParaSpan]
mergeParamDocs doc docs =
  case catMaybes (doc:docs) of
    [] -> Nothing
    [doc'] -> Just doc'
    docs' -> let (varNames, paramDocs) =
                   unzip [ case doc' of 
                            (DocArg varName : _) -> (cParamNameToHsName varName, doc')
                            _                    -> ("_", doc')
                         | doc' <- docs' ]
                 returnValName = DocLiteral ("(" ++ sepBy ", " varNames "" ++ ")")
                 fixmeMessage  = DocText " {FIXME: merge return value docs} "
              in Just $ returnValName : fixmeMessage : concat paramDocs

genModuleBody :: KnownSymbols -> Module -> Doc
genModuleBody knownTypes module_ =
     summary
  $$ comment
  $$ (if Module.module_deprecated module_
        then text "module" <+> moduleName
          $$ deprecatedNote <+> lparen
        else text "module" <+> moduleName <+> lparen)
 $+$ documentation
 $+$ exports
  $$ nest 2 (rparen <+> text "where")
 $+$ imports
 $+$ context
 $+$ decls
  
  where summary = haddocFormatDescription knownTypes
                    (module_summary module_)

        moduleName | isEmpty prefix = name
                   | otherwise      = prefix <> char '.' <> name
          where name = text (Module.module_name module_)
                prefix = text (Module.module_prefix module_)

        deprecatedNote | Module.module_deprecated module_ =
          deprecated empty (text "this module should not be used in newly-written code.")
                       | otherwise = empty

        documentation = genModuleDocumentation knownTypes
                          (module_cname module_) (module_description module_)
                          (module_sections module_) (module_hierarchy module_)

        exports = genExports module_
        imports = genImports module_

        context = c2hsHook "context" $
                    text "lib" <> equals <> doubleQuotes (text $ module_context_lib module_)
                <+> text "prefix" <> equals <> doubleQuotes (text $ module_context_prefix module_)

        decls = genDecls knownTypes module_

genDecls :: KnownSymbols -> Module -> Doc
genDecls knownSymbols module_ =
    doVersionIfDefs vsep
  . map adjustDeprecatedAndSinceVersion
  $  sectionHeader "Interfaces"
     [ (genDecl knownSymbols decl, (since, deprecated))
     | decl@Decl { decl_since = since,
                   decl_deprecated = deprecated,
                   decl_body = Instance { }
       } <- module_decls module_ ]
  ++ sectionHeader "Constructors"
     [ (genDecl knownSymbols decl, (since, deprecated))
     | decl@Decl { decl_since = since,
                   decl_deprecated = deprecated,
                   decl_body = Method { method_is_constructor = True }
       } <- module_decls module_ ]
  ++ sectionHeader "Methods"
     [ (genDecl knownSymbols decl, (since, deprecated))
     | decl@Decl { decl_since = since,
                   decl_deprecated = deprecated,
                   decl_body = Method { method_is_constructor = False }
       } <- module_decls module_ ]
  ++ sectionHeader "Attributes"
     [ (genDecl knownSymbols decl, (since, deprecated))
     | decl@Decl { decl_since = since,
                   decl_deprecated = deprecated
       } <- module_decls module_, isAttr decl ]
  ++ sectionHeader "Child Attributes"
     [ (genDecl knownSymbols decl, (since, deprecated))
     | decl@Decl { decl_since = since,
                   decl_deprecated = deprecated,
                   decl_body = AttributeProp { attribute_is_child = True }
       } <- module_decls module_ ]
  ++ sectionHeader "Signals"
     [ (genDecl knownSymbols decl, (since, deprecated))
     | decl@Decl { decl_since = since,
                   decl_deprecated = deprecated,
                   decl_body = Module.Signal {}
       } <- module_decls module_ ]
  where sectionHeader _    []      = []
        sectionHeader name entries =
          let header = (text "--------------------"
                     $$ comment <+> text name, (Nothing, False))
           in header : entries
        adjustDeprecatedAndSinceVersion (doc, (since, deprecated)) =
          (doc, (module_since module_ `max` since, Module.module_deprecated module_ || deprecated))
        

genAtter :: Decl -> String
         -> Maybe String -> Maybe String -> Maybe String
         -> Either (ShowS, ShowS) ShowS -> ShowS
genAtter Decl { decl_module = module_ }
         propertyName classConstraint getterType setterType attrImpl =
  ss propertyName. ss " :: ". classContext. attrType. sc ' '. objectParamType. sc ' '. attrArgs. nl.
  ss propertyName. ss " = ". attrBody
  where objectType = ss (module_name module_)
        objectParamType | leafClass (module_cname module_) = objectType
                        | otherwise                        = ss "self"
        classContext = case (leafClass (module_cname module_), classConstraint) of 
                         (True,  Nothing)              -> id
                         (False, Nothing)              -> objectType. ss "Class self => "
                         (True,  Just classConstraint') -> ss classConstraint'. ss " => "
                         (False, Just classConstraint') -> sc '('. objectType. ss "Class self, ".
                                                           ss classConstraint'. ss ") => "
        (attrType, attrConstructor, attrArgs) =
          case (getterType, setterType) of
            (Just gt, Nothing)        -> (ss "ReadAttr",     ss "readAttr", ss gt)
            (Nothing, Just st)        -> (ss "WriteAttr",    ss "writeAttr", ss st)
            (Just gt, Just st)
              | gt == st              -> (ss "Attr",          ss "newAttr", ss gt)
              | length (words st) > 1 -> (ss "ReadWriteAttr", ss "newAttr", ss gt. ss " (". ss st. sc ')')
              | otherwise             -> (ss "ReadWriteAttr", ss "newAttr", ss gt. sc ' '. ss st)
	    _ -> error $ "no getter or setter for " ++ module_name module_ ++ " :: " ++ propertyName 
        attrBody =
          case (attrImpl) of
            Left (getter, setter) -> attrConstructor.
              case (getterType, setterType) of
                (Just _, Nothing) -> indent 1. getter
                (Nothing, Just _) -> indent 1. setter
                (Just _,  Just _) -> indent 1. getter. indent 1. setter
            Right body            -> body


genChildAtter :: Decl -> String
 -> Maybe String -> Maybe String -> Maybe String -> Either (ShowS, ShowS) ShowS -> ShowS
genChildAtter Decl { decl_module = module_ }
              propertyName classConstraint getterType setterType attrImpl =
  ss propertyName. ss " :: ". classContext. ss "child -> ". attrType. sc ' '. objectParamType. sc ' '. attrArgs. nl.
  ss propertyName. ss " = ". attrBody
  where objectType = ss (module_name module_)
        objectParamType | leafClass (module_cname module_) = objectType
                        | otherwise                        = ss "self"
        classContext = case (leafClass (module_cname module_), classConstraint) of 
                         (True,  Nothing)              -> id
                         (False, Nothing)              -> objectType. ss "Class self => "
                         (True,  Just classConstraint') -> ss classConstraint'. ss " => "
                         (False, Just classConstraint') -> sc '('. objectType. ss "Class self, ".
                                                           ss classConstraint'. ss ") => "
        (attrType, attrConstructor, attrArgs) =
          case (getterType, setterType) of
            (Just gt, Nothing)        -> (ss "ReadAttr",     ss "readAttr", ss gt)
            (Nothing, Just st)        -> (ss "WriteAttr",    ss "writeAttr", ss st)
            (Just gt, Just st)
              | gt == st              -> (ss "Attr",          ss "newAttr", ss gt)
              | length (words st) > 1 -> (ss "ReadWriteAttr", ss "newAttr", ss gt. ss " (". ss st. sc ')')
              | otherwise             -> (ss "ReadWriteAttr", ss "newAttr", ss gt. sc ' '. ss st)
        attrBody =
          case (attrImpl) of
            Left (getter, setter) -> attrConstructor.
              case (getterType, setterType) of
                (Just _, Nothing) -> indent 1. getter
                (Nothing, Just _) -> indent 1. setter
                (Just _,  Just _) -> indent 1. getter. indent 1. setter
            Right body            -> body

makeKnownSymbolsMap :: Api.API -> KnownSymbols
makeKnownSymbolsMap api =
   (Map.fromList
  . reverse
  . concat)
  [ [ (Api.enum_cname enum
      ,case Api.enum_variety enum of
        Api.EnumVariety -> SymEnumType EnumKind
        Api.FlagsVariety -> SymEnumType FlagsKind)
    | enum <- Api.namespace_enums namespace ]
 ++ [ (Api.object_cname object, objectKind object)
    | object <- Api.namespace_objects namespace ]
 ++ [ (Api.class_cname class_, SymClassType)
    | class_ <- Api.namespace_classes namespace ]
 ++ [ (Api.boxed_cname boxed, SymBoxedType)
    | boxed <- Api.namespace_boxed namespace ]
 ++ [ (Api.member_cname member, SymEnumValue)
    | enum <- Api.namespace_enums namespace
    , member <- Api.enum_members enum ]
 ++ [ (Api.misc_cname misc, miscToCSymbol misc )
    | misc <- Api.namespace_misc namespace ]
  | namespace <- api ]

        -- find if an object inherits via GtkObject or directly from GObject
  where objectKind :: Api.Object -> CSymbol
        objectKind object | "GObject" `elem` parents = SymObjectType parents
                          -- FIXME: These hacks should go elsewhere
                          | Api.object_cname object == "GtkClipboard" = SymObjectType ["GtkClipboard", "GObject"]
                          | Api.object_cname object == "GParamSpec" = SymStructType
                          | Api.object_cname object == "GdkBitmap" = SymStructType
                          | otherwise = trace ("Warning: non-GObject "
                                            ++ Api.object_cname object) SymStructType
          where parents = objectParents object
        objectParents :: Api.Object -> [String]
        objectParents object = Api.object_cname object :
          case Api.object_parent object `lookup` objectMap of
            Nothing -> [Api.object_parent object]
            Just parent -> objectParents parent
        objectMap :: [(String, Api.Object)]
        objectMap = [ (Api.object_cname object, object)
                    | namespace <- api
                    , object <- Api.namespace_objects namespace ]
        miscToCSymbol (Api.Struct   _ _) = SymStructType
        miscToCSymbol (Api.Alias    _ _) = SymTypeAlias
        miscToCSymbol (Api.Callback _ _) = SymCallbackType

genExports :: Module -> Doc
genExports module_ =
    doVersionIfDefs vcat
  . map adjustDeprecatedAndSinceVersion
  $  sectionHeader False "Types"
     [(text (Module.module_name module_), defaultAttrs)
     ,(text (Module.module_name module_) <> text "Class", defaultAttrs)
     ,(text "castTo" <> text (Module.module_name module_), defaultAttrs)
     ,(text "to"     <> text (Module.module_name module_), defaultAttrs)]
  ++ sectionHeader True "Constructors"
     [ (text name, (since, deprecated))
     | Decl { decl_name = name,
              decl_since = since,
              decl_deprecated = deprecated,
              decl_body = Method { method_is_constructor = True }
       } <- exports ]
  ++ sectionHeader True "Methods"
     [ (text name, (since, deprecated))
     | Decl { decl_name = name,
              decl_since = since,
              decl_deprecated = deprecated,
              decl_body = Method { method_is_constructor = False }
       } <- exports ]
  ++ sectionHeader True "Attributes"
     [ (text name, (since, deprecated))
     | decl@Decl { decl_since = since,
                   decl_deprecated = deprecated,
                   decl_name = name 
       } <- module_decls module_
       , isAttr decl ]
  ++ sectionHeader True "Child Attributes"
     [ (text name, (since, deprecated))
     | Decl { decl_name = name,
              decl_since = since,
              decl_deprecated = deprecated,
              decl_body = AttributeProp { attribute_is_child = True }
       } <- module_decls module_ ]
  ++ sectionHeader True "Signals"
     [ (text name, (since, deprecated))
     | Decl { decl_name = name,
              decl_since = since,
              decl_deprecated = deprecated,
              decl_body = Module.Signal { signal_is_old_style = False }
       } <- exports ]
  ++ sectionHeader True "Deprecated"
     [ (text name, (since, deprecated))
     | Decl { decl_name = name,
              decl_since = since,
              decl_deprecated = deprecated,
              decl_body = Module.Signal { signal_is_old_style = True }
       } <- exports ]

  where defaultAttrs = (Nothing, False)
        sectionHeader _ _    []      = []
        sectionHeader False name entries = (text "-- * " <> text name, defaultAttrs)
                                   : map (\(doc, attrs) -> (nest 2 (doc <> comma), attrs)) entries
        sectionHeader True name entries = (emptyLine, defaultAttrs)
                                   : (text "-- * " <> text name, defaultAttrs)
                                   : map (\(doc, attrs) -> (nest 2 (doc <> comma), attrs)) entries
        adjustDeprecatedAndSinceVersion (doc, (since, deprecated)) =
          (doc, (Module.module_since module_ `max` since, Module.module_deprecated module_ || deprecated))
        exports = sortBy (comparing decl_index_export) (module_decls module_)

genImports :: Module -> Doc
genImports module_ = 
  (case [ text importLine
        | (_, importLine) <- stdModules ] of
     []   -> empty
     mods -> vcat mods)
  $+$ vcat [ text importLine
           | (_, importLine) <- extraModules ]
  where (stdModules, extraModules)
          | null (Module.module_imports module_) =
             ([(undefined, "import Monad\t(liftM)")]
             ,[(undefined, "import System.Glib.FFI")
             ,(undefined, "{#import Graphics.UI.Gtk.Types#}")
             ,(undefined, "-- CHECKME: extra imports may be required")])
          | otherwise = partition (\(m, _) -> m `elem` knownStdModules)
                                  (Module.module_imports module_)
        knownStdModules = ["Maybe", "Monad", "Char", "List", "Data.IORef"]

genTodoItems :: Module -> Doc
genTodoItems Module { module_todos = varargsFunctions } 
  | null varargsFunctions = empty
  | otherwise =
       comment <+> text "TODO: the following varargs functions were not bound"
    $$ (commentBlock . map ((text "  " <>) . text)) varargsFunctions
    $$ comment

type Deprecated = Bool
type Since = Maybe Version

doVersionIfDefs :: ([Doc] -> Doc) -> [(Doc, (Since, Deprecated))] -> Doc
doVersionIfDefs sep =
    sep
  . layoutChunks True empty
  . makeChunks [Nothing] False
  . map (\group@((_,(since, deprecated)):_) -> (map fst group, since, deprecated))
  . groupBy (equating snd)

  where makeChunks :: [Since] -> Deprecated -> [([Doc], Since, Deprecated)] -> [Chunk]
        makeChunks    sinceStack  True [] = EndChunk : makeChunks sinceStack False []
        makeChunks (_:[])         _    [] = []
        makeChunks (_:sinceStack) _    [] = EndChunk : makeChunks sinceStack False []
        makeChunks sinceStack@(sinceContext:_) prevDeprecated whole@((group, since, deprecated):rest)
          | deprecated < prevDeprecated = EndChunk              : makeChunks sinceStack deprecated whole
          | since < sinceContext        = EndChunk              : makeChunks (tail sinceStack)  prevDeprecated whole
          | deprecated > prevDeprecated = BeginDeprecatedChunk  : makeChunks sinceStack deprecated whole
          | since > sinceContext        = BeginSinceChunk since : makeChunks (since:sinceStack) prevDeprecated whole
          | otherwise                   = SimpleChunk group     : makeChunks sinceStack prevDeprecated rest
        
        layoutChunks :: Bool -> Doc -> [Chunk] -> [Doc]
        layoutChunks _     doc  []                             = doc : []
        layoutChunks _     doc (EndChunk              :chunks) =       layoutChunks False (doc $$ endif) chunks
        layoutChunks False doc (SimpleChunk group     :chunks) = doc : layoutChunks False (sep group) chunks
        layoutChunks True  doc (SimpleChunk group     :chunks) =       layoutChunks False (doc $$ sep group) chunks
        layoutChunks _     doc (BeginDeprecatedChunk  :chunks) = doc : layoutChunks True ifndefDeprecated chunks
        layoutChunks _     doc (BeginSinceChunk since :chunks) = doc : layoutChunks True (ifSinceVersion since) chunks
        
        ifSinceVersion (Just Version { versionBranch = [major,minor] }) =
          text "#if GTK_CHECK_VERSION(" <> int major <> comma <> int minor <> text ",0)"
        ifndefDeprecated = text "#ifndef DISABLE_DEPRECATED"
        endif = text "#endif"

data Chunk = SimpleChunk [Doc]
           | BeginDeprecatedChunk
           | BeginSinceChunk Since
           | EndChunk
