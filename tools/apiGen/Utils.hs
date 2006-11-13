module Utils (
  
  -- pretty printing
  module Text.PrettyPrint,
  ($$), ($+$),
  emptyLine,
  vsep,
  comment,
  commentBlock,
  
  -- string things
  lowerCaseFirstChar,
  upperCaseFirstChar,
  splitBy,
  wrapText,
  templateSubstitute,
  
  -- generic things
  equating,
  comparing,
  mergeBy,
  
  ) where

import Data.Char (toLower, toUpper)
import Data.List (unfoldr)
import Text.PrettyPrint hiding (($$), ($+$))
import qualified Text.PrettyPrint (($+$))

-------------------------------------------------------------------------------
-- Fix Text.PrettyPrint to mean the right thing
-------------------------------------------------------------------------------

infixl 5 $$, $+$

($$) = (Text.PrettyPrint.$+$)
d1 $+$ d2 | isEmpty d1 = d2
          | isEmpty d2 = d1
          | otherwise  = d1
                      $$ emptyLine
                      $$ d2

-- some useful extensions
vsep = foldr ($+$) empty
emptyLine = text ""
comment = text "--"
commentBlock = vcat . map (comment <+>)

-------------------------------------------------------------------------------
-- Stringy things
-------------------------------------------------------------------------------

lowerCaseFirstChar :: String -> String
lowerCaseFirstChar (c:cs) = toLower c : cs

upperCaseFirstChar :: String -> String
upperCaseFirstChar (c:cs) = toUpper c : cs

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy sep xs = split xs
  where split xs = case break (==sep) xs of
          (chunk,[])     -> chunk : []
          (chunk,_:rest) -> chunk : split rest

-- wraps a list of words to lines of words
wrapText :: Int -> Int -> [String] -> [[String]]
wrapText initialCol width = wrap initialCol []
  
  where wrap :: Int -> [String] -> [String] -> [[String]]
        wrap 0   []   (word:words)
          | length word + 1 > width
          = wrap (length word) [word] words

        wrap col line (word:words)
          | col + length word + 1 > width
          = reverse line : wrap 0 [] (word:words)

        wrap col line (word:words)
          = let col' = col + length word + 1
             in wrap col' (word:line) words

        wrap _ []   [] = []
        wrap _ line [] = [reverse line]

templateSubstitute :: String -> (String -> Doc) -> Doc
templateSubstitute template varSubst = vcat . map substLine . lines $ template
  where substLine ('$':var) = varSubst (init var)
        substLine line =
          case span (/= '<') line of
            (chunk, []) -> text chunk
            (chunk, '<':rest) ->
              case span (/= '>') rest of
                (var, '>':rest) -> text chunk <> varSubst var <> substLine rest

-------------------------------------------------------------------------------
-- Totally generic things
-------------------------------------------------------------------------------

equating :: Eq a => (b -> a) -> b -> b -> Bool
equating p x y = p x == p y

comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)


-- mergeBy cmp xs ys = (only_in_xs, in_both, only_in_ys)
mergeBy :: (a -> b -> Ordering) -> [a] -> [b] -> ([a], [(a, b)], [b])
mergeBy cmp = merge [] [] []
  where merge l m r []     ys     = (reverse l, reverse m, reverse (ys++r))
        merge l m r xs     []     = (reverse (xs++l), reverse m, reverse r)
        merge l m r (x:xs) (y:ys) = 
          case x `cmp` y of
            GT -> merge    l         m  (y:r) (x:xs)    ys
            EQ -> merge    l  ((x,y):m)    r     xs     ys
            LT -> merge (x:l)        m     r     xs  (y:ys)
