-------------------------------------------------------------------------
--- A library containing some data types to describe makefiles.
---
--- @author Michael Hanus
--- @version February 2017
-------------------------------------------------------------------------

module MakeFile ( MakeFile, MakeElement(..), SpecialTarget(..)
                , showMakeFile ) where

import List ( intercalate )

-------------------------------------------------------------------------

--- A make file consists of a list of make elements.
type MakeFile = [MakeElement]

--- A data type for the description of an element of a make file.
data MakeElement = DefineVariable String [String]
                 | Rule [SpecialTarget] String [String] [String]
                 | Comment String
                 | Empty

--- A data type to describe special targets.
data SpecialTarget = PHONY | NOTPARALLEL

--- Shows a `MakeFile` as a string in standard makefile syntax.
showMakeFile :: MakeFile -> String
showMakeFile = unlines . map showMakeElement

showMakeElement :: MakeElement -> String
showMakeElement Empty = ""
showMakeElement (Comment s) = "# " ++ s
showMakeElement (DefineVariable name values) =
  name ++ " = " ++ intercalate " \\\n\t  " (unwordsUpTo 70 values)
showMakeElement (Rule specials target prereqs actions) = unlines $
  map (\s -> showSpecialTarget s ++ ": " ++ target) specials ++
  [target ++ ": " ++ intercalate " \\\n\t  " (unwordsUpTo 70 prereqs) ] ++
  map ("\t"++) actions

showSpecialTarget :: SpecialTarget -> String
showSpecialTarget st = '.' : show st

--- Fill all words into lines up to the given maximal length.
unwordsUpTo :: Int -> [String] -> [String]
unwordsUpTo max words = fillWords 0 "" words
 where
  fillWords _ s [] = [s]
  fillWords n s (w:ws) =
    let nw = n + length w
    in if nw < max || n==0 -- no line break if we are at the line begin
         then fillWords (nw+1) (s ++ (if n==0 then "" else " ") ++ w) ws
         else s : fillWords 0 "" (w:ws)
