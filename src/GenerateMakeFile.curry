--------------------------------------------------------------------------
--- A tool to generate a makefile for a Curry application.
---
--- @author Michael Hanus
--- @version December 2020
--------------------------------------------------------------------------

module GenerateMakeFile where

import Control.Monad               ( when )
import Curry.Compiler.Distribution ( installDir )
import Data.List          ( intercalate, isPrefixOf, union, sort )
import System.Environment ( getEnv )

import FlatCurry.Types    ( Prog(..) )
import FlatCurry.Read     ( readFlatCurryIntWithImports )
import System.CurryPath   ( lookupModuleSourceInLoadPath )
import System.Directory   ( doesFileExist, getCurrentDirectory, renameFile )
import System.FilePath    ( (</>), searchPathSeparator, splitSearchPath )

import MakeFile

-- Create a makefile for a given main module:
generateMakeForApplication :: Int -> String -> String -> String -> String
                           -> String -> IO ()
generateMakeForApplication verb args mainmod target root tool = do
  makefile <- generateMakeFile args root tool mainmod
  let makefiletext = showMakeFile makefile
  when (null target || verb>1) $ putStr makefiletext
  when (not (null target)) $ do
    texists <- doesFileExist target
    when texists $ do
      let tbak = target ++ ".bak"
      renameFile target tbak
      putStrLn $ "Existing makefile saved to `" ++ tbak ++ "'"
    writeFile target makefiletext
    when (verb>0) $ putStrLn $ "Makefile written to `" ++ target ++ "'"

-- Generate a makefile for a given main module:
generateMakeFile :: String -> String -> String -> String -> IO MakeFile
generateMakeFile args root tool mainmod = do
  allints <- readFlatCurryIntWithImports mainmod
  let allmods = (foldl union [mainmod]
                       (map (\ (Prog _ imps _ _ _) -> imps) allints))
  allsources <- mapM findSourceFileInLoadPath (filter (/="Prelude") allmods)
  currypath  <- getEnv "CURRYPATH"
  curdir     <- getCurrentDirectory
  let simpcurrypath = if null currypath
                        then ""
                        else intercalate [searchPathSeparator]
                                         (map (simplifyPath curdir)
                                              (splitSearchPath currypath))
  return $ modToMakeFile args root tool mainmod
                         (sort (map (simplifyPath curdir) allsources))
                         simpcurrypath

-- Translate a module with its dependent source files and a (possibly empty)
-- load path into a makefile:
modToMakeFile :: String -> String -> String -> String -> [String] -> String
              -> MakeFile
modToMakeFile args root tool mainmod sourcefiles currypath =
  [ Comment $ "Makefile for main module \""++mainmod++"\""
  , Comment $ "Created by: curry-genmake " ++ args
  , Empty
  , Comment "The root directory of the Curry system:"
  , DefineVariable "CURRYHOME" [root]
  , Empty
  , Comment "The executable of the Curry system:"
  , DefineVariable "REPL"  ["$(CURRYHOME)/bin/curry"]
  , Empty
  , Comment "Default options for the Curry system:"
  , DefineVariable "REPL_OPTS"  [":set -time"]
  , Empty
  , Comment "The directory of the Curry system libraries:"
  , DefineVariable "CURRYLIB"  ["$(CURRYHOME)/lib"]
  , Empty ] ++
  ifNotNull tool
    [ Comment "The tool name of the application:"
    , DefineVariable "TOOL" [tool], Empty] ++
  ifNotNull currypath
    [ Comment "The load path of the application:"
    , DefineVariable "LOADPATH" [currypath], Empty] ++
  [ Comment "Source modules:"
  , DefineVariable "DEPS" sourcefiles
  , Empty
  , Rule [PHONY] "all" ["install"] []
  , Rule [PHONY] "install" ["compile"]
     (ifNotNull tool
       [ "mkdir -p $(dir $(TOOL))", "rm -f $(TOOL)"
       , "cd $(dir $(TOOL)) && ln -s $(CURDIR)/"++mainmod++" $(notdir $(TOOL))"
       , "@echo Tool installed into: $(TOOL)"])
  , Rule [PHONY] "compile" [mainmod] []
  , Comment "Load the application into the interactive Curry system:"
  , Rule [PHONY] "load" []
         ["$(REPL) $(REPL_OPTS) " ++ setpath ++ ":l "++mainmod]
  , Comment "Compile and create an executable of the application:"
  , Rule [] mainmod ["$(DEPS)"]
         ["# create executable for top-level function \"main\":"
         ,"$(REPL) $(REPL_OPTS) " ++ setpath ++ ":l "++mainmod++" :save :q"]
  , Comment "Clean intermediate files:"
  , Rule [PHONY] "clean" [] ["$(CURRYHOME)/bin/cleancurry"]
  , Rule [PHONY] "uninstall" ["clean"]
         ["rm -f " ++ mainmod ++ if null tool then "" else " $(TOOL)"]
  ]
 where
  setpath = if null currypath then "" else ":set path $(LOADPATH) "

  ifNotNull s xs = if null s then [] else xs

-- add a directory name for a Curry source file by looking up the
-- current load path (CURRYPATH):
findSourceFileInLoadPath :: String -> IO String
findSourceFileInLoadPath modname =
  lookupModuleSourceInLoadPath modname >>=
  maybe (error ("Curry file for module \""++modname++"\" not found!"))
        (return . dropLocal . snd)
 where
  dropLocal f = if take 2 f == "./" then drop 2 f else f

-- Simplify the path a file name:
-- * replace CURRY lib directory prefix by $(CURRYLIB)
-- * strip prefix if it is identical to the given working directory
simplifyPath :: String -> String -> String
simplifyPath curdir filename
 | pakcslib `isPrefixOf` filename
 = "$(CURRYLIB)" ++ drop (length pakcslib) filename
 | curdir `isPrefixOf` filename
 = drop (length curdir + 1) filename
 | otherwise
 = filename
 where
  pakcslib = installDir </> "lib"

--------------------------------------------------------------------------
