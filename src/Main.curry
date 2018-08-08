--------------------------------------------------------------------------
--- Main module of a tool to generate a makefile for a Curry application.
---
--- @author Michael Hanus
--- @version May 2017
--------------------------------------------------------------------------

module Main where

import System.Console.GetOpt
import System.Process     ( exitWith )
import System.Environment ( getArgs )
import Distribution       ( installDir, stripCurrySuffix )
import Numeric            ( readNat )

import GenerateMakeFile

main :: IO ()
main = do
  argv <- getArgs
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldl (flip id) defaultOptions funopts
  unless (null opterrors || length args /= 1)
         (putStr (unlines opterrors) >> putStrLn usageText >> exitWith 1)
  when (optVerb opts > 0) $ putStr banner
  when (null args || optHelp opts) (putStrLn usageText >> exitWith 1)
  generateMakeForApplication (optVerb opts) (unwords argv)
    (stripCurrySuffix (head args)) (optOutput opts)
    (if null (optRoot opts) then installDir else optRoot opts)
    (optTool opts)

version :: String
version = "Version of 08/09/2018"

-- Banner of this tool:
banner :: String
banner = unlines [bannerLine,bannerText,bannerLine]
 where
  bannerText =
    "curry-genmake: Generate a makefile for a Curry application ("++version++")"
  bannerLine = take (length bannerText) (repeat '-')

-- Help text
usageText :: String
usageText =
  usageInfo "Usage: curry-genmake [options] <main module name>\n" options

--------------------------------------------------------------------------
-- Processing options:

-- Representation of command line options
data Options = Options
  { optHelp     :: Bool
  , optVerb     :: Int
  , optOutput   :: String   -- output file (or empty)
  , optRoot     :: String   -- root directory of the Curry system
  , optTool     :: String   -- name of the tool binary (or empty)
  }

-- Default command line options.
defaultOptions :: Options
defaultOptions  = Options
  { optHelp     = False
  , optVerb     = 1
  , optOutput   = ""
  , optRoot     = ""
  , optTool     = ""
  }

-- Definition of actual command line options.
options :: [OptDescr (Options -> Options)]
options =
  [ Option "h?" ["help"]  (NoArg (\opts -> opts { optHelp = True }))
           "print help and exit"
  , Option "q" ["quiet"] (NoArg (\opts -> opts { optVerb = 0 }))
           "run quietly (no output, only exit code)"
  , Option "v" ["verbosity"]
            (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
            "verbosity level:\n0: quiet (same as `-q')\n1: show progress (default)\n2: show generated output (same as `-v')"
  , Option "o" ["output"]
           (ReqArg (\n opts -> opts { optOutput = n }) "<o>")
           "name of output file (e.g., Makefile)"
  , Option "r" ["root"]
           (ReqArg (\n opts -> opts { optRoot = n }) "<r>")
           "root directory of the Curry system in Makefile"
  , Option "t" ["tool"]
           (ReqArg (\n opts -> opts { optTool = n }) "<t>")
           "name of the tool binary"
  ]
 where
  safeReadNat opttrans s opts = case readNat s of
    [(n,"")] -> opttrans n opts
    _        -> error "Illegal number argument (try `-h' for help)"

  checkVerb n opts = if n>=0 && n<3
                     then opts { optVerb = n }
                     else error "Illegal verbosity level (try `-h' for help)"


--------------------------------------------------------------------------
