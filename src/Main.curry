--------------------------------------------------------------------------
--- Main module of a tool to generate a makefile for a Curry application.
---
--- @author Michael Hanus
--- @version February 2017
--------------------------------------------------------------------------

module Main where

import Distribution    ( stripCurrySuffix )
import GetOpt
import ReadNumeric     (readNat)
import System          ( exitWith, getArgs )

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
    (stripCurrySuffix (head args)) (optOutput opts) (optTool opts)

version :: String
version = "Version of 01/03/2017"

-- Banner of this tool:
banner :: String
banner = unlines [bannerLine,bannerText,bannerLine]
 where
  bannerText =
    "curry-createmake: create makefile for a Curry application ("++version++")"
  bannerLine = take (length bannerText) (repeat '-')

-- Help text
usageText :: String
usageText =
  usageInfo "Usage: curry-createmake [options] <main module name>\n" options
  
--------------------------------------------------------------------------
-- Processing options:

-- Representation of command line options
data Options = Options
  { optHelp     :: Bool
  , optVerb     :: Int
  , optOutput   :: String   -- output file (or empty)
  , optTool     :: String   -- name of the tool binary (or empty)
  }

-- Default command line options.
defaultOptions :: Options
defaultOptions  = Options
  { optHelp     = False
  , optVerb     = 1
  , optOutput   = ""
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
  , Option "t" ["tool"]
           (ReqArg (\n opts -> opts { optTool = n }) "<t>")
           "name of the tool binary"
  ]
 where
  safeReadNat opttrans s opts =
   let numError = error "Illegal number argument (try `-h' for help)" in
    maybe numError
          (\ (n,rs) -> if null rs then opttrans n opts else numError)
          (readNat s)

  checkVerb n opts = if n>=0 && n<3
                     then opts { optVerb = n }
                     else error "Illegal verbosity level (try `-h' for help)"


--------------------------------------------------------------------------
