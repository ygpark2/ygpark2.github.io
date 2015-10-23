module Main (main) where

import Data.Monoid
-- import Httpd (runServer)
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Directory
import Data.Time
import Data.Time.Format (defaultTimeLocale)
import Data.Time.Format
-- import Locale
-- import Data.String.Utils (replace, join)
-- import System.Posix
import qualified Data.ByteString.Lazy as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as E

import Data.Text.Template

-- | Create 'Context' from association list.
context :: [(T.Text, T.Text)] -> Context
context assocs x = maybe err id . lookup x $ assocs
  where err = error $ "Could not find key: " ++ T.unpack x

-- Default options

defaultOptions = Options
  { optFile = "new_post"
  , optTitle = "dafault title"
  }

-- Parsing command line options

data Options = Options
  { optFile :: FilePath    -- Root directory of the new post
  , optTitle :: String   -- Post Title
  } deriving (Eq,Show)

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['f'] ["file"]
    (ReqArg (\f opts -> opts { optFile = f }) "DIR")
    "file name"
  , Option ['t'] ["title"]
    (ReqArg (\t opts -> opts { optTitle = read t }) "S")
    "post title"
  ]

postOpts :: [String] -> IO (Options, [String])
postOpts argv =
  case getOpt Permute options argv of
    (o,n,[]) -> do
      let foldf = appEndo . mconcat . (map Endo)
      return (foldf o defaultOptions, n)
    (_,_,errs) ->
      ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: httpd [OPTION...]"


replace :: String -> Char -> String -> String
replace xs c s = foldr go [] xs
   where go x acc = if x == c  then acc ++ s
                                  else acc ++ [x]

-- Main

main :: IO ()
main = do
  -- Parse command line options
  args <- getArgs
  (opts,_) <- postOpts args
  let file = optFile opts
  let title = optTitle opts

  -- By default SIGPIPE exits the program, so ignore it.
  -- installHandler sigPIPE Ignore Nothing

  -- Run the server
  putStrLn $ "Starting server on port " ++ (show title) ++ " at " ++ file

  datePath <- fmap (formatTime defaultTimeLocale "/%Y/%m/%d/") getCurrentTime
  currentPath <- getCurrentDirectory
  -- formatTime defaultTimeLocale "The date is %A (%a) %d/%m/%Y" getCurrentTime

  putStrLn datePath

  putStrLn (currentPath ++ datePath)

  -- getAppDataDirectory >>= print
  -- getAppUserDataDirectory >>= print
  getHomeDirectory >>= print
  getUserDocumentsDirectory >>= print

  S.putStr $ E.encodeUtf8 $ substitute helloTemplate helloContext
  where
    helloTemplate = "Hello, $name!\n"
    helloContext  = context [("name", "Joe")]

  getCurrentTime >>= print

  now <- getCurrentTime

  let (year, month, day) = toGregorian $ utctDay now
  putStrLn $ "Year: " ++ show year
  putStrLn $ "Month: " ++ show month
  putStrLn $ "Day: " ++ show day

  -- handle <- openFile "file.txt" ReadMode
  -- contents <- hGetContents handle
  -- putStr contents
  -- hClose handle

  -- writeFile "file.txt" "Hello, world!"
  -- readFile "file.txt" >>= print
