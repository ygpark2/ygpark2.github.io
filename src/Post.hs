-- Post.hs
-- License: MIT3
-- Author: Young Gyu Park
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs, getExecutablePath)
import System.Exit
import System.Directory
import System.FilePath.Posix
import Control.Monad (when)
import Data.Time

data PostOptions = PostOptions
    { title :: String
     , file :: FilePath
    } deriving (Data, Typeable, Show, Eq)

-- Customize your options, including help messages, shortened names, etc.
postOpts :: PostOptions
postOpts = PostOptions
    { title = def &= help "your post title"
     , file = def &= typDir &= help "your post file name"
    }

getOpts :: IO PostOptions
getOpts = cmdArgs $ postOpts
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME = "post"
_PROGRAM_VERSION = "0.0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "기본 포스팅 글을 생성해주는 프로그램"
_COPYRIGHT = "(C) Young Gyu Park 2015"

jungtoTemplate :: String -> String -> String -> String -> String
jungtoTemplate thounsand hundred day today = unlines ["'---",
    "title: " ++ thounsand ++ "차 천일 결사 " ++ hundred ++ "차 백일 기도 정진 " ++ day ++ "일째",
    "date: " ++ today,
    "published: true",
    "tags: 10000 결사, " ++ thounsand ++ "000th, " ++ thounsand ++ "-" ++ hundred ++ "00th, " ++ day ++ "th",
    "---",
    "",
    "#수행일지",
    ""
  ]

postTemplate :: String -> String -> String
postTemplate title today = unlines [ "---",
    "title: " ++ title ,
    "date: " ++ today,
    "published: true",
    "tags: ",
    "---",
    "",
    ""
  ]

main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) getOpts
    optionHandler opts

-- Before directly calling your main program, you should warn your user about incorrect arguments, if any.
optionHandler :: PostOptions -> IO ()
optionHandler opts@PostOptions{..}  = do
    -- Take the opportunity here to weed out ugly, malformed, or invalid arguments.
    when (null title) $ putStrLn "--title is blank!" >> exitWith (ExitFailure 1)
    when (null file) $ putStrLn "--file is blank!" >> exitWith (ExitFailure 1)
    -- When you're done, pass the (corrected, or not) options to your actual program.
    exec opts

exec :: PostOptions -> IO ()
exec opts@PostOptions{..} = do
  now <- getCurrentTime
  currentPath <- getExecutablePath

  -- hakyll date format
  let today = formatTime defaultTimeLocale "%FT%X+09:00" now

  let datePath = formatTime defaultTimeLocale "/../../../posts/%Y/%m/%d/" now

  let newPostDir = takeDirectory currentPath

  createDirectoryIfMissing True (newPostDir ++ datePath)

  let newPostFile = newPostDir ++ datePath  ++ file ++ ".md"

  -- putStrLn $ postTemplate "this is title" today

  writeFile newPostFile $ postTemplate title today

  putStrLn $ "Title => " ++ title
  putStrLn $ "File Path => " ++ newPostFile
