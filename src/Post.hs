-- Post.hs
-- License: MIT3
-- Author: Young Gyu Park
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import System.Directory
import System.FilePath
import Control.Monad (when)
import Data.Char (isAlphaNum, toLower)
import Data.List (dropWhileEnd, intercalate)
import Data.Time

data PostOptions = PostOptions
    { title :: String
     , file :: FilePath
     , thousand :: String
     , hundred :: String
     , day :: String
    } deriving (Data, Typeable, Show, Eq)

-- Customize your options, including help messages, shortened names, etc.
postOpts :: PostOptions
postOpts = PostOptions
    { title = def &= help "your post title"
     , file = def &= typFile &= help "your post file name"
     , thousand = def &= help "jungto: thousand cycle number"
     , hundred = def &= help "jungto: hundred cycle number"
     , day = def &= help "jungto: day number"
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

mapPostTemplate :: String -> String -> String
mapPostTemplate title today = unlines [ "---",
    "title: " ++ title ,
    "date: " ++ today,
    "lat: ", -- 37.5665
    "lng: ", -- 126.9780
    "location: ", -- Seoul
    "published: true",
    "tags: ",
    "---",
    "",
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
-- CLI entry point for creating a new post file with front-matter.
main = do
    args <- getArgs
    let (templateType, restArgs) = parseTemplateArg args
        optsArgs = if null restArgs then ["--help"] else restArgs
    opts <- withArgs optsArgs getOpts
    optionHandler templateType opts

data PostTemplate
    = TemplatePost
    | TemplateMap
    | TemplateJungto
    deriving (Eq, Show)

parseTemplateArg :: [String] -> (PostTemplate, [String])
parseTemplateArg args =
    case args of
        (cmd:rest) ->
            case cmd of
                "post" -> (TemplatePost, rest)
                "map" -> (TemplateMap, rest)
                "jungto" -> (TemplateJungto, rest)
                _ -> (TemplatePost, args)
        [] -> (TemplatePost, [])

-- Before directly calling your main program, you should warn your user about incorrect arguments, if any.
optionHandler :: PostTemplate -> PostOptions -> IO ()
optionHandler templateType opts@PostOptions{..}  = do
    -- Take the opportunity here to weed out ugly, malformed, or invalid arguments.
    when (null title) $ putStrLn "--title is blank!" >> exitWith (ExitFailure 1)
    when (null file) $ putStrLn "--file is blank!" >> exitWith (ExitFailure 1)
    when (null (slugify file)) $ putStrLn "--file contains no usable characters!" >> exitWith (ExitFailure 1)
    when (templateType == TemplateJungto && null thousand) $
        putStrLn "--thousand is blank!" >> exitWith (ExitFailure 1)
    when (templateType == TemplateJungto && null hundred) $
        putStrLn "--hundred is blank!" >> exitWith (ExitFailure 1)
    when (templateType == TemplateJungto && null day) $
        putStrLn "--day is blank!" >> exitWith (ExitFailure 1)
    -- When you're done, pass the (corrected, or not) options to your actual program.
    exec templateType opts

exec :: PostTemplate -> PostOptions -> IO ()
-- Resolve repo root, create dated directory, and write a new post file.
exec templateType opts@PostOptions{..} = do
  now <- getCurrentTime
  cwd <- getCurrentDirectory
  root <- findGitRoot cwd

  -- hakyll date format
  let today = formatTime defaultTimeLocale "%FT%X+09:00" now

  let datePath = formatTime defaultTimeLocale "posts/%Y/%m/%d" now

  let newPostDir = root </> datePath
  let safeTitle = quoteYamlString title
  let safeFile = slugify file

  createDirectoryIfMissing True newPostDir

  let newPostFile = newPostDir </> (safeFile ++ ".md")
  exists <- doesFileExist newPostFile
  when exists $ do
      putStrLn $ "File already exists: " ++ newPostFile
      exitWith (ExitFailure 1)

  -- putStrLn $ postTemplate "this is title" today

  let content =
        case templateType of
            TemplatePost -> postTemplate safeTitle today
            TemplateMap -> mapPostTemplate safeTitle today
            TemplateJungto -> jungtoTemplate thousand hundred day today

  writeFile newPostFile content

  putStrLn $ "Title => " ++ title
  putStrLn $ "File Path => " ++ newPostFile

findGitRoot :: FilePath -> IO FilePath
findGitRoot start = go (normalise start)
  where
    go dir = do
        hasGitDir <- doesDirectoryExist (dir </> ".git")
        hasGitFile <- doesFileExist (dir </> ".git")
        if hasGitDir || hasGitFile
            then return dir
            else do
                let parent = takeDirectory dir
                if parent == dir
                    then return start
                    else go parent

quoteYamlString :: String -> String
quoteYamlString s = "\"" ++ concatMap escape s ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = [c]

slugify :: String -> String
slugify input =
    let lowered = map toLower input
        cleaned = map (\c -> if isAlphaNum c then c else '-') lowered
        trimmed = dropWhileEnd (== '-') $ dropWhile (== '-') cleaned
        collapsed = filter (not . null) (splitOnDash trimmed)
    in intercalate "-" collapsed
  where
    splitOnDash [] = []
    splitOnDash s =
        let (chunk, rest) = break (== '-') s
        in chunk : case rest of
            [] -> []
            (_:xs) -> splitOnDash xs
