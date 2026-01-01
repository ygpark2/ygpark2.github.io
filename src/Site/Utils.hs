module Site.Utils
    ( escapeJson
    , normalizeSpaces
    , truncateWithEllipsis
    , normalizeUrlPath
    , unwrap
    , transformDescription
    , cutDescription
    , getTagIdentifier
    , getPageIdentifier
    , addIndexRoute
    , removeExtension
    , jsonRoute
    , identifierToUrl
    , ensureTrailingSlash
    , simplifiedUrl
    , identifierToDisqus
    , countText
    , getWeight
    , removeIfExists
    , getSettingInt
    , getSettingString
    , getNavMenu
    , getNavMenuIO
    , writeFileIfChanged
    ) where

import Control.Exception (catch, throwIO)
import Control.Monad (filterM, when)
import Data.Char (isSpace, ord)
import Data.List (dropWhileEnd, intercalate)
import Numeric (showHex)
import System.Directory (doesFileExist, removeFile)
import System.FilePath (takeDirectory, takeBaseName, splitDirectories)
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)

import Hakyll hiding (trim)

escapeJson :: String -> String
escapeJson = concatMap escapeChar
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c
        | ord c < 0x20 = "\\u00" ++ padHex (showHex (ord c) "")
        | otherwise = [c]
    padHex [x] = ['0', x]
    padHex [x, y] = [x, y]
    padHex xs
        | length xs >= 4 = xs
        | length xs == 3 = '0' : xs
        | otherwise = replicate (4 - length xs) '0' ++ xs

normalizeSpaces :: String -> String
normalizeSpaces = unwords . words

truncateWithEllipsis :: Int -> String -> String
truncateWithEllipsis maxLen txt
    | length txt <= maxLen = txt
    | maxLen <= 3 = take maxLen txt
    | otherwise = take (maxLen - 3) txt ++ "..."

normalizeUrlPath :: String -> String
normalizeUrlPath path =
    let trimmed = dropWhile (== '/') path
    in '/' : trimmed

getSettingInt :: String -> Int -> Int
getSettingInt key fallback = unsafePerformIO $ do
    contents <- readSettings
    case lookupKey key contents of
        Just value ->
            case reads value of
                [(num, "")] -> return num
                _ -> return fallback
        Nothing -> return fallback
  where
    readSettings = readSettingsFile
    lookupKey = lookupNestedSetting

getSettingString :: String -> String -> String
getSettingString key fallback = unsafePerformIO $ do
    contents <- readSettingsFile
    return $ maybe fallback id (lookupNestedSetting key contents)

getNavMenu :: [(String, String)]
getNavMenu = unsafePerformIO getNavMenuIO

getNavMenuIO :: IO [(String, String)]
getNavMenuIO = do
    contents <- readSettingsFile
    let items = parseNavMenuNested "site" "topNavMenu" (lines contents)
    return $ if null items then defaultNavMenu else items
  where
    parseNavMenuNested rootKey menuKey lines' =
        case findBlockAt 0 rootKey lines' of
            Nothing -> []
            Just (_, block) -> parseNavMenu menuKey block

    findBlockAt indent key lines' =
        case dropWhile (not . isKeyLineAt indent key) lines' of
            [] -> Nothing
            (line:rest) ->
                let baseIndent = leadingSpaces line
                    childIndent = baseIndent + 2
                    block = takeWhile (\l -> null (trim l) || leadingSpaces l >= childIndent) rest
                in Just (childIndent, block)

    isKeyLineAt indent key line =
        let stripped = trim (takeWhile (/= '#') line)
            (name, _) = break (== ':') stripped
        in not (null stripped)
           && leadingSpaces line == indent
           && trim name == key

    leadingSpaces = length . takeWhile (== ' ')

readSettingsFile :: IO String
readSettingsFile = do
    let path = "settings.yml"
    exists <- doesFileExist path
    if exists
        then readFile path
        else return ""

lookupSetting :: String -> String -> Maybe String
lookupSetting key =
    foldr (\line acc -> acc <|> parseLine key line) Nothing . lines
  where
    parseLine k line =
        let stripped = trim (takeWhile (/= '#') line)
            (name, rest) = break (== ':') stripped
        in if trim name == k
            then Just (trim (drop 1 rest))
            else Nothing

lookupNestedSetting :: String -> String -> Maybe String
lookupNestedSetting key = case splitKey key of
    [] -> const Nothing
    (root:rest) -> findNested root rest . lines
  where
    splitKey = map trim . splitOn '.'
    splitOn _ "" = []
    splitOn c s =
        let (a, b) = break (== c) s
        in a : case b of
            [] -> []
            (_:rest) -> splitOn c rest
    findNested _ [] _ = Nothing
    findNested root (leaf:[]) lines' =
        findValue root leaf lines'
    findNested root (next:rest) lines' =
        case findBlock root lines' of
            Nothing -> Nothing
            Just (indent, block) ->
                case findBlockAt indent next block of
                    Nothing -> Nothing
                    Just (nextIndent, subBlock) ->
                        findNestedAt nextIndent rest subBlock
    findNestedAt _ [] _ = Nothing
    findNestedAt indent (leaf:[]) lines' = findValueAt indent leaf lines'
    findNestedAt indent (next:rest) lines' =
        case findBlockAt indent next lines' of
            Nothing -> Nothing
            Just (nextIndent, subBlock) ->
                findNestedAt nextIndent rest subBlock

    findBlock key lines' =
        findBlockAt 0 key lines'

    findBlockAt indent key lines' =
        case dropWhile (not . isKeyLineAt indent key) lines' of
            [] -> Nothing
            (line:rest) ->
                let baseIndent = leadingSpaces line
                    childIndent = baseIndent + 2
                    block = takeWhile (\l -> null (trim l) || leadingSpaces l >= childIndent) rest
                in Just (childIndent, block)

    findValue rootKey leafKey lines' =
        case findBlock rootKey lines' of
            Nothing -> Nothing
            Just (indent, block) -> findValueAt indent leafKey block

    findValueAt indent key lines' =
        case dropWhile (not . isKeyLineAt indent key) lines' of
            [] -> Nothing
            (line:_) -> Just (trim (drop 1 (dropWhile (/= ':') line)))

    isKeyLineAt indent key line =
        let stripped = trim (takeWhile (/= '#') line)
            (name, _) = break (== ':') stripped
        in not (null stripped)
           && leadingSpaces line == indent
           && trim name == key

    leadingSpaces = length . takeWhile (== ' ')

parseNavMenu :: String -> [String] -> [(String, String)]
parseNavMenu menuKey = go False 0 Nothing Nothing []
  where
    go _ _ currentTitle currentUrl acc [] =
        let (finalItem, _) = finalize currentTitle currentUrl
        in reverse $ maybe acc (: acc) finalItem
    go inBlock indent currentTitle currentUrl acc (line:rest) =
        let stripped = trim (takeWhile (/= '#') line)
            leading = length line - length (dropWhile (== ' ') line)
        in if stripped == (menuKey ++ ":")
            then go True (leading + 2) Nothing Nothing acc rest
            else if not inBlock
                then go False indent currentTitle currentUrl acc rest
                else if null stripped
                    then go True indent currentTitle currentUrl acc rest
                    else if leading < indent
                        then go False indent currentTitle currentUrl acc (line:rest)
                    else if head stripped == '-' then
                        let (nextTitle, nextUrl) = finalize currentTitle currentUrl
                            acc' = maybe acc (: acc) nextTitle
                        in go True indent (parseValue "title" stripped) (parseValue "url" stripped) acc' rest
                    else if startsWith stripped "title:" then
                        go True indent (Just (trim (drop 6 stripped))) currentUrl acc rest
                    else if startsWith stripped "url:" then
                        go True indent currentTitle (Just (trim (drop 4 stripped))) acc rest
                    else if isKeyLine stripped && leading == 0 then
                        let (nextTitle, nextUrl) = finalize currentTitle currentUrl
                            acc' = maybe acc (: acc) nextTitle
                        in go False indent Nothing Nothing acc' (line:rest)
                    else
                        go True indent currentTitle currentUrl acc rest
    finalize title url =
        case (title, url) of
            (Just t, Just u) -> (Just (t, u), Nothing)
            _ -> (Nothing, Nothing)
    parseValue key val =
        if startsWith val ("- " ++ key ++ ":")
            then Just (trim (drop (length key + 3) val))
            else Nothing
    isKeyLine val = ':' `elem` val && not (head val == '-')
    startsWith val prefix = take (length prefix) val == prefix

defaultNavMenu :: [(String, String)]
defaultNavMenu =
    [ ("Archive", "/archive/")
    , ("Tags", "/tags/")
    , ("Projects", "/projects/")
    , ("About", "/about/")
    , ("Search", "/pages/search/")
    , ("Map", "/map/")
    ]

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) a b = case a of
    Just _ -> a
    Nothing -> b

unwrap :: String -> String
unwrap str -- TODO decode escaped chars
    | str == "\"" || str == "" = str
    | otherwise =
        case str of
            '"' : rest ->
                case reverse rest of
                    '"' : middleRev -> reverse middleRev
                    _ -> str
            _ -> str

transformDescription :: String -> String
transformDescription = map (\ch -> if ch == '\n' then ' ' else ch)

cutDescription :: String -> String
cutDescription d
    | length d > 512 = reverse (dropWhile isSpace $ dropWhile (not . isSpace) $ reverse $ take 512 d) ++ "..."
    | otherwise = d

getTagIdentifier :: String -> PageNumber -> Identifier
getTagIdentifier tag pageNum
    | pageNum == 1 = fromFilePath $ "tag/" ++ tag ++ "/"
    | otherwise = fromFilePath $ "tag/" ++ tag ++ "/page/" ++ show pageNum ++ "/"

getPageIdentifier :: PageNumber -> Identifier
getPageIdentifier pageNum
    | pageNum == 1 = fromFilePath ""
    | otherwise = fromFilePath $ "page/" ++ show pageNum ++ "/"

addIndexRoute :: Routes
addIndexRoute = customRoute (\id' ->
    if toFilePath id' == ""
        then "index.html"
        else dropWhileEnd (== '/') (toFilePath id') ++ "/index.html")

-- | Transforms 'something/something.md' into 'something/something/index.html'
-- and 'something/YYYY-MM-DD-something.md' into 'something/something/index.html'
removeExtension :: Routes
removeExtension = customRoute $ removeExtension' . toFilePath

removeExtension' :: String -> String
removeExtension' filepath =
    let dir  = takeDirectory filepath
        base = takeBaseName filepath
        parts = splitAll "-" base
        base' = case parts of
            (y:m:d:rest) | length y == 4 && length m == 2 && length d == 2 -> intercalate "-" rest
            _ -> base
        path = intercalate "/" $ filter (not . null) (splitDirectories dir ++ [base'])
    in path ++ "/index.html"

jsonRoute :: Routes
jsonRoute = customRoute (jsonRoute' . toFilePath)

jsonRoute' :: String -> String
jsonRoute' filepath =
    let urlPath = identifierToUrl filepath
        trimmed = dropWhile (== '/') urlPath
    in trimmed ++ "index.json"

identifierToUrl :: String -> String
identifierToUrl filepath =
    let dirs = splitDirectories (takeDirectory filepath)
        base = takeBaseName filepath
        -- strip leading date prefix like YYYY-MM-DD- from base name
        base' = case splitAll "-" base of
                  (y:m:d:rest) | length y == 4 && length m == 2 && length d == 2 -> intercalate "-" rest
                  _ -> base
        path = intercalate "/" (dirs ++ [base'])
    in ensureTrailingSlash path

ensureTrailingSlash :: String -> String
ensureTrailingSlash p
    | null p = "/"
    | otherwise =
        case reverse p of
            '/' : _ -> p
            _ -> p ++ "/"

simplifiedUrl :: String -> String
simplifiedUrl url =
    let trimmed = dropWhileEnd (== '/') url
        stripped =
            case reverse trimmed of
                ('l':'m':'t':'h':'.':'x':'d':'n':'i':'/':rest) -> reverse rest
                _ -> trimmed
    in case stripped of
        ('/':_) -> stripped
        _ -> '/' : stripped

identifierToDisqus :: String -> String
identifierToDisqus filepath =
    let base = takeBaseName filepath
    in case splitAll "-" base of
        (y:m:d:rest) | length y == 4 && length m == 2 && length d == 2 -> intercalate "-" rest
        _ -> base

countText :: Int -> String -> String -> String -> String
countText count one two many
    | count `mod` 100 `div` 10 == 1 =
        show count ++ " " ++ many
    | count `mod` 10 == 1 =
        show count ++ " " ++ one
    | (count `mod` 10) `elem` [2, 3, 4] =
        show count ++ " " ++ two
    | otherwise =
        show count ++ " " ++ many

getWeight :: Int -> Int -> Int -> Int
getWeight minCount maxCount count
    | maxCount <= minCount = 3
    | otherwise =
        let c = fromIntegral count :: Double
            lo = fromIntegral minCount
            hi = fromIntegral maxCount
            scaled = 1 + 4 * (c - lo) / (hi - lo)
        in round scaled

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

writeFileIfChanged :: FilePath -> String -> IO ()
writeFileIfChanged path content = do
    existing <- readFile path `catch` handleMissing
    when (existing /= content) $
        writeFile path content
  where
    handleMissing e
        | isDoesNotExistError e = return ""
        | otherwise = throwIO e
