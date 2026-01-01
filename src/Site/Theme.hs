{-# LANGUAGE CPP #-}
module Site.Theme
    ( themeBase
    , themeTemplate
    , themePart
    ) where

-- Theme path helpers to switch templates by build mode.
import Hakyll (Identifier, fromFilePath)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

themeBase :: FilePath
themeBase = "templates/themes" </> themeName

themeName :: FilePath
themeName =
#ifdef DEVELOPMENT
    getSetting "site.themeDevelopment" (getSetting "site.theme" defaultTheme)
#else
    getSetting "site.theme" defaultTheme
#endif

defaultTheme :: FilePath
defaultTheme = "default"

getSetting :: String -> FilePath -> FilePath
getSetting key fallback = unsafePerformIO $ do
    let filePath = "settings.yml"
    exists <- doesFileExist filePath
    if not exists
        then return fallback
        else do
            contents <- readFile filePath
            return $ fromMaybe fallback (lookupKey key contents)
  where
    fromMaybe def val = case val of
        Nothing -> def
        Just v -> v
    lookupKey k =
        case splitKey k of
            [] -> const Nothing
            (root:rest) -> findNested root rest . lines
    parseLine k line =
        let stripped = trim (takeWhile (/= '#') line)
            (name, rest) = break (== ':') stripped
        in if trim name == k
            then Just (trim (drop 1 rest))
            else Nothing
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
    trim = dropWhileEnd isSpace . dropWhile isSpace
    (<|>) a b = case a of
        Just _ -> a
        Nothing -> b

themeTemplate :: FilePath -> Identifier
themeTemplate name = fromFilePath (themeBase </> name)

themePart :: FilePath -> Identifier
themePart name = fromFilePath (themeBase </> "parts" </> name)
