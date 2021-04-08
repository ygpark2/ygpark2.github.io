{-# LANGUAGE OverloadedStrings #-}

module Site.Context (siteContext,
                     postCtx,
                     postCtxWithTags,
                     feedContext,
                     postContext,
                     aliasContext,
                     teaserContext,
                     pageTitleField,
                     previewField,
                     urlField',
                     groupPosts,
                     loadPosts) where

import           Hakyll                     hiding (load)

import Control.Monad (msum, filterM, (<=<), (>=>), liftM, filterM)
import Control.Applicative ((<|>), Alternative(..))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>), mconcat)
import Data.Functor ((<$>), fmap)
-- import Data.Functor.Identity
import Data.Char (isSpace, toLower, toUpper)
import Data.List (groupBy, intercalate, intersperse, foldl', isPrefixOf)

import System.FilePath (takeFileName, joinPath, splitPath, dropFileName, takeDirectory, takeBaseName, splitDirectories, (</>))
-- import System.Environment (getArgs)
import Data.Time.Format (TimeLocale, defaultTimeLocale, parseTimeM, formatTime)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (toGregorian)
-- import Text.Regex (Regex, matchRegex, mkRegex)
import Text.Regex.Posix hiding (empty)

import Text.Read (readMaybe)
import Text.HTML.TagSoup (Tag(..))
import Text.Blaze.Html                 (toHtml, toValue, (!))
import Text.Blaze.Html5.Attributes     (href, class_)
import Text.Blaze.Html.Renderer.String (renderHtml)

import           Data.Configurator          (Worth (Required), getMap, load)
import           Data.Configurator.Types    (convert)
import qualified Data.HashMap.Strict        as H (empty, lookup, lookupDefault)
import qualified Data.Text                  as T
import qualified Data.Map                   as M
import qualified Text.Blaze.Html5           as H
import qualified Hakyll.Core.Metadata       as Meta
import Text.HTML.TagSoup (Tag(..))

import           Data.Maybe
import           System.IO.Unsafe           (unsafePerformIO)
import           Text.Regex                 (mkRegex, subRegex)

import Site.Routes
import Site.Archive
import Site.Pandoc

-- import           Debug.Trace
-- import           Data.Configurator.Types.Internal (Name)

--------------------------------------------------------------------------------
data AppConfig = AppConfig {
                     cfgTitle :: String,
                     cfgDescription :: String,
                     cfgEmail :: String,
                     cfgUrl :: String,
                     cfgHeaderType :: String,
                     cfgPaginationType :: String,
                     cfgSnsGithubUsername :: String,
                     cfgSnsTwitterUsername :: String,
                     cfgSnsGoogleplay :: String,
                     cfgSnsGooglePlusCommunity :: String,
                     cfgSnsGooglePlusProfile :: String
                 } -- deriving (Show, Read)

loadConfig :: IO AppConfig
loadConfig = do
  conf <- load [Required "src/application.cfg"] >>= getMap
  let cfg def key =  fromMaybe def . maybe (Just def) convert $ H.lookup key conf
  return AppConfig { cfgTitle = cfg "" $ T.pack "site.title" 
                   , cfgDescription = cfg "" $ T.pack "site.description" 
                   , cfgEmail = cfg "" $ T.pack "site.email" 
                   , cfgUrl = cfg "" $ T.pack "site.url" 
                   , cfgHeaderType = cfg "" $ T.pack "site.header.type"
                   , cfgPaginationType = cfg "" $ T.pack "site.pagination.type"
                   , cfgSnsGithubUsername = cfg "" $ T.pack "site.sns.github.username"
                   , cfgSnsTwitterUsername = cfg "" $ T.pack "site.sns.twitter.username"
                   , cfgSnsGoogleplay = cfg "" $ T.pack "site.sns.google.play"
                   , cfgSnsGooglePlusCommunity = cfg "" $ T.pack "site.sns.google.plus.community"
                   , cfgSnsGooglePlusProfile = cfg "" $ T.pack "site.sns.google.plus.profile"
                   }

pageTitleField :: String -> Context String
pageTitleField key =
   aliasContext alias metadataField <> -- use page title from metadata
   pathTitleField key               <> -- or read from the path
   constField key "Crypto and Code"    -- alternatively use this
   where
      alias x | x == key = "title"
      alias x            = x

pathTitleField :: String -> Context String
pathTitleField =
   flip field title
   where
      title = maybe empty (emptyTitle . pageTitle) <=< getRoute . itemIdentifier
      pageTitle = intercalate " &#x276f;&#x276f;= " . splitDirectories . capitalize . dropFileName
      emptyTitle "." = empty
      emptyTitle x = return x
      capitalize []     = []
      capitalize (x:xs) = toUpper x : map toLower xs

urlField' :: String -> Context String
urlField' =
  mapContext dropFileName . urlField

categoryField' :: String -> Tags -> Context a
categoryField' =
   tagsFieldWith getCategory (renderLink "@") mconcat

categoryListField :: String -> Tags -> Context a
categoryListField key tags =
   field key (const $ renderList tags)
   where
      renderList = renderTags makeLink (intercalate " ")
      makeLink tag url _ _ _ = renderHtml $ do
         "@"
         H.a ! href (toValue url) $ toHtml tag

tagsField' :: String -> Tags -> Context a
tagsField' =
   tagsFieldWith getTags (renderLink "#") (mconcat . intersperse " ")

tagsListField :: String -> Tags -> Context a
tagsListField key tags =
   field key (const $ renderList tags)
   where
      renderList = renderTags makeLink (intercalate " ")
      makeLink tag url _ _ _ = renderHtml $ do
         "#"
         H.a ! href (toValue url) $ toHtml tag

summaryField :: String -> Context String
summaryField key =
   field key meta                 <>  -- summary from metadata
   teaserField key postSnapshot   <>  -- teaser is summary
   previewField key postSnapshot      -- first paragraph is summary
   where
      meta :: Item a -> Compiler String
      meta item = do
         summary <- getMetadataField' (itemIdentifier item) "summary"
         return . renderHtml $ do
            H.p (toHtml summary)
      alias x | x == key  = "summary"
      alias x             = x

previewField :: String -> Snapshot -> Context String
previewField key snapshot  =
   field key trim'
   where
      trim' item = do
         body <- loadSnapshotBody (itemIdentifier item) snapshot
         return $ withTagList firstParagraph body
      firstParagraph = map fst . takeWhile (\(_, s) -> s > 0) . acc 0 . (map cnt)
      acc _ []           = []
      acc s ((x, s'):xs) = (x, s + s') : acc  (s + s') xs
      cnt tag@(TagOpen "p" _) = (tag, 1)
      cnt tag@(TagClose "p")  = (tag, -1)
      cnt tag                 = (tag, 0)

readingTimeField :: String -> Snapshot -> Context String
readingTimeField key snapshot =
   field key calculate
   where
      calculate item = do
         body <- loadSnapshotBody (itemIdentifier item) snapshot
         return $ withTagList acc body
      acc ts = [TagText (show (time ts))]
      time ts = foldl' count 0  ts `div` 265
      count n (TagText s) = n + length (words s)
      count n _           = n

polishField :: String -> Context String
polishField name =
   functionField name $ \args _ ->
      return $ withTags text' (unwords args)
   where
      text' (TagText s) = TagText (concat $ map f (split isSpace s))
      text' t           = t
      f ""                   = ""
      f ":+1:"               = "👍"
      f ":coffee:"           = "☕️"
      f ":disappointed:"     = "😞"
      f ":frowning:"         = "😦"
      f ":grinning:"         = "😀"
      f ":heart:"            = "❤"
      f ":ramen:"            = "🍜"
      f ":rice_ball:"        = "🍙"
      f ":smile:"            = "😄"
      f ":sushi:"            = "🍣"
      f ":stuck_out_tongue:" = "😛"
      f ":thumbsup:"         = "👍"
      f ":tada:"             = "🎉"
      f x                    = x

publishedGroupField :: String           -- name
                    -> [Item String]    -- posts
                    -> Context String   -- Post context
                    -> Context String   -- output context
publishedGroupField name posts postContext = listField name groupCtx $ do
    tuples <- traverse extractYear posts
    let grouped = groupByYear tuples
    let merged = fmap merge $ grouped
    let itemized = fmap makeItem $ merged

    sequence itemized

    where groupCtx = field "year" (return . show . fst . itemBody)
                  <> listFieldWith "posts" postContext (return . snd . itemBody)

          merge :: [(Integer, [Item String])]  -> (Integer, [Item String])
          merge gs = let conv (year, acc) (_, toAcc) = (year, toAcc ++ acc)
                      in  foldr conv (head gs) (tail gs)

          groupByYear = groupBy (\(y, _) (y', _) -> y == y')

          extractYear :: Item a -> Compiler (Integer,  [Item a])
          extractYear item = do
             time <- getItemUTC defaultTimeLocale (itemIdentifier item)
             let    (year, _, _) = (toGregorian . utctDay) time
             return (year, [item])

tocField :: String -- ^ The name for the created field
         -> Context String
tocField key = field key $ \_ -> do
  itemToc <- makeTOC
  let toc = itemBody itemToc
  return toc

siteContext :: Context String
siteContext = constField "site.title" (cfgTitle conf) `mappend`
              constField "site.description" (cfgDescription conf) `mappend`
              constField "site.email" (cfgEmail conf) `mappend`
              constField "site.url" (cfgUrl conf) `mappend`
              constField "site.header.type" (cfgHeaderType conf) `mappend`
              constField "site.pagination.type" (cfgPaginationType conf) `mappend`
              constField "site.sns.github.username" (cfgSnsGithubUsername conf) `mappend`
              constField "site.sns.twitter.username" (cfgSnsTwitterUsername conf) `mappend`
              constField "site.sns.google.play" (cfgSnsGoogleplay conf) `mappend`
              constField "site.sns.google.plus.community" (cfgSnsGooglePlusCommunity conf) `mappend`
              constField "site.sns.google.plus.profile" (cfgSnsGooglePlusProfile conf) `mappend`
              pageTitleField "page.title"                    <>
              constField "page.description" postDescription  <>
              constField "page.root" postRoot                <>
              urlField' "page.url"                           <>
              pathField "page.path"                          <>
              polishField "polish"                           <>
              metadataField                                  <>
              mainClassName                                  <>
              tocField "toc"                                 <>
              defaultContext
  where conf = unsafePerformIO loadConfig
      -- do
        -- config <- (=<< loadConfig) -- >>= confData
        -- return config
      -- return conf <- loadConfig
      -- return test
    -- let map = getMap cfg;
    -- lst <- require cfg "dir" :: IO Value

    -- print lst
    -- ) "../site.cfg" `mappend`

mainClassName :: Context String
mainClassName = functionField "mainClassName" $ \[arg] item ->
  return $ case () of
    _ | arg =~ s"^archives" -> "archive posts-collapse"
      | arg =~ s"^posts" -> "post posts-expand"
      | arg =~ s"^tagCloud" -> "post posts-expand"
      | arg =~ s"^tags" -> "archive posts-collapse"
      | otherwise -> ""
      where s :: String -> String
            s = id

tocContext :: Context String
tocContext = field "toc" $ \item ->
            loadBody ((itemIdentifier item) { identifierVersion = Just "toc"})

{-
activeClassField :: Context a
activeClassField = functionField "activeClass" $ \[p] _ -> do
    path <- takeBaseName . toFilePath <$> getUnderlying
    return $ case path of
       ("posts":_)    -> "I'm quitting!"
       ("archives":x) -> "Now I will load "
       otherwise  -> "Not sure what you want me to do!  " ++ path


fieldDummyFunction =
  functionField "dummy" f
    where
      f [] _ = return "empty args"
      f [x] _ = return $ case x of
                           ("q":_)    -> "I'm quitting!"
                           ("load":x) -> "Now I will load "
                           otherwise  -> "Not sure what you want me to do!"
      f _ _ = error "dummy: many args"

mainClassName :: Context a
mainClassName = functionField "mainClassName" $ \args item ->
    case args of
        ("q":_)    -> "I'm quitting!"
        ("load":x) -> "Now I will load "
        otherwise  -> "Not sure what you want me to do!"
-}

postContext :: Context String
postContext =
  dateField "date" "%Y-%m-%d" <> siteContext

teaserContext :: Context String
teaserContext =
  teaserField "teaser" "content" <> postContext

-- aliasContext maps a new key to another key. If the other key
-- is not defined or returns empty the alias returns empty.
aliasContext :: (String -> String) -> Context a -> Context a
aliasContext f (Context c) =
   Context $ \k a i -> c (f k) a i <|> c' k
   where
      c' k = noResult $ unwords ["Tried to alias", k, "as", f k, "which doesn't exist"]

postCtxWithTags :: Tags -> Tags -> Context String
postCtxWithTags tags categories =
   pageTitleField "post.title"                        <>
   dateField "post.date" "%B %e, %Y"                  <>
   urlField' "post.url"                               <>
   categoryField' "post.categories" categories          <>
   tagsField' "post.tags" tags                        <>
   field "post.next.url" nextPost                     <>
   field "post.previous.url" previousPost             <>
   summaryField "post.summary"                        <>
   readingTimeField "post.reading.time" postSnapshot  <>
   postContext

postCtx :: PageNumber -> Paginate -> Tags -> Tags -> Context String
postCtx i pages categories tags =
   listField "posts" (postCtxWithTags categories tags) (loadPosts pattern) <>
   archiveContext pattern (postCtxWithTags categories tags)                <>
   categoryListField "categories" categories                             <>
   tagsListField "tags" tags                                             <>
   pagesField i                                                          <>
   siteContext
   where
       pattern = fromList . fromMaybe [] . M.lookup i . paginateMap $ pages
       pagesField = aliasContext alias . paginateContext pages
       alias "pages.first.number"    = "firstPageNum"
       alias "pages.first.url"       = "firstPageUrl"
       alias "pages.next.number"     = "nextPageNum"
       alias "pages.next.url"        = "nextPageUrl"
       alias "pages.previous.number" = "previousPageNum"
       alias "pages.previous.url"    = "previousPageUrl"
       alias "pages.last.number"     = "lastPageNum"
       alias "pages.last.url"        = "lastPageUrl"
       alias "pages.current.number"  = "currentPageNum"
       alias "pages.count"           = "numPages"
       alias x                       = x


simplifiedUrl :: String -> String
simplifiedUrl url = subRegex (mkRegex "/index\\.html$") url "/"

feedContext :: Context String
feedContext =
  postContext `mappend` bodyField "description"

licenseCtx :: Context a
licenseCtx = field "license" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ case Meta.lookupString "license" metadata of
               Nothing -> ""
               Just m -> case M.lookup (trim m) licenses of
                           Nothing -> "unknown license"
                           Just (u,i) -> "<a href=\""++u++"\"><img src=\""++i++"\"/></a>"
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

--licenses :: String -> String
licenses = M.fromList
    [ ("by",       ( "https://creativecommons.org/licenses/by/3.0"
                   , "https://i.creativecommons.org/l/by/3.0/88x31.png"))
    , ("by-sa",    ( "https://creativecommons.org/licenses/by-sa/3.0"
                   , "https://i.creativecommons.org/l/by-sa/3.0/88x31.png"))
    , ("by-nd",    ( "https://creativecommons.org/licenses/by/3.0"
                   , "https://i.creativecommons.org/l/by/3.0/88x31.png"))
    , ("by-nc",    ( "https://creativecommons.org/licenses/by/3.0"
                   , "https://i.creativecommons.org/l/by/3.0/88x31.png"))
    , ("by-nc-sa", ( "https://creativecommons.org/licenses/by-nc-sa/3.0"
                   , "https://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png"))
    , ("by-nc-nd", ( "https://creativecommons.org/licenses/by-nc-nd/3.0"
                   , "https://i.creativecommons.org/l/by-nc-nd/3.0/88x31.png"))]

--------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------
loadPosts :: Pattern -> Compiler [Item String]
loadPosts = recentFirst <=< flip loadAllSnapshots postSnapshot

nextPost :: Item String -> Compiler String
nextPost post = do
   posts <- loadPosts postPattern
   let idents = map itemIdentifier posts
       ident = itemAfter idents (itemIdentifier post)
   case ident of
      Just i -> (fmap (maybe empty toUrl) . getRoute) i
      Nothing -> empty
   where
      itemAfter xs x =
         lookup x $ zip xs (tail xs)

previousPost :: Item String -> Compiler String
previousPost post = do
   posts <- loadPosts postPattern
   let idents = map itemIdentifier posts
       ident = itemBefore idents (itemIdentifier post)
   case ident of
      Just i -> (fmap (maybe empty toUrl) . getRoute) i
      Nothing -> empty
   where
      itemBefore xs x =
         lookup x $ zip (tail xs) xs

-- html
renderLink :: String -> String -> (Maybe FilePath) -> Maybe H.Html
renderLink _ _   Nothing       = Nothing
renderLink pre text (Just url) =
   Just $ do
      toHtml pre
      H.a ! href (toValue $ toUrl url) $ toHtml text


-- misc
split :: (Char -> Bool) -> String -> [String]
split p' s =
   go p' ("", s)
   where
      go _ ("", "") = []
      go p ("", y) = go (not . p) (span (not . p) y)
      go p (x, y) = x : go (not . p) (span (not . p) y)

-- Groups post items by year (reverse order).
groupPosts :: [Item String] -> [(Int, [Item String])]
groupPosts = fmap merge . group . fmap tupelise
    where
        merge :: [(Int, [Item String])] -> (Int, [Item String])
        merge gs   = let conv (year, acc) (_, toAcc) = (year, toAcc ++ acc)
                     in  foldr conv (head gs) (tail gs)

        group ts   = groupBy (\(y, _) (y', _) -> y == y') ts
        tupelise i = let path = (toFilePath . itemIdentifier) i
                     in case postYear path of
                             Just year -> (year, [i])
                             Nothing   -> error $ "[ERROR] wrong format: " ++ path

-- Extracts year from article file name.
postYear :: FilePath -> Maybe Int
postYear s = readMaybe ((head . tail . tail . splitDirectories . takeDirectory) s) :: Maybe Int
