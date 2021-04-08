--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (msum, filterM, (<=<), (>=>), liftM, filterM)
import Control.Applicative ((<|>), Alternative(..))
import Control.Monad.Fail (MonadFail)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>), mconcat)
import Data.Functor ((<$>), fmap)
import Data.Functor.Identity
import Data.Char (isSpace, toLower, toUpper)
import Data.List (intercalate, intersperse, foldl', isPrefixOf)
import Data.List (groupBy)

import System.FilePath (takeFileName, joinPath, splitPath, dropFileName, takeDirectory, takeBaseName, splitDirectories, (</>))
import System.Environment (getArgs)
import Data.Time.Format (TimeLocale, defaultTimeLocale, parseTimeM, formatTime)
import Data.Time.Clock (UTCTime(..))
-- import Text.Regex (Regex, matchRegex, mkRegex)

import Text.Read (readMaybe)
import Text.HTML.TagSoup (Tag(..))
import Text.Blaze.Html                 (toHtml, toValue, (!))
import Text.Blaze.Html5.Attributes     (href, class_)
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.DocTemplates as DT
import qualified Text.Blaze.Html5 as H
import Text.Pandoc.Options
import Hakyll.Web.Sass (sassCompiler)
import Hakyll hiding (pandocCompiler)

import Site.Configuration
import Site.Context
import Site.Routes
import Site.Pandoc

--------------------------------------------------------------------------------
main :: IO ()
main = do
    isWatching <- fmap (== "watch") <$> listToMaybe <$> getArgs
    let allPattern =
         case isWatching of
            Just True -> (postPattern .||. draftPattern)
            _         -> postPattern

    hakyllWith config $ do

        excludePattern <- liftM fromList $ includeTagM "icelandic" <=< getMatches $ postPattern
        let visiblePattern = allPattern .&&. complement excludePattern

        pages   <- buildPaginateWith postsGrouper visiblePattern postsPageId
        -- pages      <- buildPages visiblePattern (\i -> fromCapture "pages/*/index.html" (show i))
        categories <- buildCategories visiblePattern (fromCapture "categories/*/index.html")
        tags       <- buildTags visiblePattern (fromCapture "tags/*/index.html")

        match (fromList ["matrix.html", "resume/*.html"]) $ do
          route idRoute
          compile $ getResourceBody >>= relativizeUrls

        match ("assets/themes/**" .||. "assets/js/**" .||. "assets/css/*.css" .||. "assets/images/**" .||. "robots.txt" .||. "keybase.txt") $ do
          route idRoute
          compile copyFileCompiler

        match "assets/css/*.scss" $ do
          route $ setExtension "css"
          let compressCssItem = fmap compressCss
          compile (compressCssItem <$> sassCompiler)

        {--
        match "assets/css/style.scss" $ do
         route   $ setExtension "css"
         compile $ getResourceFilePath
              >>= \fp -> unixFilter "sass" ["--scss", "--compass", "--style", "compressed", "--load-path",  "assets/css/sass", fp] ""
              >>= makeItem
              >>= return . fmap compressCss

        match "assets/css/style.scss" $ do
         route $ setExtension "css"
         compile $ getResourceString
              >>= withItemBody (unixFilter "sass" ["-s", "--scss", "--compass", "--style", "compressed", "--load-path",  "assets/css/sass"])
              >>= return . fmap compressCss

        match "style.scss" $ do
         route   $ setExtension "css"
         compile $ getResourceString >>=
              withItemBody (unixFilter "sass" ["-s", "--scss"]) >>=
              return . fmap compressCss
        --}

        match (fromList ["about.rst", "contact.markdown"]) $ do
          -- route $ setExtension "html"
          route niceRoute
          compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" postContext
            >>= loadAndApplyTemplate "templates/_layout.html" postContext
            >>= relativizeUrls
            >>= removeIndexHtml

        rulesForTags categories tags categories "categories" (\tag -> "Posts in category \"" ++ tag ++ "\"")
        rulesForTags tags tags categories "tags" (\tag -> "Posts tagged \"" ++ tag ++ "\"")

        {--
        -- Configure pagenations
        archives <-
         let archivePageName n =  if n == 1
                 then fromFilePath "archives.html"
                 else fromFilePath $ "archives/" ++ show n ++ ".html"
         in  buildPaginateWith
           (sortRecentFirst >=> return . paginateEvery 15) "posts/**" archivePageName

        paginateRules archives $ \pageNum identifier -> do
         route niceRoute
         compile $ do
             posts <- fmap groupPosts $ recentFirst =<< loadAll identifier
             let archiveCtx =
                     listField "years"
                         (
                              field "year" (return . fst . itemBody) <>
                              listFieldWith "posts" (postCtxWithTags tags categories) (return . snd . itemBody)
                         )
                         (sequence $ fmap (\(y, is) -> makeItem (show y, is)) posts)
                     <> constField "title" "Archives"
                     -- <> listField "posts" (postCtxWithTags tags categories) (return posts)
                     <> paginateContext archives pageNum
                     <> siteContext
             makeItem ""
                 >>= loadAndApplyTemplate "templates/archives.html" archiveCtx
                 >>= loadAndApplyTemplate "templates/_layout.html" archiveCtx
                 >>= relativizeUrls
                 >>= removeIndexHtml
        --}

        -- blogs
        match allPattern $ do
           route   $ niceRoute
           compile $ postCompiler
               >>= saveSnapshot postSnapshot
               >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags categories tags)
               >>= loadAndApplyTemplate "templates/_layout.html" postContext
               >>= indexCompiler
               >>= relativizeUrls
               >>= removeIndexHtml

        -- blog pages
        paginateRules pages $ \i _ -> do
           route   $ niceRoute
           compile $ makeItem (show i)
               >>= loadAndApplyTemplate "templates/archives.html" (postCtx i pages categories tags)
               >>= loadAndApplyTemplate "templates/_layout.html" postContext
               >>= indexCompiler
               >>= relativizeUrls
               >>= removeIndexHtml

        {-
        -- blog category index
        tagsRules categories $ \category pattern -> do
           catPages <- buildPages pattern (\i -> fromCaptures "categories/*/*/index.html" [category, show i])
           route   $ idRoute
           compile $ makeItem category
               >>= loadAndApplyTemplate "templates/post-list.html" (postCtx 1 catPages categories tags)
               >>= loadAndApplyTemplate "templates/_layout.html" postContext
               >>= indexCompiler
               >>= relativizeUrls
           paginateRules catPages $ \i _ -> do -- blog category pages
               route   $ idRoute
               compile $ makeItem category
                   >>= loadAndApplyTemplate "templates/post-list.html" (postCtx i catPages categories tags)
                   >>= loadAndApplyTemplate "templates/_layout.html" postContext
                   >>= indexCompiler
                   >>= relativizeUrls

        -- blog tags index
        tagsRules tags $ \tag pattern -> do
           tagPages <- buildPages pattern (\i -> fromCaptures "tags/*/*/index.html" [tag, show i])
           route   $ idRoute
           compile $ makeItem tag
               >>= loadAndApplyTemplate "templates/post-list.html" (postCtx 1 tagPages categories tags)
               >>= loadAndApplyTemplate "templates/_layout.html" postContext
               >>= indexCompiler
               >>= relativizeUrls
           paginateRules tagPages $ \i _ -> do -- blog tags pages
               route idRoute
               compile $ makeItem tag
                   >>= loadAndApplyTemplate "templates/post-list.html" (postCtx i tagPages categories tags)
                   >>= loadAndApplyTemplate "templates/_layout.html" postContext
                   >>= indexCompiler
                   >>= relativizeUrls

        match "posts/**" $ do
         route (setExtension "html")
         compile $ pandocCompilerWithTOC
           >>= saveSnapshot "content"
           >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags categories)
           >>= loadAndApplyTemplate "templates/_layout.html" (postCtxWithTags tags categories)
           >>= relativizeUrls

        -- Render the articles

        match "posts/*" $ do
          route $ setExtension "html"
          compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
              >>= loadAndApplyTemplate "templates/article.hamlet" (postContext tagsOfPosts)
              >>= saveSnapshot "content"
              >>= loadAndApplyTemplate "templates/social.hamlet" (postContext tagsOfPosts)
              >>= loadAndApplyTemplate "templates/flame.hamlet" (postContext tagsOfPosts)
              >>= relativizeUrls
        -}

        create ["tagCloud.html"] $ do
          route idRoute
          compile $ do
            posts <- fmap (take 10) $ loadPosts visiblePattern
            let archiveContext =
                  listField "posts" postContext (return posts) <>
                  field "tagcloud" (const $ renderTagCloud 30 150 tags) <>
                  constField "title" "Archives" <>
                  siteContext
            makeItem ""
              >>= loadAndApplyTemplate "templates/tag_cloud.html" archiveContext
              >>= loadAndApplyTemplate "templates/_layout.html" archiveContext
              >>= relativizeUrls

        match "index.html" $ do
          route idRoute
          compile $ do
            posts <- fmap (take 5) $ loadPosts visiblePattern
            let (headPost, tailPosts) = splitAt 1 posts
            let indexContext = listField "headPost" teaserContext (return headPost) <>
                               listField "tailPosts" teaserContext (return tailPosts) <>
                               constField "title" "Blog" <> siteContext
            getResourceBody
              >>= applyAsTemplate indexContext
              >>= loadAndApplyTemplate "templates/_layout.html" indexContext
              >>= relativizeUrls


        match "templates/**" $
          compile $ templateCompiler

        create ["atom.xml"] $ do
          route idRoute
          compile $ do
            posts <- fmap (take 10) $ loadPosts visiblePattern
            renderAtom feedConfiguration feedContext posts

        create ["rss.xml"] $ do
          route idRoute
          compile $ do
            posts <- fmap (take 10) $ loadPosts visiblePattern
            renderRss feedConfiguration feedContext posts


--------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------

rulesForTags :: Tags -> Tags -> Tags -> String -> (String -> String) -> Rules ()
rulesForTags ruleTags tags categories folderName titleForTag = tagsRules ruleTags $ \tag pattern -> do
    let title = titleForTag tag -- "Posts tagged \"" ++ tag ++ "\""
    pages <- buildPaginateWith postsGrouper pattern (\i -> fromCaptures (fromGlob (folderName ++ "/*/*/index.html")) [tag, show i])
    route   $ idRoute
    compile $ makeItem tag
        >>= loadAndApplyTemplate "templates/tag.html" (postCtx 1 pages tags categories)
        >>= loadAndApplyTemplate "templates/_layout.html" postContext
        >>= indexCompiler
        >>= relativizeUrls
    paginateRules pages $ \i _ -> do -- blog tags pages
        route   $ idRoute
        compile $ makeItem tag
            >>= loadAndApplyTemplate "templates/tag.html" (postCtx i pages tags categories)
            >>= loadAndApplyTemplate "templates/_layout.html" postContext
            >>= indexCompiler
            >>= relativizeUrls

postsGrouper :: (MonadFail m, MonadMetadata m) => [Identifier] -> m [[Identifier]]
postsGrouper = liftM (paginateEvery postPerPage) . sortRecentFirst

postsPageId :: PageNumber -> Identifier
postsPageId n = fromFilePath $ if (n == 1) then "archives.html" else "archives/" ++ show n ++ "/index.html"

{--
sassCompiler :: Compiler (Item String)
sassCompiler = do
   ident <- getUnderlying
   output <- unixFilter "sass" [toFilePath ident] ""
   makeItem output
--}

loadDecks :: Pattern -> Compiler [Item String]
loadDecks =
   recentFirst <=< flip loadAllSnapshots decksSnapshot

renderPostAtom :: [Item String] -> Compiler (Item String)
renderPostAtom =
   -- renderAtom feedConfiguration
   renderAtom feedConfiguration atomContext

-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr :: String -> String
    removeIndexStr str@(x:xs) | str == "/index.html" = ""
                              | otherwise = x:removeIndexStr xs
    removeIndexStr [] = []

-- contexts
atomContext :: Context String
atomContext =
   mapContext cdata (pageTitleField "title")   <>
   aliasContext alias metadataField        <>  -- description from metadata
   teaserField "description" postSnapshot  <>  -- teaser is description
   previewField "description" postSnapshot <>  -- first paragraph is description
   urlField' "url"
   where
      alias "description" = "summary"
      alias x             = x
      cdata s | "<![CDATA[" `isPrefixOf` s = s
      cdata s                              = "<![CDATA[" <> s <> "]]>"

-- metadata
includeTagM :: MonadMetadata m => String -> [Identifier] -> m [Identifier]
includeTagM tag =
   filterTagsM (return . elem tag)

filterTagsM :: MonadMetadata m => ([String] -> m Bool) -> [Identifier] -> m [Identifier]
filterTagsM p =
   filterM (p <=< getTags)

-- pagination
buildPages :: (MonadMetadata m, MonadFail m) => Pattern -> (PageNumber -> Identifier) -> m Paginate
buildPages pattern makeId =
   buildPaginateWith
      (return . paginateEvery postPerPage <=< sortRecentFirst)
      pattern
      makeId
