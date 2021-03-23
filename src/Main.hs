--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad ((>=>))
import Control.Applicative ((<|>), Alternative(..))
-- import Control.Monad (msum, filterM, (<=<), (>=>), liftM, filterM)
-- import Control.Monad.Fail (MonadFail)
-- import Data.Maybe (fromMaybe, listToMaybe)
-- import Data.Monoid ((<>), mappend)
-- import Data.Functor ((<$>), fmap)
-- import Data.Char (isSpace, toLower, toUpper)
-- import Data.List (intercalate, intersperse, foldl', isPrefixOf)
import Data.Functor.Identity
import Hakyll
import Hakyll.Web.Sass (sassCompiler)
import Text.Pandoc.Options
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.DocTemplates as DT

import Site.Configuration
import Site.Context

--------------------------------------------------------------------------------
main :: IO ()
-- main = hakyll $ do
main = hakyllWith config $ do
    match (fromList ["matrix.html", "resume/*.html"]) $ do
      route idRoute
      compile $ getResourceBody >>= relativizeUrls

    match ("assets/js/**" .||. "assets/css/*.css" .||. "assets/images/**" .||. "robots.txt" .||. "keybase.txt" .||. "resume/*") $ do
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

    -- match (fromList ["about.md", "contact.md", "index.md"]) $ do
    match (fromList ["about.rst", "contact.markdown"]) $ do
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postContext
        >>= loadAndApplyTemplate "templates/default.html" postContext
        >>= relativizeUrls

    tags <- buildTags "posts/**" (fromCapture "tags/*.html")
    categories <- buildCategories "posts/**" (fromCapture "categories/*.html")
    let tagsRules' t s = tagsRules t $ \tag pattern -> do
            let title = s ++ " \"" ++ tag ++ "\""
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll pattern
                let ctx = constField "title" title
                          `mappend` listField "posts" (postCtxWithTags tags categories) (return posts)
                          `mappend` siteContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/tag.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls
    tagsRules' categories "Posts in category"
    tagsRules' tags "Posts tagged"


    -- Configure pagenations
    archive <-
      let archivePageName n =  if n == 1
               then fromFilePath "archive.html"
               else fromFilePath $ "archive/" ++ show n ++ ".html"
      in  buildPaginateWith
        (sortRecentFirst >=> return . paginateEvery 15) "posts/**" archivePageName

    paginateRules archive $ \pageNum identifier -> do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll identifier
            let archiveCtx =
                    constField "title" "Archives"
                    <> listField "posts" (postCtxWithTags tags categories) (return posts)
                    <> paginateContext archive pageNum
                    <> siteContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "posts/**" $ do
      route (setExtension "html")
      compile $ pandocCompilerWithTOC
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags categories)
        >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags categories)
        >>= relativizeUrls

    -- Render the articles
    {-
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
        posts <- fmap (take 10) . recentFirst =<< loadAll "posts/**"
        let archiveContext =
              listField "posts" postContext (return posts) `mappend`
              field "tagcloud" (const $ renderTagCloud 30 150 tags) `mappend`
              constField "title" "Archives" `mappend`
              siteContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag_cloud.html" archiveContext
          >>= loadAndApplyTemplate "templates/default.html" archiveContext
          >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- fmap (take 5) . recentFirst =<< loadAll "posts/**"
        let (headPost, tailPosts) = splitAt 1 posts
        let indexContext = listField "headPost" teaserContext (return headPost) `mappend` listField "tailPosts" teaserContext (return tailPosts) `mappend` constField "title" "Blog" `mappend` siteContext
        getResourceBody
          >>= applyAsTemplate indexContext
          >>= loadAndApplyTemplate "templates/default.html" indexContext
          >>= relativizeUrls

    match "templates/**" (compile templateCompiler)

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        posts <- fmap (take 10) . recentFirst
                   =<< loadAllSnapshots "posts/**" "content"
        renderAtom feedConfiguration feedContext posts

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        posts <- fmap (take 10) . recentFirst
                   =<< loadAllSnapshots "posts/**" "content"
        renderRss feedConfiguration feedContext posts


--------------------------------------------------------------------------------
pandocCompilerWithTOC :: Compiler (Item String)
pandocCompilerWithTOC = do
      ident <- getUnderlying
      toc   <- getMetadataField ident "toc"
      let writerSettings = case toc of
                                Just "yes"  -> myWriterOptionsToc
                                _           -> myWriterOptions
      pandocCompilerWith defaultHakyllReaderOptions writerSettings

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions {
      writerReferenceLinks = True
    , writerHTMLMathMethod = MathJax ""
    }

myWriterOptionsToc :: WriterOptions
myWriterOptionsToc = myWriterOptions {
      writerTableOfContents = True
    , writerNumberSections  = True
    , writerTOCDepth        = 2
    , writerTemplate        = Just tocTemplate
    -- , writerTemplate = Just "$if(toc)$<div id=\"toc\"><h2>Table of Contents</h2>$toc$</div>$endif$\n$body$"
    -- , writerStandalone = True
    }

tocTemplate :: DT.Template T.Text
tocTemplate = either error id . runIdentity . DT.compileTemplate "" $ T.unlines
  [ "<div class=\"toc\"><div class=\"header\">Table of Contents</div>"
  , "$toc$"
  , "</div>"
  , "$body$"
  ]

assetsRoute :: Routes
assetsRoute = customRoute $ (\x -> x :: String) . drop 7 . toFilePath

projectRoute :: Routes
projectRoute =
  idRoute `composeRoutes`
  (customRoute $ (++ "/index") . takeWhile (/= '.') . drop 9 . toFilePath ) `composeRoutes`
  setExtension "html"
