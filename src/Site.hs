{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
import           Blaze.ByteString.Builder (toByteString)
import           Control.Applicative ((<$>))
import           Control.Exception
import           Control.Monad (forM_, filterM, liftM, msum, foldM)
import           Data.Char
import           Data.Function (on)
import           Data.List (sortBy, intercalate, isPrefixOf, find, groupBy, dropWhileEnd)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid (mempty, mappend, mconcat)
import           Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Format (formatTime, parseTime)
import           Hakyll hiding (chronological, dateFieldWith, getItemUTC, getTags, paginateContext,
                    pandocCompiler, recentFirst, teaserField)
import           System.Directory
import           System.FilePath (takeFileName)
import           System.IO.Error
import           System.Locale
import           System.Process
import           Text.HTML.TagSoup (Tag(..))
import qualified Text.HTML.TagSoup as TS
import           Text.Pandoc
import           Text.Regex (mkRegex, subRegex)
import           Text.XmlHtml
import           XmlHtmlWriter


config :: Configuration
config = defaultConfiguration
        {   deployCommand = "ls -al" }


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    staticFilesRules
    lessCompilerRules
    scriptsCompilerRules
    commentsRules
    collectionRules
    postsRules
    tagsPagesRules
    archiveRules
    indexPagesRules
    staticPagesRules
    feedRules
    sitemapRules

    match "templates/**" $ compile templateCompiler


--------------------------------------------------------------------------------
-- Defines
--------------------------------------------------------------------------------

#ifdef DEVELOPMENT

archiveTemplateName :: Identifier
archiveTemplateName = "templates/archive-development.html"
defaultTemplateName :: Identifier
defaultTemplateName = "templates/default-development.html"
indexTemplateName :: Identifier
indexTemplateName = "templates/index-development.html"
listTemplateName :: Identifier
listTemplateName = "templates/list-development.html"
postTemplateName :: Identifier
postTemplateName = "templates/post-development.html"
routePlannerTemplateName :: Identifier
routePlannerTemplateName = "templates/route-planner-development.html"
tagsTemplateName :: Identifier
tagsTemplateName = "templates/tags-development.html"
visitedCountriesTemplateName :: Identifier
visitedCountriesTemplateName = "templates/map-development.html"

#else

archiveTemplateName :: Identifier
archiveTemplateName = "templates/archive.html"
defaultTemplateName :: Identifier
defaultTemplateName = "templates/default.html"
indexTemplateName :: Identifier
indexTemplateName = "templates/index.html"
listTemplateName :: Identifier
listTemplateName = "templates/list.html"
postTemplateName :: Identifier
postTemplateName = "templates/post.html"
routePlannerTemplateName :: Identifier
routePlannerTemplateName = "templates/route-planner.html"
tagsTemplateName :: Identifier
tagsTemplateName = "templates/tags.html"
visitedCountriesTemplateName :: Identifier
visitedCountriesTemplateName = "templates/map.html"

#endif

mainSiteDomain :: T.Text
mainSiteDomain = "http://ygpark2.github.io"

--------------------------------------------------------------------------------
-- Archive
--------------------------------------------------------------------------------

archiveRules :: Rules ()
archiveRules = do
    d <- makePatternDependency "posts/**/*.md"
    rulesExtraDependencies [d] $ do
        ids <- getMatches "posts/**/*.md"
        filteredIds <- filterM isPublished ids
        years <- mapM yearsMap filteredIds
        let ym = sortBy (\a b -> compare (fst b) (fst a)) $ yearsMap1 years
            firstYear = fst $ head ym
            fp year
                | year == firstYear = "archive/index.html"
                | otherwise = "archive/" ++ year ++ "/index.html"
            fp' year
                | year == firstYear = "/archive/"
                | otherwise = "/archive/" ++ year ++ "/"
        forM_ ym $ \(year, list) ->
            create [fromFilePath $ fp year] $ do
                route idRoute
                compile $ do
                    posts <- recentFirst =<< loadAllSnapshots (fromList list) "content"
                    months' <- mapM monthsMap posts
                    let yearCtx =
                            field "active" (\i -> if itemBody i == year then return "active" else fail "") `mappend`
                            field "href" (return . fp' . itemBody) `mappend`
                            bodyField "year"

                        mm = groupBy ((==) `on` fst) months'

                        postsList i = do
                            tpl <- loadBody "templates/parts/_post-archive.html"
                            str <- applyTemplateList tpl ctx items
                            item <- makeItem str
                                >>= loadAndApplyTemplate "templates/parts/_post-list-archive.html" postCtx
                            return $ itemBody item
                            where
                                items = map snd $ filter (\m -> fst m == itemBody i) months'
                                ctx = field "day" daysField `mappend` postCtx

                        monthsCtx =
                            field "posts" postsList `mappend`
                            {- listField "posts" postCtx postsList `mappend` -}
                            bodyField "month"

                        archiveCtx =
                            listField "years" yearCtx (mapM (makeItem . fst) ym) `mappend`
                            listField "months" monthsCtx (mapM (makeItem . fst . head) mm) `mappend`
                            pageCtx (defaultMetadata
                                { metaTitle = Just "Archive"
                                , metaDescription = "List of all stations for \"Quick Search\""
                                , metaUrl = "/archive/"
                                })
                    makeItem ""
                        >>= loadAndApplyTemplate archiveTemplateName archiveCtx
    where
        yearsMap i = do
            utc <- getItemUTC defaultTimeLocale i
            return (formatTime defaultTimeLocale "%Y" utc, [i])
        yearsMap1 = M.assocs . M.fromListWith (++)
        monthsMap i = do
            utc <- getItemUTC defaultTimeLocale $ itemIdentifier i
            return (formatTime timeLocale' "%B" utc, i)
        daysField i = do
            utc <- getItemUTC defaultTimeLocale $ itemIdentifier i
            return $ formatTime timeLocale' "%e" utc

isPublished :: (MonadMetadata m) => Identifier -> m Bool
isPublished identifier = do
    published <- getMetadataField identifier "published"
    return (published /= Just "false")

timeLocale' :: TimeLocale
timeLocale' = timeLocale
  { months =
    [ ("January", "Jan")
    , ("February", "Feb")
    , ("March", "Mar")
    , ("April", "Apr")
    , ("May", "May")
    , ("June", "June")
    , ("July", "July")
    , ("August", "Aug")
    , ("September", "Sept")
    , ("October", "Oct")
    , ("November", "Nov")
    , ("December", "Dec")
    ]
  }


--------------------------------------------------------------------------------
-- RSS feed
--------------------------------------------------------------------------------

feedPostCtx :: Context String
feedPostCtx =
    dateFieldWith defaultTimeLocale "pub-date" "%a, %d %b %Y %H:%M:%S GMT" `mappend`
    field "url" (return . identifierToUrl . toFilePath . itemIdentifier) `mappend`
    field "description" (return . escapeHtml . itemBody) `mappend`
    field "title" (\i -> do
      metadata <- getMetadata $ itemIdentifier i
      return $ escapeHtml $ maybe "" unwrap $ M.lookup "title" metadata) `mappend`
    defaultContext

feedRules :: Rules ()
feedRules =
    create ["feed.rss"] $ do
        route idRoute
        compile $ do
            ids <- getMatches "posts/**/*.md"
            filteredIds <- filterM isPublished ids
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots (fromList filteredIds) "rss"
            time <- unsafeCompiler getCurrentTime
            lastItemTime <- getItemUTC defaultTimeLocale $ itemIdentifier $ head posts
            let postsCtx =
                    listField "posts" feedPostCtx (return posts) `mappend`
                    constField "build-date" (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" time) `mappend`
                    constField "pub-date" (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" lastItemTime)
            makeItem ("" :: String)
                >>= loadAndApplyTemplate "templates/rss.xml" postsCtx

--------------------------------------------------------------------------------
-- Static files
--------------------------------------------------------------------------------

-- DO NOT merge patterns with files! It won't work!
staticFilesRules :: Rules ()
staticFilesRules = do
    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "demos/**" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList
        [ "robots.txt"
        , "yandex-widget-manifest.json"
        , "css/style.css"
        , "js/html5shiv.js"
        , "js/respond.min.js"
        , "map/data.json"
        ]) $ do
        route   idRoute
        compile copyFileCompiler

    match "favicons/**" $ do
        route (gsubRoute "favicons/" (const ""))
        compile copyFileCompiler


--------------------------------------------------------------------------------
-- Styles
--------------------------------------------------------------------------------

{-
env lessc --clean-css -O2 --include-path=less less/style.less css/style.css
-}

lessCompilerRules :: Rules ()
lessCompilerRules = do
    match "less/**" $
        compile getResourceBody

    d <- makePatternDependency "less/**"
    rulesExtraDependencies [d] $ create ["css/style.css"] $ do
        route idRoute
        compile $ loadBody "less/style.less"
            >>= makeItem
            >>= withItemBody
              (unixFilter "./node_modules/less/bin/lessc" ["--clean-css=advanced", "--include-path=less","-"])

    rulesExtraDependencies [d] $ create ["css/print.css"] $ do
        route idRoute
        compile $ loadBody "less/print.less"
            >>= makeItem
            >>= withItemBody
              (unixFilter "./node_modules/less/bin/lessc" ["--clean-css=advanced", "--include-path=less","-"])
              -- "lessc" ["-","--yui-compress","-O2"])

--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------

highlightLanguages :: [String]
highlightLanguages = ["bash", "css", "haskell", "javascript", "markdown", "sql", "xml", "dart"]

concatResources :: Identifier -> [Identifier] -> Rules ()
concatResources out inputs = do
    create [out] $ do
        route idRoute
        compile $ do
            res <- foldM (\buffer input -> do
                a <- loadBody input
                return $ buffer ++ a) ("" :: String) inputs
            makeItem $ res


scriptsCompilerRules :: Rules ()
scriptsCompilerRules = do
    match (fromList ["dart/packages/browser/dart.js", "js/d3/d3.min.js", "js/topojson/topojson.min.js",
        "js/polyhedron.js"]) $ compile getResourceBody

    -- Building highlight.js
    match "js/highlight.js/src/**" $
        compile copyFileCompiler

    highlightjs <- makePatternDependency "js/highlight.js/src/**"
    rulesExtraDependencies [highlightjs] $ create ["js/highlight.pack.js"] $
        compile $ do
            -- TODO logging
            js <- unsafeCompiler $ do
                (_, _, _, h) <- createProcess $ (proc "node" ("tools/build.js" : highlightLanguages)) {
                        cwd = Just "js/highlight.js"
                    }
                _ <- waitForProcess h
                readFile "js/highlight.js/build/highlight.pack.js"
            makeItem js

    -- Building additional js
    concatResources "dart/s.js" ["js/highlight.pack.js"]
    -- TODO sroute-planner
    concatResources "dart/smap.js" ["js/d3/d3.min.js", "js/polyhedron.js", "js/topojson/topojson.min.js"]

--------------------------------------------------------------------------------
-- Posts
--------------------------------------------------------------------------------

postsRules :: Rules ()
postsRules = do
    d <- makePatternDependency "collections/*.txt"
    rulesExtraDependencies [d] $ match "posts/**/*.md" $ do
        route removeExtension

        compile $ do
            identifier <- getUnderlying
            -- Comments
            thread <- getMetadataField identifier "thread"
            comments <- getComments thread

            -- Collection
            collectionName <- getMetadataField identifier "collection"
            collection <- getCollectionFile collectionName

            -- Metadata
            title <- getMetadataField identifier "title"
            tags <- getTags identifier
            description <- getMetadataField identifier "description"
            item <- pandocCompiler False >>= saveSnapshot "content"
            pandocCompiler True >>= saveSnapshot "rss"
            let images = map (fromMaybe "") $ filter isJust $ map imagesMap $ TS.parseTags $ itemBody item

            time <- getItemUTC defaultTimeLocale identifier
            loadAndApplyTemplate "templates/parts/_post.html"
                    (collectionField collection `mappend`
                    commentsField comments `mappend`
                    postCtx
                    ) item
                >>= loadAndApplyTemplate postTemplateName (postCtx `mappend` pageCtx (defaultMetadata
                    { metaTitle = fmap unwrap title
                    , metaUrl = '/' : identifierToUrl (toFilePath identifier)
                    , metaKeywords = tags
                    , metaDescription = maybe (cutDescription $ transformDescription $ escapeHtml $ TS.innerText $ TS.parseTags $
                        itemBody item) unwrap description
                    , metaType = FacebookArticle time tags images
                    }))



imagesMap :: Tag String -> Maybe String
imagesMap (TagOpen "img" attrs) = snd <$> find (\attr -> fst attr == "src") attrs
imagesMap _ = Nothing

--------------------------------------------------------------------------------
-- Comments
--------------------------------------------------------------------------------

commentsRules :: Rules ()
commentsRules =
    match "comments/*.html" $
        compile getResourceBody

getComments :: Maybe String -> Compiler [Item String]
getComments Nothing = return []
getComments (Just thread) = do
    ids <- getMatches "comments/*.html"
    filteredIds <- filterM compareThread ids
    loadAll (fromList filteredIds)
    where
        compareThread :: (MonadMetadata m) => Identifier -> m Bool
        compareThread identifier = do
            thread' <- getMetadataField identifier "thread"
            return (thread' == Just thread)


commentsField :: [Item String] -> Context String
commentsField items =
    field "comments" commentsList

    where
        ctx = bodyField "body" `mappend` metadataField

        commentsList _ = do
            tpl <- loadBody "templates/parts/_comment.html"
            str <- applyTemplateList tpl ctx items
            item <- makeItem str
                >>= loadAndApplyTemplate "templates/parts/_comments-list.html" postCtx
            return $ itemBody item


--------------------------------------------------------------------------------
-- Collections
--------------------------------------------------------------------------------

collectionRules :: Rules ()
collectionRules = do
    match "collections/*.txt" $ do
        compile getResourceBody

getCollectionFile :: Maybe String -> Compiler (Maybe String)
getCollectionFile Nothing = return Nothing
getCollectionFile (Just collection) = do
    item <- loadBody (fromFilePath $ "collections/" ++ (unwrap collection) ++ ".txt")
    return $ Just item


parseCollectionFile :: String -> (String, [Item String])
parseCollectionFile file = (title,
        map (\l -> Item (fromFilePath $ fst $ line l) $ (tail $ snd $ line l)) ls)
    where
        (title : ls) = lines file
        line = break (== '|')

collectionField :: Maybe String -> Context String
collectionField Nothing = mempty
collectionField (Just collection) =
    field "collection" collectionString
    where
        ctx = bodyField "title" `mappend` pathField "url"

        (title, items) = parseCollectionFile collection

        collectionString _ = do
            tpl <- loadBody "templates/parts/_collection-item.html"
            str <- applyTemplateList tpl ctx items
            item <- makeItem str
                >>= loadAndApplyTemplate "templates/parts/_collection.html" (bodyField "body" `mappend` constField "title" title)
            return $ itemBody item

--------------------------------------------------------------------------------
-- Index pages
--------------------------------------------------------------------------------

indexPagesRules :: Rules ()
indexPagesRules = do
    match "index.md" $
        compile $ pandocCompiler False

    paginate <- buildPaginateWith (\ids -> return $ paginateEvery 5 $ reverse ids) "posts/**/*.md" getPageIdentifier
    d <- makePatternDependency "posts/**/*.md"
    rulesExtraDependencies [d] $ paginateRules paginate $ \page ids -> do
        route addIndexRoute
        compile $ if page == 1
            then do
                posts <- recentFirst =<< loadAllSnapshots ids "content"
                topPost <- loadBody "index.md"
                let postsCtx =
                        constField "body" topPost `mappend`
                        listField "posts" postWithCommentsCountCtx (return posts) `mappend`
                        paginateContext paginate page `mappend`
                        pageCtx (defaultMetadata
                            { metaDescription = "My personal blog. "
                                ++ "I am talking about programming and sometimes about their lives."
                            })
                makeItem ""
                    >>= loadAndApplyTemplate indexTemplateName postsCtx
            else do
                posts <- recentFirst =<< loadAllSnapshots ids "content"
                let postsCtx =
                        listField "posts" postWithCommentsCountCtx (return posts) `mappend`
                        paginateContext paginate page `mappend`
                        pageCtx (defaultMetadata
                            { metaTitle = Just $ show page ++ "th page"
                            , metaDescription = "My personal blog, recording " ++ show ((page - 1) * 5 + 1)
                                ++ " по " ++ show (page * 5) ++ "."
                            , metaUrl = "/page/" ++ show page ++ "/"
                            })
                makeItem ""
                    >>= loadAndApplyTemplate listTemplateName postsCtx

--------------------------------------------------------------------------------
-- Tags
--------------------------------------------------------------------------------

tagsPagesRules :: Rules ()
tagsPagesRules = do
    metadata <- getAllMetadata "posts/**/*.md"
    let idents = fst $ unzip $ filter filterFn metadata
    tags <- buildTagsWith getTags (fromList idents) (\tag -> fromFilePath $ "tag/" ++ tag ++ "/index.html")
    d <- makePatternDependency "posts/**/*.md"
    rulesExtraDependencies [d] $ create ["tags/index.html"] $ do
        ids <- getMatches "posts/**/*.md"
        filteredIds <- filterM isPublished ids
        years <- mapM yearsMap filteredIds
        route idRoute
        compile $ do
            t <- renderTags
                (\tag _ count minCount maxCount ->
                    "<a href=\"/tag/" ++ tag ++ "/\" title=\"" ++ countText count "office" "Posts" "posts" ++
                    "\" class=\"weight-" ++ show (getWeight minCount maxCount count) ++ "\">" ++ tag ++ "</a>")
                unwords tags
            let ctx =
                    listField "years" yearCtx (mapM (makeItem . fst) ym) `mappend`
                    pageCtx (defaultMetadata
                        { metaTitle = Just "threads"
                        , metaDescription = "Full list of topics (tags) on the site"
                        , metaUrl = "/tags/"
                        })
                ym = sortBy (\a b -> compare (fst b) (fst a)) $ yearsMap1 years
                yearCtx =
                    field "href" (return . fp' . itemBody) `mappend`
                    bodyField "year"
                firstYear = fst $ head ym
                fp' year
                    | year == firstYear = "/archive/"
                    | otherwise = "/archive/" ++ year ++ "/"

            makeItem t
                >>= loadAndApplyTemplate "templates/parts/_tags-wrapper.html" ctx
                >>= loadAndApplyTemplate tagsTemplateName ctx

    rulesExtraDependencies [d] $ tagsRules tags $ \tag identifiers -> do
        paginate <- buildPaginateWith (\ids -> return $ paginateEvery 5 $ reverse ids) identifiers (getTagIdentifier tag)
        paginateRules paginate $ \page ids -> do
            route addIndexRoute
            compile $ do
                posts <- recentFirst =<< loadAllSnapshots ids "content"
                let postsCtx =
                        listField "posts" postWithCommentsCountCtx (return posts) `mappend`
                        paginateContext paginate page `mappend`
                        pageCtx (defaultMetadata
                            { metaTitle = Just $ "\"" ++ tag ++
                                (if page == 1 then "\""
                                    else "\", " ++ show page ++ "-th page")
                            , metaDescription = "My personal blog entries tagged \"" ++ tag ++
                                (if page == 1 then "\"."
                                    else "\" с " ++ show ((page - 1) * 5 + 1) ++ " по " ++ show (page * 5) ++ ".")
                            , metaUrl = "/tag/" ++ tag ++
                                (if page == 1 then "/"
                                    else "/page/" ++ show page ++ "/")
                            })

                makeItem ""
                    >>= loadAndApplyTemplate listTemplateName postsCtx
    where
        filterFn :: (a, Metadata) -> Bool
        filterFn (_, metadata)
            | M.lookup "published" metadata == Just "false" = False
            | otherwise = True
        yearsMap i = do
            utc <- getItemUTC defaultTimeLocale i
            return (formatTime defaultTimeLocale "%Y" utc, [i])
        yearsMap1 = M.assocs . M.fromListWith (++)



getTags :: MonadMetadata m => Identifier -> m [String]
getTags identifier = do
    metadata <- getMetadata identifier
    return $ maybe [] (map trim . splitAll "," . unwrap) $ M.lookup "tags" metadata

--------------------------------------------------------------------------------
-- Static pages
--------------------------------------------------------------------------------

staticPagesRules :: Rules ()
staticPagesRules = do
    match "route-planner/index.html" $ do
        route idRoute
        compile $
            getResourceBody
                >>= loadAndApplyTemplate "templates/parts/_post-without-footer.html" postCtx
                >>= loadAndApplyTemplate routePlannerTemplateName (pageCtx (defaultMetadata
                    { metaTitle = Just "Route Planner"
                    , metaDescription = "The calculation of the optimal route of travel by city"
                    , metaUrl = "/route-planner/"
                    }))

    match "map/index.html" $ do
            route idRoute
            compile $
                getResourceBody
                    >>= loadAndApplyTemplate visitedCountriesTemplateName (pageCtx (defaultMetadata
                        { metaTitle = Just "Map of countries"
                        , metaDescription = "Map of visited countries and cities"
                        , metaUrl = "/map/"
                        }))


    match (fromList ["resume.md", "about.md", "projects.md", "404.md"]) $ do
        route removeExtension
        compile $ do
            identifier <- getUnderlying
            title <- getMetadataField identifier "title"
            description <- getMetadataField identifier "description"
            pandocCompiler False
                >>= loadAndApplyTemplate "templates/parts/_post-without-footer.html" postCtx
                >>= loadAndApplyTemplate defaultTemplateName (pageCtx (defaultMetadata
                    { metaTitle = fmap unwrap title
                    , metaDescription = unwrap $ fromMaybe "" description
                    , metaUrl = '/' : identifierToUrl (toFilePath identifier)
                    }))
--------------------------------------------------------------------------------
-- Sitemap
--------------------------------------------------------------------------------

data SitemapItem = SitemapItem
    { siUrl :: String
    , siPriority :: String
    }

sitemapRules :: Rules ()
sitemapRules = do
    d <- makePatternDependency "posts/**/*.md"
    rulesExtraDependencies [d] $ create ["sitemap.xml"] $ do
        route idRoute

        ids <- getMatches "posts/**/*.md"
        let
            postItems = map (\i -> SitemapItem ("http://ygpark2.github.io/" ++ identifierToUrl (toFilePath i)) "1.0") ids
        compile $
            makeItem ""
                 >>= loadAndApplyTemplate "templates/sitemap.xml" (sitemapField (staticItems ++ postItems))
    where
        staticItems =
            [ SitemapItem "http://ygpark2.github.io/" "0.5"
            , SitemapItem "http://ygpark2.github.io/about/" "0.8"
            ]

sitemapField :: [SitemapItem] -> Context String
sitemapField items =
    constField "sitemap" $ concatMap sitemap items
    where
        sitemap (SitemapItem url priority) = "<url><loc>" ++ url ++
            "</loc><changefreq>daily</changefreq><priority>" ++ priority ++ "</priority></url>\n"


--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------

timeLocale :: TimeLocale
timeLocale = defaultTimeLocale
  { wDays =
    [ ("Sunday", "Sun")
    , ("Monday", "Mon")
    , ("Tuesday", "Tue")
    , ("Wednesday", "Wed")
    , ("Thursday", "Thu")
    , ("Friday", "Fri")
    , ("Saturday", "Sat")
    ]
  , months =
    [ ("January", "Jan")
    , ("February", "Feb")
    , ("March", "Mar")
    , ("April", "Apr")
    , ("May", "May")
    , ("June", "June")
    , ("July", "July")
    , ("August", "Aug")
    , ("September", "Sept")
    , ("October", "Oct")
    , ("November", "Nov")
    , ("December", "Dec")
    ]
  }

tagsContext :: Context a
tagsContext = field "tags" convertTags
    where
        convertTags item = do
            tags <- getTags $ itemIdentifier item
            return $ concatMap (\tag -> "<a href=\"/tag/" ++ tag ++ "/\" class=\"label label-default\">" ++ tag ++ "</a> ") tags

postCtx :: Context String
postCtx =
    dateFieldWith timeLocale "date" "%A, %e %B %Y, %R" `mappend`
    dateFieldWith defaultTimeLocale "post-date" "%Y-%m-%dT%H:%M:%S%z" `mappend`
    field "url" (return . identifierToUrl . toFilePath . itemIdentifier) `mappend`
    field "disqus" (return . identifierToDisqus . toFilePath . itemIdentifier) `mappend`
    field "title" (\i -> do
      metadata <- getMetadata $ itemIdentifier i
      return $ escapeHtml $ maybe "" unwrap $ M.lookup "title" metadata) `mappend`
    tagsContext `mappend`
    defaultContext

postWithCommentsCountCtx :: Context String
postWithCommentsCountCtx =
    constField "commentsCount" "" `mappend`
    teaserField "teaser" "readmore" "content" `mappend`
    postCtx

pageCtx :: PageMetadata -> Context String
pageCtx (PageMetadata title url description keywords fType)=
    constField "meta.title" (escapeHtml $ metaTitle' title) `mappend`
    constField "meta.url" (escapeHtml $ "http://ygpark2.github.io/" ++ url) `mappend`
    constField "meta.description" (escapeHtml description) `mappend`
    constField "meta.keywords" (escapeHtml $ intercalate ", " keywords) `mappend`
    constField "meta.dc.subject" (escapeHtml $ intercalate "; " keywords) `mappend`
    facebookFields fType `mappend`
    defaultContext
    where
        metaTitle' Nothing = "[Young Gyu's blog]"
        metaTitle' (Just title') = title' ++ " :: [Young Gyu's blog]"

        facebookFields (FacebookArticle published keywords' images) =
                constField "meta.facebook.article" "" `mappend`
                constField "meta.facebook.published" (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" published) `mappend`
                listField "meta.facebook.tags" defaultContext (mapM makeItem keywords') `mappend`
                listField "meta.facebook.images" defaultContext (mapM makeItem images)
        -- TODO Facebook profile
        facebookFields _ = constField "meta.facebook.nothing" ""

--------------------------------------------------------------------------------
-- Html metadata
--------------------------------------------------------------------------------

data FacebookType = FacebookBlog
    | FacebookArticle UTCTime [String] [String] -- Published, keywords, images
    | FacebookProfile
    | FacebookNothing

data PageMetadata = PageMetadata
    { metaTitle :: Maybe String
    , metaUrl :: String
    , metaDescription :: String
    , metaKeywords :: [String]
    , metaType :: FacebookType
    }

defaultMetadata :: PageMetadata
defaultMetadata = PageMetadata
    { metaTitle = Nothing
    , metaUrl = "/"
    , metaDescription = ""
    , metaKeywords = ["Blog", "Ain", "Siyul", "Sunju"]
    , metaType = FacebookNothing
    }


--------------------------------------------------------------------------------
-- Replacement of Hakyll functions
--------------------------------------------------------------------------------


paginateNumPages :: Paginate -> Int
paginateNumPages = M.size . paginateMap


paginatePage :: Paginate -> PageNumber -> Maybe Identifier
paginatePage pag pageNumber
    | pageNumber < 1                      = Nothing
    | pageNumber > (paginateNumPages pag) = Nothing
    | otherwise                           = Just $ paginateMakeId pag pageNumber


paginateContext :: Paginate -> PageNumber -> Context a
paginateContext pag currentPage = mconcat
    [ field "firstPageNum"    $ \_ -> otherPage 1                 >>= num
    , field "firstPageUrl"    $ \_ -> otherPage 1                 >>= url
    , field "previousPageNum" $ \_ -> otherPage (currentPage - 1) >>= num
    , field "previousPageUrl" $ \_ -> otherPage (currentPage - 1) >>= url
    , field "nextPageNum"     $ \_ -> otherPage (currentPage + 1) >>= num
    , field "nextPageUrl"     $ \_ -> otherPage (currentPage + 1) >>= url
    , field "lastPageNum"     $ \_ -> otherPage lastPage          >>= num
    , field "lastPageUrl"     $ \_ -> otherPage lastPage          >>= url
    , field "currentPageNum"  $ \i -> thisPage i                  >>= num
    , field "currentPageUrl"  $ \i -> thisPage i                  >>= url
    , constField "numPages"   $ show $ paginateNumPages pag
    ]
  where
    lastPage = paginateNumPages pag

    thisPage i = return (currentPage, itemIdentifier i)
    otherPage n
        | n == currentPage = fail $ "This is the current page: " ++ show n
        | otherwise        = case paginatePage pag n of
            Nothing -> fail $ "No such page: " ++ show n
            Just i  -> return (n, i)

    num :: (Int, Identifier) -> Compiler String
    num = return . show . fst

    url :: (Int, Identifier) -> Compiler String
    url (n, i) = getRoute i >>= \mbR -> case mbR of
        Just r  -> return $ simplifiedUrl ('/' : r)
        Nothing -> fail $ "No URL for page: " ++ show n


getItemUTC :: MonadMetadata m
           => TimeLocale        -- ^ Output time locale
           -> Identifier        -- ^ Input page
           -> m UTCTime         -- ^ Parsed UTCTime
getItemUTC locale id' = do
    metadata <- getMetadata id'
    let tryField k fmt = fmap unwrap (M.lookup k metadata) >>= parseTime' fmt
        fn             = takeFileName $ toFilePath id'

    maybe empty' return $ msum $
        [tryField "published" fmt | fmt <- formats] ++
        [tryField "date"      fmt | fmt <- formats] ++
        [parseTime' "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn]
  where
    empty'     = fail $ "Hakyll.Web.Template.Context.getItemUTC: " ++
        "could not parse time for " ++ show id'
    parseTime' = parseTime locale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%dT%H:%M%Z"
        , "%Y-%m-%d %H:%M%Z"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
        ]


dateFieldWith :: TimeLocale  -- ^ Output time locale
              -> String      -- ^ Destination key
              -> String      -- ^ Format to use on the date
              -> Context a   -- ^ Resulting context
dateFieldWith locale key format = field key $ \i -> do
    time <- getItemUTC locale $ itemIdentifier i
    return $ formatTime locale format time


pandocCompiler :: Bool -> Compiler (Item String)
pandocCompiler rss = do
    post <- getResourceBody
    makeItem $ T.unpack $ T.decodeUtf8 $ toByteString $ renderHtmlFragment UTF8 $ writeXmlHtml defaultXmlHtmlWriterOptions
        { idPrefix = "" --postUrl post
        , renderForRSS = rss
        , siteDomain = mainSiteDomain
        , debugOutput = False
        }
        (readMarkdown readerOptions $ itemBody post)


readerOptions :: ReaderOptions
readerOptions = def
  { readerSmart = True
  , readerParseRaw = True
  }

--------------------------------------------------------------------------------
teaserSeparatorStart :: String
teaserSeparatorStart = "<!--more"

teaserSeparatorEnd :: String
teaserSeparatorEnd = "-->"

--------------------------------------------------------------------------------
-- | A context with "teaser" key which contain a teaser of the item.
-- The item is loaded from the given snapshot (which should be saved
-- in the user code before any templates are applied).
teaserField :: String           -- ^ Key to use
            -> String           -- ^ Read more text field
            -> Snapshot         -- ^ Snapshot to load
            -> Context String   -- ^ Resulting context
teaserField key readMoreKey snapshot =
    field key teaser `mappend`
    field readMoreKey readMore
    where
        teaser item = do
            body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
            case findTeaser body of
                Nothing -> fail $
                    "Hakyll.Web.Template.Context: no teaser defined for " ++
                    show (itemIdentifier item)
                Just (t, _) -> return t
        readMore item = do
            body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
            case findTeaser body of
                Nothing -> fail $
                    "Hakyll.Web.Template.Context: no teaser defined for " ++
                    show (itemIdentifier item)
                Just (_, Nothing) -> fail $
                    "Hakyll.Web.Template.Context: no readmore defined for " ++
                    show (itemIdentifier item)
                Just (_, Just t) -> return t


findTeaser :: String -> Maybe (String, Maybe String) -- Teaser, optional custom readmore text
findTeaser = go []
    where
       go _ [] = Nothing
       go acc xss@(x:xs)
           | teaserSeparatorStart `isPrefixOf` xss = Just (reverse acc, go2 [] $ drop (length teaserSeparatorStart) xss )
           | otherwise                             = go (x : acc) xs

       go2 _ [] = Nothing
       go2 acc xss@(x:xs)
           | teaserSeparatorEnd `isPrefixOf` xss =
                if trim' acc /= [] then Just $ reverse $ trim' acc
                else Nothing
           | otherwise                           = go2 (x : acc) xs
       trim' str = dropWhileEnd isSpace $ dropWhile isSpace str

--------------------------------------------------------------------------------
-- | Sort pages chronologically. Uses the same method as 'dateField' for
-- extracting the date.
chronological :: MonadMetadata m => [Item a] -> m [Item a]
chronological =
    sortByM $ getItemUTC defaultTimeLocale . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

--------------------------------------------------------------------------------
-- | The reverse of 'chronological'
recentFirst :: (MonadMetadata m, Functor m) => [Item a] -> m [Item a]
recentFirst = fmap reverse . chronological

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

unwrap :: String -> String
unwrap str -- TODO decode escaped chars
    | str == "\"" || str == "" = str
    | head str == '"' && last str == '"' = tail $ init str
    | otherwise = str

-- Replace newlines with spaces
transformDescription :: String -> String
transformDescription = map (\ch -> if ch == '\n' then ' ' else ch)

-- Cut long descriptions
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
        else toFilePath id' ++ "/index.html")

-- | Transforms 'something/something.md' into 'something/something/index.html'
-- and 'something/YYYY-MM-DD-something.md' into 'something/something/index.html'
removeExtension :: Routes
removeExtension = customRoute $ removeExtension' . toFilePath

removeExtension' :: String -> String
removeExtension' filepath = subRegex (mkRegex "^(.*)\\.md$")
                                        (subRegex (mkRegex "/[0-9]{4}-[0-9]{2}-[0-9]{2}-(.*)\\.md$") filepath "/\\1/index.html")
                                        "\\1/index.html"

identifierToUrl :: String -> String
identifierToUrl filepath = subRegex (mkRegex "^(.*)\\.md$")
                                        (subRegex (mkRegex "/[0-9]{4}-[0-9]{2}-[0-9]{2}-(.*)\\.md$") filepath "/\\1/")
                                        "\\1/"

simplifiedUrl :: String -> String
simplifiedUrl url = subRegex (mkRegex "/index\\.html$") url "/"

identifierToDisqus :: String -> String
identifierToDisqus filepath = subRegex (mkRegex "^post/[0-9]{4}-[0-9]{2}-[0-9]{2}-(.*)\\.md$") filepath  "\\1"

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
getWeight minCount maxCount count =
    round ((5 * ((fromIntegral count :: Double) - fromIntegral minCount) +
        fromIntegral maxCount - fromIntegral minCount) /
        (fromIntegral maxCount - fromIntegral minCount))

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e