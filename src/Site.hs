{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RecordWildCards   #-}
import           Blaze.ByteString.Builder (toByteString)
import           Control.Monad (forM_, filterM, foldM, when)
import           Data.Function (on)
import           Data.List (sortBy, intercalate, find, groupBy, nub)
import qualified Data.Map as M
import qualified Data.Aeson.KeyMap as KM
import           Data.Aeson (Value(..))
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Format
import           Hakyll hiding (chronological, dateFieldWith, getItemUTC, getTags, paginateContext,
                    pandocCompiler, recentFirst, teaserField)
import           System.Directory ()
import           System.IO (hPutStrLn, stderr)
import           System.Process
import           Text.HTML.TagSoup (Tag(..))
import qualified Text.HTML.TagSoup as TS
import           Text.Pandoc hiding (getCurrentTime)
import           Text.XmlHtml
import           XmlHtmlWriter

import           Site.Compat (dateFieldWith, getItemUTC, paginateContext, recentFirst)
import           Site.Context (FacebookType(..), PageMetadata(..), defaultMetadata, getTags,
                    isPublished, pageCtx, postCtx, postWithCommentsCountCtx, timeLocale)
import           Site.Theme (themePart, themeTemplate)
import           Site.Utils (addIndexRoute, countText, cutDescription, escapeJson, getPageIdentifier,
                    getSettingInt, getTagIdentifier, getWeight, identifierToUrl, jsonRoute, normalizeSpaces,
                    normalizeUrlPath, removeExtension, transformDescription,
                    truncateWithEllipsis, unwrap)


main :: IO ()
main = hakyll $ do
    staticFilesRules
    commentsRules
    collectionRules
    settingsDep <- makePatternDependency "settings.yml"
    rulesExtraDependencies [settingsDep] $ do
        postsRules
        postsJsonRules
        tagsPagesRules
        archiveRules
        indexPagesRules
        staticPagesRules
        feedRules
        sitemapRules

    match "templates/**" $ compile templateCompiler

postExcerptLengthIndex :: Int
postExcerptLengthIndex = getSettingInt "postExcerptLengthIndex" 300

postExcerptLengthList :: Int
postExcerptLengthList = getSettingInt "postExcerptLengthList" 300

postListCtx :: Context String
postListCtx =
    field "teaser" (\item -> do
        body <- itemBody <$> loadSnapshot (itemIdentifier item) "content"
        let plain = normalizeSpaces $ TS.innerText $ TS.parseTags body
            teaser = truncateWithEllipsis postExcerptLengthList plain
        return $ "<p>" ++ escapeHtml teaser ++ "</p>")
    `mappend` constField "readmore" "Read more ..."
    `mappend` postWithCommentsCountCtx

postIndexCtx :: Context String
postIndexCtx =
    field "teaser" (\item -> do
        body <- itemBody <$> loadSnapshot (itemIdentifier item) "content"
        let plain = normalizeSpaces $ TS.innerText $ TS.parseTags body
            teaser = truncateWithEllipsis postExcerptLengthIndex plain
        return $ "<p>" ++ escapeHtml teaser ++ "</p>")
    `mappend` constField "readmore" "Read more ..."
    `mappend` postWithCommentsCountCtx


#ifdef DEVELOPMENT

archiveTemplateName :: Identifier
archiveTemplateName = themeTemplate "archive.html"
defaultTemplateName :: Identifier
defaultTemplateName = themeTemplate "default.html"
indexTemplateName :: Identifier
indexTemplateName = themeTemplate "index.html"
listTemplateName :: Identifier
listTemplateName = themeTemplate "list.html"
postTemplateName :: Identifier
postTemplateName = themeTemplate "post.html"
routePlannerTemplateName :: Identifier
routePlannerTemplateName = themeTemplate "route-planner.html"
tagsTemplateName :: Identifier
tagsTemplateName = themeTemplate "tags.html"
visitedCountriesTemplateName :: Identifier
visitedCountriesTemplateName = themeTemplate "map.html"

#else

archiveTemplateName :: Identifier
archiveTemplateName = themeTemplate "archive.html"
defaultTemplateName :: Identifier
defaultTemplateName = themeTemplate "default.html"
indexTemplateName :: Identifier
indexTemplateName = themeTemplate "index.html"
listTemplateName :: Identifier
listTemplateName = themeTemplate "list.html"
postTemplateName :: Identifier
postTemplateName = themeTemplate "post.html"
routePlannerTemplateName :: Identifier
routePlannerTemplateName = themeTemplate "route-planner.html"
tagsTemplateName :: Identifier
tagsTemplateName = themeTemplate "tags.html"
visitedCountriesTemplateName :: Identifier
visitedCountriesTemplateName = themeTemplate "map.html"

#endif

mainSiteDomain :: T.Text
mainSiteDomain = "http://ygpark2.github.io"


archiveRules :: Rules ()
archiveRules = do
    d <- makePatternDependency "posts/**"
    rulesExtraDependencies [d] $ do
        ids <- getMatches "posts/**"
        filteredIds <- filterM isPublished ids
        years <- mapM yearsMap filteredIds
        let ym = sortBy (\a b -> compare (fst b) (fst a)) $ yearsMap1 years
        case ym of
            [] -> return ()
            (firstYear, _) : _ -> do
                let fp year
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
                                    tpl <- loadBody (themePart "_post-archive.html")
                                    str <- applyTemplateList tpl ctx items
                                    item <- makeItem str
                                        >>= loadAndApplyTemplate (themePart "_post-list-archive.html") postCtx
                                    return $ itemBody item
                                    where
                                        items = map snd $ filter (\m -> fst m == itemBody i) months'
                                        ctx = field "day" daysField `mappend` postCtx

                                monthsCtx =
                                    field "posts" postsList `mappend`
                                    {- listField "posts" postCtx postsList `mappend` -}
                                    bodyField "month"

                                monthKey ((m, _) : _) = Just m
                                monthKey [] = Nothing

                                archiveCtx =
                                    listField "years" yearCtx (mapM (makeItem . fst) ym) `mappend`
                                    listField "months" monthsCtx (mapM makeItem (mapMaybe monthKey mm)) `mappend`
                                    pageCtx (defaultMetadata
                                        { metaTitle = Just "Archive"
                                        , metaDescription = "All posts in one place for quick browsing."
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



feedPostCtx :: Context String
feedPostCtx =
    dateFieldWith defaultTimeLocale "pub-date" "%a, %d %b %Y %H:%M:%S GMT" `mappend`
    field "url" (return . identifierToUrl . toFilePath . itemIdentifier) `mappend`
    field "description" (return . escapeHtml . itemBody) `mappend`
    field "title" (\i -> do
      title <- getMetadataField (itemIdentifier i) "title"
      return $ escapeHtml $ maybe "" unwrap title) `mappend`
    defaultContext

feedRules :: Rules ()
feedRules =
    create ["feed.rss"] $ do
        route idRoute
        compile $ do
            ids <- getMatches "posts/**"
            let baseIds = nub $ map (setVersion Nothing) ids
            filteredIds <- filterM isPublished baseIds
            posts <- fmap (take 10) . recentFirst =<<
                mapM (\i -> loadSnapshot i "rss") filteredIds
            time <- unsafeCompiler getCurrentTime
            lastItemTime <- case posts of
                (post : _) -> getItemUTC defaultTimeLocale $ itemIdentifier post
                [] -> return time
            let postsCtx =
                    listField "posts" feedPostCtx (return posts) `mappend`
                    constField "build-date" (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" time) `mappend`
                    constField "pub-date" (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" lastItemTime)
            makeItem ("" :: String)
                >>= loadAndApplyTemplate (themeTemplate "rss.xml") postsCtx


staticFilesRules :: Rules ()
staticFilesRules = do
    match "assets/fonts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/js/**" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList [ "robots.txt" ]) $ do
        route   idRoute
        compile copyFileCompiler

    match "favicons/**" $ do
        route (gsubRoute "favicons/" (const ""))
        compile copyFileCompiler



postsRules :: Rules ()
postsRules = do
    d <- makePatternDependency "collections/*.txt"
    rulesExtraDependencies [d] $ match "posts/**" $ do
        route removeExtension

        compile $ do
            identifier <- getUnderlying
            thread <- getMetadataField identifier "thread"
            comments <- getComments thread

            collectionName <- getMetadataField identifier "collection"
            collection <- getCollectionFile collectionName

            title <- getMetadataField identifier "title"
            tags <- getTags identifier
            description <- getMetadataField identifier "description"
            item <- pandocCompiler False >>= saveSnapshot "content"
            pandocCompiler True >>= saveSnapshot "rss"
            let images = map (fromMaybe "") $ filter isJust $ map imagesMap $ TS.parseTags $ itemBody item

            time <- getItemUTC defaultTimeLocale identifier
            loadAndApplyTemplate (themePart "_post.html")
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


postsJsonRules :: Rules ()
postsJsonRules = do
    match "posts/**" $ version "json" $ do
        route jsonRoute
        compile $ do
            identifier <- getUnderlying
            let originalId = setVersion Nothing identifier
            meta <- buildPostJson originalId
            makeItem $ renderPostJson meta

    d <- makePatternDependency "posts/**"
    rulesExtraDependencies [d] $ match "assets/data/posts-index.json" $ do
        route idRoute
        compile $ do
            ids <- getMatches "posts/**"
            let baseIds = filter (isNothing . identifierVersion) ids
            filteredIds <- filterM isPublished baseIds
            metas <- mapM buildPostJson filteredIds
            time <- unsafeCompiler getCurrentTime
            let payload = renderPostsIndexJson time metas
                payloadSize = length payload
                fiveMb = 5 * 1024 * 1024
            when (payloadSize > fiveMb) $
                unsafeCompiler $ hPutStrLn stderr "Warning: posts-index.json exceeds 5MB budget"
            makeItem payload

    rulesExtraDependencies [d] $ match "assets/data/visited.geojson" $ do
        route idRoute
        compile $ do
            ids <- getMatches "posts/**"
            filteredIds <- filterM isPublished ids
            features <- catMaybes <$> mapM buildVisitedFeature filteredIds
            let payload = renderVisitedGeoJson features
            makeItem payload

data PostJson = PostJson
    { postJsonTitle       :: String
    , postJsonUrl         :: String
    , postJsonDate        :: String
    , postJsonTags        :: [String]
    , postJsonDescription :: String
    , postJsonExcerpt     :: String
    , postJsonHtml        :: String
    , postJsonPlain       :: String
    }

data VisitedFeature = VisitedFeature
    { vfTitle    :: String
    , vfLocation :: String
    , vfUrl      :: String
    , vfVisited  :: String
    , vfLat      :: Double
    , vfLng      :: Double
    }

buildPostJson :: Identifier -> Compiler PostJson
buildPostJson identifier = do
    title <- getMetadataField identifier "title"
    description <- getMetadataField identifier "description"
    tags <- getTags identifier
    time <- getItemUTC defaultTimeLocale identifier
    raw <- getResourceBody
    doc <- either (fail . show) return $
        runPure $ readMarkdown readerOptions (T.pack $ itemBody raw)
    let htmlText = T.decodeUtf8 $ toByteString $ renderHtmlFragment UTF8 $ writeXmlHtml defaultXmlHtmlWriterOptions
            { idPrefix = ""
            , renderForRSS = False
            , siteDomain = mainSiteDomain
            , debugOutput = False
            } doc
        html = T.unpack htmlText
        title' = maybe "" unwrap title
        url = normalizeUrlPath $ identifierToUrl (toFilePath identifier)
        description' = maybe (cutDescription $ transformDescription $ escapeHtml $ TS.innerText $ TS.parseTags html) unwrap description
        plain = normalizeSpaces $ TS.innerText $ TS.parseTags html
        excerpt = truncateWithEllipsis 240 plain
        isoDate = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time
    return PostJson
        { postJsonTitle = title'
        , postJsonUrl = url
        , postJsonDate = isoDate
        , postJsonTags = tags
        , postJsonDescription = description'
        , postJsonExcerpt = excerpt
        , postJsonHtml = html
        , postJsonPlain = plain
        }

renderPostJson :: PostJson -> String
renderPostJson PostJson{..} = "{" ++ intercalate "," fields ++ "}"
    where
        fields =
            [ kv "title" (renderString postJsonTitle)
            , kv "url" (renderString postJsonUrl)
            , kv "date" (renderString postJsonDate)
            , kv "tags" (renderArray postJsonTags)
            , kv "description" (renderString postJsonDescription)
            , kv "excerpt" (renderString postJsonExcerpt)
            , kv "html" (renderString postJsonHtml)
            , kv "plain" (renderString postJsonPlain)
            ]
        kv k v = renderString k ++ ":" ++ v
        renderArray xs = "[" ++ intercalate "," (map renderString xs) ++ "]"
        renderString s = "\"" ++ escapeJson s ++ "\""

renderPostsIndexJson :: UTCTime -> [PostJson] -> String
renderPostsIndexJson time metas =
    "{"
    ++ "\"generatedAt\":\"" ++ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time ++ "\","
    ++ "\"total\":" ++ show (length metas) ++ ","
    ++ "\"posts\":[" ++ intercalate "," (map renderPostIndex metas) ++ "]"
    ++ "}"
    where
        renderPostIndex PostJson{..} =
            "{" ++ intercalate ","
                [ kv "title" (renderString postJsonTitle)
                , kv "url" (renderString postJsonUrl)
                , kv "date" (renderString postJsonDate)
                , kv "tags" (renderArray postJsonTags)
                , kv "description" (renderString postJsonDescription)
                , kv "excerpt" (renderString postJsonExcerpt)
                ]
            ++ "}"
        kv k v = renderString k ++ ":" ++ v
        renderArray xs = "[" ++ intercalate "," (map renderString xs) ++ "]"
        renderString s = "\"" ++ escapeJson s ++ "\""

buildVisitedFeature :: Identifier -> Compiler (Maybe VisitedFeature)
buildVisitedFeature identifier = do
    latStr <- getMetadataField identifier "lat"
    lngStr <- getMetadataField identifier "lng"
    lonStr <- getMetadataField identifier "lon"
    let lngField = case lngStr of
            Just _ -> lngStr
            Nothing -> lonStr
    case (latStr >>= parseDouble, lngField >>= parseDouble) of
        (Just lat, Just lng) -> do
            title <- getMetadataField identifier "title"
            location <- getMetadataField identifier "location"
            time <- getItemUTC defaultTimeLocale identifier
            let title' = maybe "" unwrap title
                location' = maybe title' unwrap location
                url = normalizeUrlPath $ identifierToUrl (toFilePath identifier)
                visited = formatTime defaultTimeLocale "%Y-%m-%d" time
            return $ Just VisitedFeature
                { vfTitle = title'
                , vfLocation = location'
                , vfUrl = url
                , vfVisited = visited
                , vfLat = lat
                , vfLng = lng
                }
        _ -> return Nothing
  where
    parseDouble :: String -> Maybe Double
    parseDouble s = case reads s of
        [(num, "")] -> Just num
        _ -> Nothing

renderVisitedGeoJson :: [VisitedFeature] -> String
renderVisitedGeoJson features =
    "{"
    ++ "\"type\":\"FeatureCollection\","
    ++ "\"features\":[" ++ intercalate "," (map renderFeature features) ++ "]"
    ++ "}"
  where
    renderFeature VisitedFeature{..} =
        "{" ++ intercalate ","
            [ "\"type\":\"Feature\""
            , "\"properties\":{" ++ intercalate ","
                [ kv "title" vfTitle
                , kv "location" vfLocation
                , kv "url" vfUrl
                , kv "visited" vfVisited
                ] ++ "}"
            , "\"geometry\":{" ++ intercalate ","
                [ "\"type\":\"Point\""
                , "\"coordinates\":[" ++ show vfLng ++ "," ++ show vfLat ++ "]"
                ] ++ "}"
            ] ++ "}"
    kv k v = "\"" ++ k ++ "\":\"" ++ escapeJson v ++ "\""


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
            tpl <- loadBody (themePart "_comment.html")
            str <- applyTemplateList tpl ctx items
            item <- makeItem str
                >>= loadAndApplyTemplate (themePart "_comments-list.html") postCtx
            return $ itemBody item



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
parseCollectionFile file =
    case lines file of
        [] -> ("", [])
        title : ls -> (title, mapMaybe parseLine ls)
    where
        parseLine l =
            case break (== '|') l of
                (path, '|' : rest) ->
                    Just $ Item (fromFilePath path) rest
                _ -> Nothing

collectionField :: Maybe String -> Context String
collectionField Nothing = mempty
collectionField (Just collection) =
    field "collection" collectionString
    where
        ctx = bodyField "title" `mappend` pathField "url"

        (title, items) = parseCollectionFile collection

        collectionString _ = do
            tpl <- loadBody (themePart "_collection-item.html")
            str <- applyTemplateList tpl ctx items
            item <- makeItem str
                >>= loadAndApplyTemplate (themePart "_collection.html") (bodyField "body" `mappend` constField "title" title)
            return $ itemBody item


indexPagesRules :: Rules ()
indexPagesRules = do
    match "index.md" $
        compile $ pandocCompiler False

    paginate <- buildPaginateWith (\ids -> return $ paginateEvery 5 $ reverse ids) "posts/**" getPageIdentifier
    d <- makePatternDependency "posts/**"
    rulesExtraDependencies [d] $ paginateRules paginate $ \page ids -> do
        route addIndexRoute
        compile $ if page == 1
            then do
                posts <- recentFirst =<< loadAllSnapshots ids "content"
                topPost <- loadBody "index.md"
                let postsCtx =
                        constField "body" topPost `mappend`
                        listField "posts" postIndexCtx (return posts) `mappend`
                        paginateContext paginate page `mappend`
                        pageCtx (defaultMetadata
                            { metaDescription = "My personal blog. "
                                ++ "I write about programming and sometimes about my life."
                            })
                makeItem ""
                    >>= loadAndApplyTemplate indexTemplateName postsCtx
            else do
                posts <- recentFirst =<< loadAllSnapshots ids "content"
                let postsCtx =
                        listField "posts" postListCtx (return posts) `mappend`
                        paginateContext paginate page `mappend`
                        pageCtx (defaultMetadata
                            { metaTitle = Just $ "Page " ++ show page
                            , metaDescription = "My personal blog, posts " ++ show ((page - 1) * 5 + 1)
                                ++ " to " ++ show (page * 5) ++ "."
                            , metaUrl = "/page/" ++ show page ++ "/"
                            })
                makeItem ""
                    >>= loadAndApplyTemplate listTemplateName postsCtx


tagsPagesRules :: Rules ()
tagsPagesRules = do
    metadata <- getAllMetadata "posts/**"
    let idents = fst $ unzip $ filter filterFn metadata
    tags <- buildTagsWith getTags (fromList idents) (\tag -> fromFilePath $ "tag/" ++ tag ++ "/index.html")
    d <- makePatternDependency "posts/**"
    rulesExtraDependencies [d] $ create ["tags/index.html"] $ do
        ids <- getMatches "posts/**"
        filteredIds <- filterM isPublished ids
        years <- mapM yearsMap filteredIds
        route idRoute
        compile $ do
            t <- renderTags
                (\tag _ count minCount maxCount ->
                    let weight = getWeight minCount maxCount count
                        fontPx = 12 + (weight * 3) -- linear scale: 12px..27px
                    in "<a href=\"/tag/" ++ tag ++ "/\" title=\"" ++ countText count "post" "posts" "posts" ++
                       "\" class=\"weight-" ++ show weight ++ "\" style=\"font-size:" ++ show fontPx ++ "px\">" ++ tag ++ "</a>")
                unwords tags
            let ctx =
                    listField "years" yearCtx (mapM (makeItem . fst) ym) `mappend`
                    pageCtx (defaultMetadata
                        { metaTitle = Just "Tags"
                        , metaDescription = "Full list of topics (tags) on the site"
                        , metaUrl = "/tags/"
                        })
                ym = sortBy (\a b -> compare (fst b) (fst a)) $ yearsMap1 years
                yearCtx =
                    field "href" (return . fp' . itemBody) `mappend`
                    bodyField "year"
                firstYear = listToMaybe (map fst ym)
                fp' year
                    | Just year == firstYear = "/archive/"
                    | otherwise = "/archive/" ++ year ++ "/"

            makeItem t
                >>= loadAndApplyTemplate (themePart "_tags-wrapper.html") ctx
                >>= loadAndApplyTemplate tagsTemplateName ctx

    rulesExtraDependencies [d] $ tagsRules tags $ \tag identifiers -> do
        paginate <- buildPaginateWith (\ids -> return $ paginateEvery 5 $ reverse ids) identifiers (getTagIdentifier tag)
        paginateRules paginate $ \page ids -> do
            route addIndexRoute
            compile $ do
                posts <- recentFirst =<< loadAllSnapshots ids "content"
                let postsCtx =
                        listField "posts" postListCtx (return posts) `mappend`
                        paginateContext paginate page `mappend`
                        pageCtx (defaultMetadata
                            { metaTitle = Just $ "\"" ++ tag ++
                                (if page == 1 then "\""
                                    else "\", page " ++ show page)
                            , metaDescription = "My personal blog, posts tagged \"" ++ tag ++
                                (if page == 1 then "\"."
                                    else "\" " ++ show ((page - 1) * 5 + 1) ++ " to " ++ show (page * 5) ++ ".")
                            , metaUrl = "/tag/" ++ tag ++
                                (if page == 1 then "/"
                                    else "/page/" ++ show page ++ "/")
                            })

                makeItem ""
                    >>= loadAndApplyTemplate listTemplateName postsCtx
    where
        filterFn :: (Identifier, Metadata) -> Bool
        filterFn (_, metadata) =
            case KM.lookup "published" metadata of
                Just (String "false") -> False
                _ -> True
        yearsMap i = do
            utc <- getItemUTC defaultTimeLocale i
            return (formatTime defaultTimeLocale "%Y" utc, [i])
        yearsMap1 = M.assocs . M.fromListWith (++)




staticPagesRules :: Rules ()
staticPagesRules = do
    match "pages/search/index.html" $ do
        route idRoute
        compile $
            getResourceBody
                >>= loadAndApplyTemplate defaultTemplateName (pageCtx (defaultMetadata
                    { metaTitle = Just "검색 · 태그 · 정렬 (JSON 기반)"
                    , metaDescription = "posts-index.json을 이용한 빠른 검색/정렬/태그 필터"
                    , metaUrl = "/search/"
                    }))

    match "pages/route-planner/index.html" $ do
        route idRoute
        compile $
            getResourceBody
                >>= loadAndApplyTemplate (themePart "_post-without-footer.html") postCtx
                >>= loadAndApplyTemplate routePlannerTemplateName (pageCtx (defaultMetadata
                    { metaTitle = Just "Route Planner"
                    , metaDescription = "Calculate an optimized travel route between cities."
                    , metaUrl = "/route-planner/"
                    }))

    match "pages/map/index.html" $ do
            route (gsubRoute "pages/" (const ""))
            compile $
                getResourceBody
                    >>= loadAndApplyTemplate visitedCountriesTemplateName (pageCtx (defaultMetadata
                        { metaTitle = Just "Visited Map"
                        , metaDescription = "Map of visited countries and cities."
                        , metaUrl = "/map/"
                        }))


    match (fromList ["about.md", "projects.md", "404.md"]) $ do
        route removeExtension
        compile $ do
            identifier <- getUnderlying
            title <- getMetadataField identifier "title"
            description <- getMetadataField identifier "description"
            pandocCompiler False
                >>= loadAndApplyTemplate (themePart "_post-without-footer.html") postCtx
                >>= loadAndApplyTemplate defaultTemplateName (pageCtx (defaultMetadata
                    { metaTitle = fmap unwrap title
                    , metaDescription = unwrap $ fromMaybe "" description
                    , metaUrl = '/' : identifierToUrl (toFilePath identifier)
                    }))

data SitemapItem = SitemapItem String String

sitemapRules :: Rules ()
sitemapRules = do
    d <- makePatternDependency "posts/**"
    rulesExtraDependencies [d] $ create ["sitemap.xml"] $ do
        route idRoute

        ids <- getMatches "posts/**"
        let
            postItems = map (\i -> SitemapItem ("http://ygpark2.github.io/" ++ identifierToUrl (toFilePath i)) "1.0") ids
        compile $
            makeItem ""
                 >>= loadAndApplyTemplate (themeTemplate "sitemap.xml") (sitemapField (staticItems ++ postItems))
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



pandocCompiler :: Bool -> Compiler (Item String)
pandocCompiler rss = do
    post <- getResourceBody
    let parsed = runPure $ readMarkdown readerOptions (T.pack $ itemBody post)
    doc <- either (fail . show) return parsed
    makeItem $ T.unpack $ T.decodeUtf8 $ toByteString $ renderHtmlFragment UTF8 $ writeXmlHtml defaultXmlHtmlWriterOptions
        { idPrefix = "" --postUrl post
        , renderForRSS = rss
        , siteDomain = mainSiteDomain
        , debugOutput = False
        }
        doc


readerOptions :: ReaderOptions
readerOptions = def
  { readerExtensions =
        enableExtension Ext_raw_html $
        enableExtension Ext_smart $
        readerExtensions def
  }
