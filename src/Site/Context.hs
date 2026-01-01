module Site.Context
    ( timeLocale
    , tagsContext
    , postCtx
    , postWithCommentsCountCtx
    , pageCtx
    , FacebookType(..)
    , PageMetadata(..)
    , defaultMetadata
    , getTags
    , isPublished
    ) where

-- Contexts and metadata used across templates and page rendering.
import Data.List (intercalate)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (TimeLocale(..), formatTime, defaultTimeLocale)

import Hakyll hiding (dateFieldWith, getTags, teaserField)

import Site.Compat (dateFieldWith, teaserField)
import Site.Utils (getNavMenuIO, getSettingString, identifierToDisqus, identifierToUrl, unwrap)

isPublished :: (MonadMetadata m) => Identifier -> m Bool
isPublished identifier = do
    published <- getMetadataField identifier "published"
    return (published /= Just "false")

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
    , ("July", "Jul")
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
      title <- getMetadataField (itemIdentifier i) "title"
      return $ escapeHtml $ maybe "" unwrap title) `mappend`
    siteCtx `mappend`
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
    constField "meta.url" (escapeHtml $ "http://ygpark2.github.io" ++ url) `mappend`
    constField "meta.description" (escapeHtml description) `mappend`
    constField "meta.keywords" (escapeHtml $ intercalate ", " keywords) `mappend`
    constField "meta.dc.subject" (escapeHtml $ intercalate "; " keywords) `mappend`
    siteCtx `mappend`
    facebookFields fType `mappend`
    defaultContext
    where
        metaTitle' Nothing = "[Young Gyu Park's blog]"
        metaTitle' (Just title') = title' ++ " :: [Young Gyu Park's blog]"

        facebookFields (FacebookArticle published keywords' images) =
                constField "meta.facebook.article" "" `mappend`
                constField "meta.facebook.published" (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" published) `mappend`
                listField "meta.facebook.tags" defaultContext (mapM makeItem keywords') `mappend`
                listField "meta.facebook.images" defaultContext (mapM makeItem images)
        facebookFields _ = constField "meta.facebook.nothing" ""

data FacebookType = FacebookArticle UTCTime [String] [String] -- Published, keywords, images
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
    , metaKeywords = ["Blog", "блог"]
    , metaType = FacebookNothing
    }

siteCtx :: Context String
siteCtx =
    constField "site.title" (escapeHtml $ getSettingString "site.title" "[Young Gyu's blog]") `mappend`
    constField "site.subtitle" (escapeHtml $ getSettingString "site.subtitle" "Thoughts, projects, and archives.") `mappend`
    constField "site.url" (escapeHtml $ getSettingString "site.url" "http://ygpark2.github.io") `mappend`
    constField "site.disqus.shortname" (escapeHtml $ getSettingString "site.disqus.shortname" "ygpark2") `mappend`
    listField "site.nav" navCtx (do
        items <- unsafeCompiler getNavMenuIO
        mapM makeItem items)
  where
    navCtx =
        field "title" (return . fst . itemBody) `mappend`
        field "url" (return . snd . itemBody)

getTags :: MonadMetadata m => Identifier -> m [String]
getTags identifier = do
    tags <- getMetadataField identifier "tags"
    return $ maybe [] (map trim . splitAll "," . unwrap) tags
