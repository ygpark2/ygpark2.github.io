module Site.Context (siteContext, postCtxWithTags, feedContext, postContext, teaserContext) where

import           Hakyll                     hiding (load)

import           Data.Configurator          (Worth (Required), getMap, load)
import           Data.Configurator.Types    (convert)
import qualified Data.HashMap.Strict        as H (empty, lookup, lookupDefault)
import qualified Data.Text                  as T
import qualified Data.Map                   as M
import           Data.Maybe
import           System.IO.Unsafe           (unsafePerformIO)
import           Text.Regex                 (mkRegex, subRegex)

-- import           Debug.Trace
-- import           Data.Configurator.Types.Internal (Name)

--------------------------------------------------------------------------------
data AppConfig = AppConfig {
                     cfgTitle :: String,
                     cfgDescription :: String,
                     cfgEmail :: String,
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
                   , cfgHeaderType = cfg "" $ T.pack "site.header.type"
                   , cfgPaginationType = cfg "" $ T.pack "site.pagination.type"
                   , cfgSnsGithubUsername = cfg "" $ T.pack "site.sns.github.username"
                   , cfgSnsTwitterUsername = cfg "" $ T.pack "site.sns.twitter.username"
                   , cfgSnsGoogleplay = cfg "" $ T.pack "site.sns.google.play"
                   , cfgSnsGooglePlusCommunity = cfg "" $ T.pack "site.sns.google.plus.community"
                   , cfgSnsGooglePlusProfile = cfg "" $ T.pack "site.sns.google.plus.profile"
                   }

siteContext :: Context String
siteContext = constField "site.title" (cfgTitle conf) `mappend`
              constField "site.description" (cfgDescription conf) `mappend`
              constField "site.email" (cfgEmail conf) `mappend`
              constField "site.header.type" (cfgHeaderType conf) `mappend`
              constField "site.pagination.type" (cfgPaginationType conf) `mappend`
              constField "site.sns.github.username" (cfgSnsGithubUsername conf) `mappend`
              constField "site.sns.twitter.username" (cfgSnsTwitterUsername conf) `mappend`
              constField "site.sns.google.play" (cfgSnsGoogleplay conf) `mappend`
              constField "site.sns.google.plus.community" (cfgSnsGooglePlusCommunity conf) `mappend`
              constField "site.sns.google.plus.profile" (cfgSnsGooglePlusProfile conf) `mappend`
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

postContext :: Context String
postContext =
  dateField "date" "%Y-%m-%d" `mappend` siteContext

teaserContext :: Context String
teaserContext =
  teaserField "teaser" "content" `mappend` postContext

postCtxWithTags :: Tags -> Tags -> Context String
postCtxWithTags tags categories = tagsField "tags" tags
                      `mappend` categoryField "categories" categories
                      `mappend` postContext

{-
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

simplifiedUrl :: String -> String
simplifiedUrl url = subRegex (mkRegex "/index\\.html$") url "/"
-}

feedContext :: Context String
feedContext =
  postContext `mappend` bodyField "description"
