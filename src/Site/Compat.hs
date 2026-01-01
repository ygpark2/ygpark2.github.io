module Site.Compat
    ( paginateContext
    , getItemUTC
    , dateFieldWith
    , teaserField
    , chronological
    , recentFirst
    ) where

-- Local copies of Hakyll helpers to customize behavior and signatures.
import Control.Monad (msum, liftM)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate, isPrefixOf, sortBy)
import Data.Ord (comparing)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (TimeLocale, defaultTimeLocale, formatTime, parseTimeM)
import Hakyll hiding (chronological, getItemUTC, paginateContext, dateFieldWith, teaserField, recentFirst)
import System.FilePath (takeFileName)
import qualified Data.Map as M

import Site.Utils (simplifiedUrl, unwrap)

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
        Just r  -> return $ normalizedRoute r
        Nothing -> fail $ "No URL for page: " ++ show n

    normalizedRoute route =
        let cleaned = simplifiedUrl (routeWithSlash route)
        in case stripIndex cleaned of
            Just base -> base
            Nothing -> cleaned

    routeWithSlash r =
        case r of
            ('/':_) -> r
            _ -> '/' : r

    stripIndex r =
        case reverse r of
            ('l':'m':'t':'h':'.':'x':'d':'n':'i':'/':rest) ->
                let base = reverse rest
                in Just $ if base == "" then "/" else base ++ "/"
            _ -> Nothing

getItemUTC :: (MonadMetadata m, MonadFail m)
           => TimeLocale        -- ^ Output time locale
           -> Identifier        -- ^ Input page
           -> m UTCTime         -- ^ Parsed UTCTime
getItemUTC locale id' = do
    let parseField key fmt = do
            mv <- getMetadataField id' key
            return (mv >>= parseTime' fmt . unwrap)
        fn = takeFileName $ toFilePath id'
    parsed <- sequence $
        [ parseField "published" fmt | fmt <- formats ] ++
        [ parseField "date" fmt | fmt <- formats ] ++
        [ return $ parseTime' "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn ]
    maybe empty' return $ msum parsed
  where
    empty'     = fail $ "Hakyll.Web.Template.Context.getItemUTC: " ++
        "could not parse time for " ++ show id'
    parseTime' = parseTimeM True locale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%dT%H:%M:%S%Ez"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%Ez"
        , "%Y-%m-%d %H:%M:%S"
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

teaserSeparatorStart :: String
teaserSeparatorStart = "<!--more"

teaserSeparatorEnd :: String
teaserSeparatorEnd = "-->"

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

chronological :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a]
chronological =
    sortByM $ getItemUTC defaultTimeLocale . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (\k -> (x, k)) (f x)) xs

recentFirst :: (MonadMetadata m, MonadFail m, Functor m) => [Item a] -> m [Item a]
recentFirst = fmap reverse . chronological
