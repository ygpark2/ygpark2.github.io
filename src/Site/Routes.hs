{-# LANGUAGE OverloadedStrings #-}
module Site.Routes (postPattern, draftPattern, postSnapshot, decksSnapshot, postPerPage,
                    rootRoute, pageRoute, postRoute, assetsRoute, projectRoute, niceRoute,
                    postDescription, postRoot) where

import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Compiler (Snapshot)
import Hakyll.Core.Routes
import Hakyll.Core.Metadata
import Hakyll.Core.Util.String

import Control.Monad (msum, filterM, (<=<), (>=>), liftM, filterM)
import Data.List (groupBy, intercalate, intersperse)

import Data.Time.Format (TimeLocale, defaultTimeLocale, parseTimeM, formatTime)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeFileName, joinPath, splitPath, dropFileName, takeDirectory, takeBaseName, splitDirectories, (</>))



--------------------------------------------------------------------------------
-- CONFIGURATION
--------------------------------------------------------------------------------
postPattern :: Pattern
postPattern = "posts/**"

draftPattern :: Pattern
draftPattern = "drafts/**"

postSnapshot :: Snapshot
postSnapshot = "post-content"

postPerPage :: Int
postPerPage = 15

postTitle :: String
postTitle = "Thought and Mind"

postDescription :: String
postDescription = "My thoughts on life and tech"

postAuthor :: String
postAuthor = "Young Gyu Park"

postAuthorEmail :: String
postAuthorEmail = "ygpark2@gmail.com"

postRoot :: String
postRoot = "https://ygpark2.github.io"

decksSnapshot :: Snapshot
decksSnapshot = "decks-content"


-- routes
rootRoute :: Routes
rootRoute =
   customRoute (joinPath . dropDirectory . splitPath . toFilePath)
   where
      dropDirectory []       = []
      dropDirectory ("/":ds) = dropDirectory ds
      dropDirectory ds       = tail ds

pageRoute :: Routes
pageRoute =
   removeExtension `composeRoutes` addIndex
   where
      removeExtension = setExtension mempty
      addIndex = postfixRoute "index.html"
      postfixRoute postfix = customRoute $ (</> postfix) . toFilePath

postRoute :: Routes
postRoute =
   customRoute (takeFileName . toFilePath) `composeRoutes`
   metadataRoute dateRoute                 `composeRoutes`
   dropDateRoute                           `composeRoutes`
   pageRoute
   where
      dateRoute metadata = customRoute $ \id' -> joinPath [dateFolder id' metadata, toFilePath id']
      dateFolder id' = maybe mempty (formatTime defaultTimeLocale "%Y/%m") . tryParseDate id'
      dropDateRoute = gsubRoute "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}-" (const mempty)

assetsRoute :: Routes
assetsRoute = customRoute $ (\x -> x :: String) . drop 7 . toFilePath

projectRoute :: Routes
projectRoute =
  idRoute `composeRoutes`
  (customRoute $ (++ "/index") . takeWhile (/= '.') . drop 9 . toFilePath ) `composeRoutes`
  setExtension "html"

-------------------------------------------------------------------------------
-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar/ in most browsers
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                             where p=toFilePath ident

-- dates
tryParseDate :: Identifier -> Metadata -> Maybe UTCTime
tryParseDate =
   tryParseDateWithLocale defaultTimeLocale

tryParseDateWithLocale :: TimeLocale -> Identifier -> Metadata -> Maybe UTCTime
tryParseDateWithLocale locale id' metadata = do
   let tryField k fmt = lookupString k metadata >>= parseTime' fmt
       fn             = takeFileName $ toFilePath id'
   maybe empty' return $ msum $
      [tryField "published" fmt | fmt <- formats] ++
      [tryField "date"      fmt | fmt <- formats] ++
      [parseTime' "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn]
   where
      empty'     = fail $ "Hakyll.Web.Template.Context.getItemUTC: "
                        ++ "could not parse time for " ++ show id'
      parseTime' = parseTimeM True locale
      formats    =
         [ "%a, %d %b %Y %H:%M:%S %Z"
         , "%Y-%m-%dT%H:%M:%S%Z"
         , "%Y-%m-%d %H:%M:%S%Z"
         , "%Y-%m-%d"
         , "%B %e, %Y %l:%M %p"
         , "%B %e, %Y"
         ]
