module Site.Meta ( filterDrafts
                 , filterDraftItems
                 , getYearContext
                 , postContext
                 , postContextWithTags
                 , stripIndexSuffix
                 -- , dateRoute
                 ) where

import           Control.Monad (filterM, liftM)
import           Data.Char (isDigit)
import           Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Time.Calendar (toGregorian)
import           Data.Time.Clock
import           Data.Time.Format
import           Hakyll
import           System.FilePath.Posix (takeFileName, dropExtension)

isNotDraft :: MonadMetadata m => Identifier -> m Bool
isNotDraft identifier = liftM (/= Just "true") (getMetadataField identifier "draft")

filterDrafts :: MonadMetadata m => [Identifier] -> m [Identifier]
filterDrafts  = filterM isNotDraft

filterDraftItems :: [Item a] -> Compiler [Item a]
filterDraftItems = filterM (isNotDraft . itemIdentifier)

-- A neat trick for static sites is to put pages in directories,
-- and to put the page itself in index.html in the directory.
-- This is how the site is generated, but links should not have the index.html suffix.
stripIndexSuffix :: Item String -> Compiler (Item String)
stripIndexSuffix = return . fmap (withUrls stripSuffix)
  where idx             = "/index.html"
        domain          = "http://amixtureofmusings.com/"
        domainidx       = "http://amixtureofmusings.com/index.html"
        shouldStrip url = (("/" `isPrefixOf` url) || (domain `isPrefixOf` url)) &&
                          (idx `isSuffixOf` url)
        stripSuffix url
          | url == idx       = "/"
          | url == domainidx = domain
          | shouldStrip url  = reverse $ drop (length idx) $ reverse url
          | otherwise        = url

-- Route a post to a directory which is based on the post date.
{-
dateRoute :: Routes
dateRoute = metadataRoute (\md ->
  -- Extract date from metadata, and format it as yyyy/mm/dd.
  -- Also extract the slug from the original file, and append it.
  let dateString = fromMaybe "2000-01-01" $ M.lookup "date" md
      date       = parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M" dateString :: UTCTime
      datePath   = formatTime defaultTimeLocale "%Y/%m/%d" date
      dropDate   = dropWhile $ \c -> isDigit c || (c == '-')
      slug       = dropDate . dropExtension . takeFileName . toFilePath
      url        = (++ "/index.html") . ((datePath ++ "/") ++) . slug
  in  customRoute url)
-}

-- Post context contains human and machine readable date.
postContext :: Context String
postContext =
  dateField  "humandate" "%e %B, %Y"  <>
  dateField  "machinedate" "%Y-%m-%d" <>
  dateField  "archivedate" "%e %B"    <>
  dateField  "archiveyear" "%Y"       <>
  defaultContext


postContextWithTags :: Tags -> Context String
postContextWithTags tags =
    tagsField "tags" tags <> postContext


-- Extracts the year from a time.
getYear :: UTCTime -> Integer
getYear = (\(y, _, _) -> y) . toGregorian . utctDay

-- Site context contains the field 'year'.
yearContext :: Integer -> Context String
yearContext year = constField "year" (show year)

getYearContext :: IO (Context String)
getYearContext = fmap (yearContext . getYear) getCurrentTime
