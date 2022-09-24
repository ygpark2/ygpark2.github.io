module Site.Configuration (config, feedConfiguration) where

import Hakyll

import Data.List        (isPrefixOf, isSuffixOf)
import System.FilePath  (isAbsolute, normalise, takeFileName)
import System.Process   (system)

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration {
    destinationDirectory = "_site",
    storeDirectory       = "_cache",
    tmpDirectory         = "_cache/tmp",
    providerDirectory    = ".",
    ignoreFile           = ignoreFile',
    deployCommand        = "git push origin `git subtree split --prefix _site hakyll`:master --force",
    deploySite           = system . deployCommand,
    inMemoryCache        = True,
    previewHost          = "127.0.0.1",
    previewPort          = 8088
} where
    ignoreFile' path
        | "."    `isPrefixOf` fileName = True
        | "#"    `isPrefixOf` fileName = True
        | "~"    `isSuffixOf` fileName = True
        | ".swp" `isSuffixOf` fileName = True
        | otherwise                    = False
      where
        fileName = takeFileName path


feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "Young Gyu's Blog"
    , feedDescription = "Young Gyu's blog"
    , feedAuthorName = "Young Gyu Park"
    , feedAuthorEmail = "ygpark2@gmail.com"
    , feedRoot = ""
    }
