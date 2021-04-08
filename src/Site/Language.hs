module Site.Language where

import Data.Char (toUpper)
import Data.Map (Map)

import Hakyll.Core.Identifier (toFilePath)
import Hakyll.Core.Item (Item(..))

import Site.Prefixes (prs)


data Lang = PT | EN | DE deriving (Eq, Ord, Bounded, Enum, Read)

instance Show Lang where
    show PT = "pt"
    show EN = "en"
    show DE = "de"

languages :: [Lang]
languages = [minBound ..]

defaultLang :: Lang
defaultLang = EN


data TR = TR { ptT :: String, enT :: String, deT :: String } deriving (Eq, Ord, Read, Show)

newtype TRs = TRs { trMap :: Map String TR } deriving (Eq, Ord, Read, Show)


itemLang :: Item a -> Lang
itemLang item = if (length p > length prs) then (lFromPath p) else defaultLang
    where p         = toFilePath (itemIdentifier item)
          lFromPath = read . map toUpper . take 2 . drop (length prs)