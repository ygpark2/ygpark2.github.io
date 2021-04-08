{-# LANGUAGE OverloadedStrings #-}

module Site.Pandoc (makeTOC, pandocCompiler, postCompiler, indexCompiler) where

import Data.Functor.Identity
import Data.Set (insert, delete)
import Hakyll hiding (pandocCompiler)
import Text.Pandoc.Options
import Text.HTML.TagSoup (Tag(..))
import System.FilePath (dropFileName)

import qualified Data.Text as T
import qualified Text.DocTemplates as DT

pandocReaderOptions :: ReaderOptions
pandocReaderOptions =
   defaultHakyllReaderOptions
      {
         readerExtensions =
            (readerExtensions defaultHakyllReaderOptions) <> extensionsFromList
               [
                 Ext_tex_math_single_backslash  -- TeX math btw (..) [..]
               , Ext_tex_math_double_backslash  -- TeX math btw \(..\) \[..\]
               , Ext_tex_math_dollars           -- TeX math between $..$ or $$..$$
               , Ext_latex_macros               -- Parse LaTeX macro definitions (for math only)
               , Ext_inline_code_attributes     -- Ext_inline_code_attributes
               , Ext_abbreviations              -- PHP markdown extra abbreviation definitions
               ]
      }

pandocWriterOptions :: WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions {
      writerReferenceLinks = True
    , writerHTMLMathMethod = MathJax ""
    }

pandocWriterOptionsToc :: WriterOptions
pandocWriterOptionsToc = pandocWriterOptions {
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
  -- , "$body$"
  ]

-- Pandoc cannot handle CRLF on systems that use LF line endings, so we
-- remove the CR before feeding things into Pandoc.
removeCr :: String -> String
removeCr = filter (/= '\r')

getBodyWithoutCr :: Compiler (Item String)
getBodyWithoutCr = fmap (fmap removeCr) getResourceBody

makeTOC :: Compiler (Item String)
makeTOC =  do
    itemToc <- getItemToc
    renderPandocWith pandocReaderOptions pandocWriterOptionsToc itemToc
  where
    getItemToc = do
      item <- getResourceString
      let origId = itemIdentifier item
          newId = setVersion (Just "toc") origId
          body = itemBody item
      return (Item newId body)

-- compilers
{-
pandocCompilerWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)
pandocCompilerWith rOpt wOpt =
  cached "Hakyll.Web.Pandoc.pandocCompilerWith" $
    writePandocWith wOpt <$> (readPandocWith rOpt =<< getBodyWithoutCr)
-}

pandocCompiler :: Compiler (Item String)
pandocCompiler = pandocCompilerWith pandocReaderOptions pandocWriterOptions

postCompiler :: Compiler (Item String)
postCompiler = do
   ident <- getUnderlying
   toc   <- getMetadataField ident "withtoc"
   pandocCompilerWith pandocReaderOptions (maybe defaultOptions postOptions toc)
   where
      defaultOptions = pandocWriterOptions
      postOptions = const pandocWriterOptionsToc

indexCompiler :: Item String -> Compiler (Item String)
indexCompiler x =
   withItemBody (return . withTags dropIndex) x
   where
      dropIndex (TagOpen "a" attrs) = TagOpen "a" (dropIndex' <$> attrs)
      dropIndex tag                 = tag
      dropIndex' ("href", url) | not (isExternal url) = ("href", dropFileName url <> takeHash url)
      dropIndex' z                                    = z
      takeHash = dropWhile (/= '#')
