{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Description : Static site generator for mesokurtosis.com
-- Copyright   : (c) Daniel Klein, 2015
-- License     : MIT
-- Maintainer  : othercriteria@gmail.com
-- Stability   : experimental
-- Portability : non-portable
--
-------------------------------------------------------------------------------

module Main where

import           Hakyll
import           Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Map as M
import           Text.Pandoc.Options

------------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------------

numRecent :: Int
numRecent = 5

tagCloudField' :: String -> Tags -> Context String
tagCloudField' key ts = tagCloudField key 80 125 ts

config :: Configuration
config = defaultConfiguration
  { deployCommand = "s3cmd --delete-removed -P sync _site/ s3://mesokurtosis.com/"
  }

mathjaxScriptTag :: String
mathjaxScriptTag = "<script type=\"text/javascript\" src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"

mathExtensions :: [Text.Pandoc.Options.Extension]
mathExtensions  = [ Ext_tex_math_dollars
                  , Ext_latex_macros
                  ]

------------------------------------------------------------------------------
-- Modified Pandoc compiler to handle MathJax
--
-- For source and details, see:
--   travis.athougies.net/posts/2013-08-13-using-math-on-your-hakyll-blog.html
------------------------------------------------------------------------------
pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
    let defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions     = foldr S.insert defaultExtensions mathExtensions
        writerOptions     = defaultHakyllWriterOptions
                              { writerExtensions     = newExtensions
                              , writerHTMLMathMethod = MathJax ""
                              }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

------------------------------------------------------------------------------
-- Contexts
------------------------------------------------------------------------------

baseCtx :: Context String
baseCtx =
    constField "mathjax" "" <>
    defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%e %b %Y" <>
    mathCtx                     <>
    baseCtx

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags =
    tagsField "tags" tags <>
    postCtx

mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ if "mathjax" `M.member` metadata
             then mathjaxScriptTag
             else ""

aboutCtx :: Context String
aboutCtx =
    constField "page-about" "" <>
    baseCtx

contactCtx :: Context String
contactCtx =
    constField "page-contact" "" <>
    baseCtx

-------------------------------------------------------------------------------
-- Site generator itself
-------------------------------------------------------------------------------

truncateRoute :: String -> Routes
truncateRoute h = gsubRoute h (const "")

main :: IO ()
main = hakyllWith config $ do
    match "root_static/*" $ do
        route   $ truncateRoute "root_static/"
        compile   copyFileCompiler

    match "bootstrap/**" $ do
        route   $ truncateRoute "bootstrap/"
        compile   copyFileCompiler
        
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "root/stats.md" $ do
        route   $ truncateRoute "root/" `composeRoutes`
                  setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" baseCtx
            >>= relativizeUrls

    match "root/about.md" $ do
        route   $ truncateRoute "root/" `composeRoutes`
                  setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" aboutCtx
            >>= relativizeUrls

    match "root/contact.md" $ do
        route   $ truncateRoute "root/" `composeRoutes`
                  setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" contactCtx
            >>= relativizeUrls

    match "posts/*" $ do
        tags <- buildTags "posts/*" (fromCapture "tags/*.html")

        tagsRules tags $ \tag pattern -> do
            let title = "Posts tagged \"" ++ tag ++ "\""

            route     idRoute
            compile $ do
                posts  <- recentFirst =<< loadAll pattern
                let ctx = constField "title" title                  <>
                          listField  "posts" postCtx (return posts) <>
                          baseCtx

                makeItem ""
                    >>= loadAndApplyTemplate "templates/tag.html"     ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

        let postCtxTagged = postCtxWithTags tags

        route   $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtxTagged
            >>= loadAndApplyTemplate "templates/default.html" postCtxTagged
            >>= relativizeUrls

    match "links/*" $ do
        compile $ pandocCompiler
            >>= applyAsTemplate baseCtx

    match "trivia/*" $ do
        compile $ pandocCompiler
            >>= applyAsTemplate baseCtx

    create ["archive.html"] $ do
        route     idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField  "posts" postCtx (return posts) <>
                    constField "title" "Archives"             <>
                    constField "page-archive" ""              <>
                    baseCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["links.html"] $ do
        route     idRoute
        compile $ do
            links <- loadAll "links/*"
            let linksCtx =
                    listField  "links" baseCtx (return links) <>
                    constField "title" "Links"                <>
                    constField "page-links" ""                <>
                    baseCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/links.html"   linksCtx
                >>= loadAndApplyTemplate "templates/default.html" linksCtx
                >>= relativizeUrls

    create ["trivia.html"] $ do
        route     idRoute
        compile $ do
            trivia <- loadAll "trivia/*"
            let triviaCtx =
                    listField  "trivia" baseCtx (return trivia) <>
                    constField "title" "Williams Trivia"        <>
                    baseCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/trivia.html"  triviaCtx
                >>= loadAndApplyTemplate "templates/default.html" triviaCtx
                >>= relativizeUrls
                
    match "root/index.html" $ do
        route   $ truncateRoute "root/"
        compile $ do
            posts <- fmap (take numRecent) . recentFirst =<< loadAll "posts/*"
            tags  <- buildTags "posts/*" (fromCapture "tags/*.html")
            let indexCtx =
                    listField      "posts" postCtx (return posts) <>
                    tagCloudField' "tag-cloud" tags               <>
                    constField     "title" "Daniel L. Klein"      <>
                    constField     "page-home" ""                 <>
                    baseCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "root/error.html" $ do
        route   $ truncateRoute "root/"
        compile $ do
            getResourceBody
                >>= loadAndApplyTemplate "templates/default.html" baseCtx
                
    match "templates/*" $ compile templateCompiler
