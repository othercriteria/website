--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


-------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match (fromList [ "favicon.ico"
                    , "robots.txt"
		    , "googledf7fba8d8e31fb41.html"]) $ do
        route   idRoute
        compile copyFileCompiler
	
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "links/*" $ do
        compile $ pandocCompiler
            >>= applyAsTemplate linkCtx

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["links.html"] $ do
        route idRoute
        compile $ do
            links <- loadAll "links/*"
            let linksCtx =
                    listField "links" linkCtx (return links) `mappend`
                    constField "title" "Links"               `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/links.html"   linksCtx
                >>= loadAndApplyTemplate "templates/default.html" linksCtx
                >>= relativizeUrls
                
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            links <- loadAll "links/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    listField "links" linkCtx (return links) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "error.html" $ do
        route idRoute
        compile $ do
            let errorCtx = defaultContext

            getResourceBody
                >>= loadAndApplyTemplate "templates/default.html" errorCtx
                >>= relativizeUrls
                
    match "templates/*" $ compile templateCompiler


------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

linkCtx :: Context String
linkCtx =
    defaultContext