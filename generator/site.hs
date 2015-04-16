-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Hakyll
import qualified Data.Set as S
import           Text.Pandoc.Options

-------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "root_static/*" $ do
        route   $ gsubRoute "root_static/" (const "")
        compile copyFileCompiler
        
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "root/about.md" $ do
        let aboutCtx =
                constField "page-about" "" `mappend`
                defaultContext

        route   $ (gsubRoute "root/" (const "")) `composeRoutes`
                  setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" aboutCtx
            >>= relativizeUrls

    match "root/contact.md" $ do
        let contactCtx =
                constField "page-contact" "" `mappend`
                defaultContext

        route   $ gsubRoute "root/" (const "") `composeRoutes`
                  setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" contactCtx
            >>= relativizeUrls

    match "posts/*" $ do
        tags <- buildTags "posts/*" (fromCapture "tags/*.html")

        tagsRules tags $ \tag pattern -> do
            let title = "Posts tagged \"" ++ tag ++ "\""
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll pattern
                let ctx = constField "title" title                 `mappend`
                          listField "posts" postCtx (return posts) `mappend`
                          defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/tag.html"     ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

        let postCtxTagged = postCtxWithTags tags

        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtxTagged
            >>= loadAndApplyTemplate "templates/default.html" postCtxTagged
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
                    constField "page-archive" ""             `mappend`
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
                    constField "page-links" ""               `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/links.html"   linksCtx
                >>= loadAndApplyTemplate "templates/default.html" linksCtx
                >>= relativizeUrls
                
    match "root/index.html" $ do
        route $ gsubRoute "root/" (const "") <> idRoute 
        compile $ do
            posts <- fmap (take 5) . recentFirst =<< loadAll "posts/*"
            tags <- buildTags "posts/*" (fromCapture "tags/*.html")
            links <- loadAll "links/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    listField "links" linkCtx (return links) `mappend`
                    tagCloudField "tag-cloud" 80 125 tags    `mappend`
                    constField "title" "Daniel L. Klein"     `mappend`
                    constField "page-home" ""                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "root/error.html" $ do
        route $ gsubRoute "root/" (const "") <> idRoute
        compile $ do
            let errorCtx = defaultContext

            getResourceBody
                >>= loadAndApplyTemplate "templates/default.html" errorCtx
                >>= relativizeUrls
                
    match "templates/*" $ compile templateCompiler


------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%e %b %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags =
    tagsField "tags" tags `mappend`
    postCtx

linkCtx :: Context String
linkCtx =
    defaultContext

------------------------------------------------------------------------------
-- travis.athougies.net/posts/2013-08-13-using-math-on-your-hakyll-blog.html
pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
    let mathExtensions    = [ Ext_tex_math_dollars
                            , Ext_tex_math_double_backslash
                            , Ext_latex_macros
                            ]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions     = foldr S.insert defaultExtensions mathExtensions
        writerOptions     = defaultHakyllWriterOptions
                              { writerExtensions     = newExtensions
                              , writerHTMLMathMethod = MathJax ""
                              }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { deployCommand = "s3cmd -P sync _site/ s3://mesokurtosis.com/"
  }
