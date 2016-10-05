--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Highlighting.Kate (styleToCss, pygments)
import           Data.Yaml.YamlLight
import           Data.ByteString.UTF8 (toString)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  configYaml <- parseYamlFile "config.yaml"
  let siteCtx = mkSiteCtx configYaml
  hakyll $ do
    match ("templates/*" .||. "includes/*") $ compile templateBodyCompiler

    create ["css/highlight.css"] $ do
        route   idRoute
        compile $ makeItem (compressCss $ styleToCss pygments)

    match "assets/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" (postCtx `mappend` siteCtx)
            >>= relativizeUrls

    match "about.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" (siteCtx `mappend` defaultContext)
            >>= loadAndApplyTemplate "templates/default.html" (siteCtx `mappend` defaultContext)
            >>= relativizeUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let archiveCtx =
                listField "posts" postCtx (return posts) `mappend`
                constField "title" "Archives" `mappend`
                siteCtx `mappend`
                defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" archiveCtx
              >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- take 4 <$> (recentFirst =<< loadAll "posts/*")
        let indexCtx =
              listField "posts" postCtx (return posts) `mappend`
              constField "title" "My Hakyll Blog" `mappend`
              siteCtx `mappend`
              defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "time" "%Y-%m-%dT%H:%M:%S%Z" `mappend`
  dateField "date" "%b %-d, %Y" `mappend`
  defaultContext

mkSiteCtx :: YamlLight -> Context String
mkSiteCtx = mconcat . fmap mkSiteCtx' . getTerminalsKeys
  where
    mkSiteCtx' (val, [YStr key]) = constField (toString key) (toString val)
    mkSiteCtx' _ = mempty
