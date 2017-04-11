#!/usr/bin/env stack runhaskell -- 

import System.Directory hiding (doesDirectoryExist)
import System.FilePath
import Control.Monad.Extra
import Development.Shake
import System.IO
import Data.Foldable

outputDirectory :: FilePath
outputDirectory = "docs"

scriptOutputDirectory :: FilePath
scriptOutputDirectory = outputDirectory </> "script"

slidesOutputDirectory :: FilePath
slidesOutputDirectory = outputDirectory </>  "slides"

revealResourcesDirectory :: FilePath
revealResourcesDirectory = slidesOutputDirectory </> "reveal.js"

copyFromReveal :: FilePath -> Action ()
copyFromReveal out = 
    let originalFile = joinPath $ dropWhile (/= "reveal.js/") $ splitPath $ slidesOutputDirectory `makeRelative` out
    in do 
        need [originalFile]
        liftIO $ copyFile originalFile out

needRstSources :: Action ()
needRstSources = getDirectoryFiles "script/source" ["*.rst", "exercises/*.rst"] >>= need . map ("script/source" </>)

buildSlideWithTheme :: String -> FilePath -> Action ()
buildSlideWithTheme theme out = do
    let sourceFile = "slides" </> takeFileName out -<.> ".md"
        templateFile = "slides/template.html"
        revealDir = takeDirectory out </> "reveal.js"
    need [sourceFile, templateFile]
    getDirectoryFiles "reveal.js/lib" ["css/*.css", "font/*/*.css", "js/*.js"] >>= need . map ((revealDir </> "lib") </>)
    need $ map (revealDir </>) ["css/reveal.css", "js/reveal.js", "lib/font/source-sans-pro/source-sans-pro.css"]
    need [revealDir </> "css/theme" </> theme <.> "css"]
    -- getDirectoryFiles "slides/img" ["*.png"] >>= need . map ((slidesOutputDirectory </> "img") </>)
    cmd "pandoc" [sourceFile, "--slide-level", "2", "-o", out, "-t", "revealjs", "-s", "--variable=theme:" ++ theme, "--template", templateFile]

main :: IO ()
main = shakeArgs shakeOptions $ do
    want ["script", "slides", outputDirectory </> ".nojekyll"]

    phony "script" $ need [scriptOutputDirectory </> "index.html", outputDirectory </> "script.pdf"]

    phony "slides" $ do 
        slideMds <- getDirectoryFiles "slides" ["*.md"]
        let htmls = map (-<.> "html") slideMds
        let whiteHtmls = map ("white" </>) htmls
        need $ map (slidesOutputDirectory </>) $ htmls ++ whiteHtmls

    phony "clean" $ do 
        removeFilesAfter outputDirectory ["*"]
        for_ [scriptOutputDirectory, slidesOutputDirectory] $ \dir ->
            whenM (doesDirectoryExist dir) $ liftIO $ removeDirectoryRecursive dir

    "**/.nojekyll" %> \out -> liftIO $ withFile out AppendMode (`hPutChar` ' ')

    (scriptOutputDirectory </> "index.html") %> \out' -> do
        let out = takeDirectory out'
        needRstSources
        unit $ cmd "make" ["html"] (Cwd "script")
        liftIO $ removeDirectoryRecursive out
        liftIO $ renameDirectory "script/build/html" out

    (outputDirectory </> "script.pdf") %> \out -> do
        needRstSources
        unit $ cmd "make" ["latexpdf"] (Cwd "script")
        liftIO $ renameFile "script/build/latex/HaskellLessons.pdf" out

    map ((outputDirectory </> "**/reveal.js") </>) ["js/*.js", "css/**/*.css", "lib/**/*.js", "lib/**/*.css"] |%> copyFromReveal
    "/reveal.js/**" %> const (unit $ cmd "git" ["submodule", "update"])

    (slidesOutputDirectory </> "img" </> "*") %> liftIO . (copyFile <$> ("slides/img" </>) . takeFileName <*> id)

    (slidesOutputDirectory </> "*.html") %> buildSlideWithTheme "black"
    (slidesOutputDirectory </> "white/*.html") %> buildSlideWithTheme "white"
