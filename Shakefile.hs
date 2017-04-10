#!/usr/bin/env stack runhaskell -- 

import System.Directory
import System.FilePath
import Control.Monad.Extra
import Development.Shake

outputDirectory :: FilePath
outputDirectory = "docs"

scriptOutputDirectory :: FilePath
scriptOutputDirectory = outputDirectory </> "script"

slidesOutputDirectory :: FilePath
slidesOutputDirectory = outputDirectory </>  "slides"

revealResourcesDirectory :: FilePath
revealResourcesDirectory = slidesOutputDirectory </> "reveal.js"

copyFromReveal out = 
    let originalFile = slidesOutputDirectory `makeRelative` out
    in do 
        need [originalFile]
        liftIO $ copyFile originalFile out

main :: IO ()
main = shakeArgs shakeOptions $ do
    want ["script", "slides"]

    phony "script" $ need [scriptOutputDirectory </> "index.html", outputDirectory </> "script.pdf"]

    phony "slides" $ getDirectoryFiles "slides" ["*.rst"] >>= need . map ((-<.> "html") . (slidesOutputDirectory </>))

    (scriptOutputDirectory </> "index.html") %> \out' -> do
        let out = takeDirectory out'
        unit $ cmd "make" ["html"] (Cwd "script")
        liftIO $ removeDirectoryRecursive out
        liftIO $ renameDirectory "script/build/html" out

    (outputDirectory </> "script.pdf") %> \out -> do
        unit $ cmd "make" ["latexpdf"] (Cwd "script")
        liftIO $ renameFile "script/build/latex/HaskellLessons.pdf" out

    map (revealResourcesDirectory </>) ["js/*.js", "css/**/*.css", "lib/**/*.js", "lib/**/*.css"] |%> copyFromReveal
    "/reveal.js/**" %> const (unit $ cmd "git" ["submodule", "update"])

    (slidesOutputDirectory </> "*.html") %> \out -> do
        getDirectoryFiles "reveal.js/lib" ["css/*.css", "font/*/*.css", "js/*.js"] >>= need . map ((revealResourcesDirectory </> "lib") </>)
        need $ map (revealResourcesDirectory </>) ["css/reveal.css", "js/reveal.js", "lib/font/source-sans-pro/source-sans-pro.css"]
        getDirectoryFiles "reveal.js/css/theme" ["*.css"] >>= need . map ((revealResourcesDirectory </> "css/theme") </>)
        cmd "pandoc" ["slides" </> makeRelative slidesOutputDirectory out -<.> ".rst", "-o", out, "-t", "revealjs", "-s", "--variable=theme:white"]
