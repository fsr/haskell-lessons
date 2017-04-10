#!/usr/bin/env stack runhaskell -- 

import System.Directory
import System.FilePath
import Control.Monad.Extra
import Development.Shake

outputDirectory :: FilePath
outputDirectory = "docs"

main :: IO ()
main = shakeArgs shakeOptions $ do
    want $ map (outputDirectory </>) ["script/index.html", "script.pdf"]

    (outputDirectory </> "script") %> \out' -> do
        let out = takeDirectory out'
        unit $ cmd "make" ["html"] (Cwd "script")
        liftIO $ removeDirectoryRecursive out
        liftIO $ renameDirectory "script/build/html" out

    (outputDirectory </> "script.pdf") %> \out -> do
        unit $ cmd "make" ["latexpdf"] (Cwd "script")
        liftIO $ renameFile "script/build/latex/HaskellLessons.pdf" out
    

    -- withCurrentDirectory "slides" makeSlides

    
