#!/usr/bin/env runhaskell

import System.Process
import System.Directory
import Control.Monad.Extra

makeScript = do
    callProcess "make" ["html"]
    callProcess "make" ["latexpdf"]

moveScript = do
    whenM (doesDirectoryExist "docs") $ removeDirectoryRecursive "docs"
    renameDirectory "script/build/html" "docs"
    renameFile "script/build/latex/HaskellLessons.pdf" "docs/script.pdf"

main = do
    withCurrentDirectory "script" makeScript
    
    -- withCurrentDirectory "slides" makeSlides

    moveScript
    
