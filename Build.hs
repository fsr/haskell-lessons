#!/usr/bin/env runhaskell

import System.Process
import System.Directory
import Control.Monad.Extra

makeScript = do
    callProcess "make" ["html"]
    callProcess "make" ["latexpdf"]

moveScript = do
    whenM (doesDirectoryExist "docs/script") $ removeDirectoryRecursive "docs/script"
    renameDirectory "script/build/html" "docs/script"
    
    renameFile "script/build/latex/HaskellLessons.pdf" "docs/script.pdf"

main = do
    withCurrentDirectory "script" makeScript
    
    -- withCurrentDirectory "slides" makeSlides

    moveScript
    
