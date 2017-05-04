#!/usr/bin/env stack runhaskell --
{-# LANGUAGE BangPatterns #-}

import           Control.Concurrent
import           Control.Monad
import           Data.Foldable
import qualified Data.HashSet       as HS
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.FSNotify
import           System.Process


shakeCmd = callProcess "./ShakeFile.hs"
shakeTargets = ["script-html", "slides"]


reloadChrome = "tell application \"Google Chrome\"\n\
                \  activate\n\
                \  tell application \"System Events\"\n\
                \    tell process \"Google Chrome\"\n\
                \      keystroke \"r\" using {command down, shift down}\n\
                \    end tell\n\
                \  end tell\n\
                \end tell\n\
                \\n\
                \tell application \"Sublime Text\" to activate\n\
                \"

main = do
    cwd <- getCurrentDirectory
    shakeCmd ["--live"]
    !files <- HS.fromList . map (cwd </>) . lines <$> readFile "live.txt"
    removeFile "live.txt"
    let dirs = HS.filter (("docs" `notElem`) . splitDirectories) $ HS.map takeDirectory files
    withManager $ \mgr -> do
        for_ dirs $ \dir ->
            watchDir
                mgr
                dir
                (flip HS.member files . eventPath)
                (const $ do
                    shakeCmd shakeTargets
                    void $ readCreateProcess (proc "osascript" []) reloadChrome)

    -- sleep forever (until interrupted)
        forever $ threadDelay 1000000
