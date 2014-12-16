#!/usr/bin/runhaskell

----------------------------
-- This script creates symlinks from the home directory to all
-- configuration files in ~/dotfiles.
----------------------------

{-# LANGUAGE LambdaCase #-}

import Control.Monad (forM_)
import System.Directory
import System.FilePath
import System.Posix.Files

dotfiles :: String
dotfiles = "dotfiles"                  -- dotfiles directory

oldDotfiles :: String
oldDotfiles = "dotfiles_old"             -- old dotfiles backup directory

data Action = Home String String
            | NewDir String String String

files :: [Action]
files = [Home "bashrc" ".bashrc",
         Home "asoundrc" ".asoundrc",
         Home "emacs.el" ".emacs.el",
         Home "xinitrc" ".xinitrc",
         Home "xmobarrc" ".xmobarrc",
         Home "muttrc" ".muttrc",
         Home "vimrc" ".vimrc",
         Home "Xresources" ".Xresources",
         Home "gitconfig" ".gitconfig",
         NewDir "cabal" ".cabal" "config",
         NewDir "xmonad.hs" ".xmonad" "xmonad.hs",
         NewDir "redshift.conf" ".config" "redshift.conf",
         NewDir "mplayer" ".mplayer" "config",
         NewDir "cabal" ".cabal" "config"
       ]

main :: IO ()
main = do
   setCurrentDirectory dotfiles
   putStrLn $ "Creating" ++ oldDotfiles ++
              " for backup of any existing dotfiles in ~"
   createDirectoryIfMissing False oldDotfiles
   home <- getHomeDirectory
   forM_ files $ \case
         Home name newName-> do
             putStrLn $ "Moving " ++ name ++ " to " ++ oldDotfiles
             rename (home </> newName) (home </> oldDotfiles </> name)
             putStrLn $ "Creating symlink to " ++ name ++ "in the home dir"
             createSymbolicLink name (home </> newName)
         NewDir name newDir newName -> do
             putStrLn $ "Moving " ++ name ++ " to " ++ oldDotfiles
             rename (home </> newDir </> newName) (home </> oldDotfiles </> name)
             createDirectoryIfMissing False newDir
             putStrLn $ "Creating symlink to " ++ name ++ "in home dir"
             createSymbolicLink name (home </> newDir </> newName)
   createSymbolicLink (home </> ".xinitrc") (home </> ".xsession") 


