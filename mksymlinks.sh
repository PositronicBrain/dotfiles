#!/bin/bash
############################
# This script creates symlinks from the home directory to any
# desired dotfiles in ~/dotfiles.
#
# Edited version of
# https://github.com/michaeljsmalley/dotfiles/blob/master/makesymlinks.sh
############################

########## Variables

dir=~/dotfiles                    # dotfiles directory
olddir=~/dotfiles_old             # old dotfiles backup directory
# list of files/folders to symlink in homedir
files="bashrc asoundrc emacs.el xinitrc xmobarrc gitconfig muttrc vimrc Xresources"

# create dotfiles_old in homedir
echo "Creating $olddir for backup of any existing dotfiles in ~"
mkdir -p "$olddir"
echo "...done"

# change to the dotfiles directory
cd $dir

# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks 
for file in $files; do
    echo "Moving .$file from ~ to $olddir"
    mv ~/.$file ~/dotfiles_old/$file
    echo "Creating symlink to .$file in home directory."
    ln -s $dir/$file ~/.$file
done

#xsession should point to xinitrc
echo "Moving .xsession from ~ to $olddir"
mv ~/.xsession ~/dotfiles_old/xsession
echo "Creating symlink to .xsession in home directory."
ln -s $dir/xinitrc ~/.xsession

# take care of xmonad config
mv  ~/.xmonad/xmonad.hs ~/dotfiles_old/
mkdir -p ~/.xmonad
ln -s $dir/xmonad.hs ~/.xmonad/xmonad.hs


# mplayer config
mv  ~/.mplayer/config ~/dotfiles_old/mplayer
mkdir -p ~/.mplayer
ln -s $dir/mplayer ~/.mplayer/config
