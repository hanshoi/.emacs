#!/bin/bash

EMACS_DIR="$(pwd)"
DOOM_DIR=$HOME/.config/doom

if [ ! -d "$DOOM_DIR" ]; then
    echo "Install DOOM emacs first!"
    git clone --depth 1 --single-branch https://github.com/doomemacs/doomemacs ~/.config/emacs ~/.config/emacs/bin/doom install
fi

# install prequisities
sudo apt install tidy shellcheck cmake


cd "$DOOM_DIR" || (echo "No such dir as $DOOM_DIR" && exit 1)

# remove old files and links
rm ./*.el
rm -f $HOME/.emacs
rm -rf $HOME/.emacs.d

ln -s "$EMACS_DIR/init.el" "$DOOM_DIR/init.el"
ln -s "$EMACS_DIR/config.el" "$DOOM_DIR/config.el"
ln -s "$EMACS_DIR/packages.el" "$DOOM_DIR/packages.el"
ln -s "$EMACS_DIR/functions.el" "$DOOM_DIR/functions.el"

sudo cp "$EMACS_DIR/doom.png" "/usr/share/icons/doom.png"
sudo cp "$EMACS_DIR/emacs.desktop" "/usr/share/applications/emacs.desktop"

echo "New Doom Emacs configuration created!"

$HOME/.config/emacs/bin/doom sync

echo "DONE!"
