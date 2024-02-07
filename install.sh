#!/bin/bash

if ! command -v doom &>/dev/null; then
    echo "Install DOOM emacs first!"
    exit 1
fi


EMACS_DIR="$(pwd)"
DOOM_DIR=$HOME/.config/doom

cd "$DOOM_DIR" || (echo "No such dir as $DOOM_DIR" && exit 1)

# remove old files and links
rm ./*.el

ln -s "$EMACS_DIR/init.el" "$DOOM_DIR/init.el"
ln -s "$EMACS_DIR/config.el" "$DOOM_DIR/config.el"
ln -s "$EMACS_DIR/packages.el" "$DOOM_DIR/packages.el"

echo "New Doom Emacs configuration created!"
