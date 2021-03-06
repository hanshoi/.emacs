#!/bin/bash

CURRENT_DIR=`pwd`

if [ -f $HOME/.emacs ]; then
    mv -f $HOME/.emacs $HOME/.backup_emacs  # store old emacs configuration in case it is needed some day
    echo "Old .emacs file changed to .backup_emacs"
else
    echo "No .emacs file found"
fi
echo ""

echo "; refer hemacs for real configuration.
;Hemacs is installed in ${CURRENT_DIR}.

(add-to-list 'load-path \"${CURRENT_DIR}\")
(require 'hemacs-init)
" > ~/.emacs

cat ~/.emacs
echo "New emacs file created:"
