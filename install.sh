#!/bin/bash

CURRENT_DIR=`pwd`

mv -f $HOME/.emacs $HOME/.backup_emacs  # store old emacs configuration in case it is needed some day
echo "; refer hemacs for real configuration.
;Hemacs is installed in ${CURRENT_DIR}.

(add-to-list 'load-path \"${CURRENT_DIR}\")
(require 'hemacs-init)
" > ~/.emacs
