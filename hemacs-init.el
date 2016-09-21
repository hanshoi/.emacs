;;; hemacs --- Hanshoi's emacs file

;;; Commentary:
; General init file for loading the whole package.
;
; Using convention `hemacs-*` to name stuff to separate my files from general
; files used elsewhere (like package.el -> hemacs-package.el).

;;; Code:
(defconst hemacs-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of these Emacs files."
)

(add-to-list 'load-path hemacs-install-dir)

(require 'hemacs-packages)  ; install all packages
(require 'hemacs-settings)  ; general settings file
(require 'hemacs-functions) ; custom functions
(require 'hemacs-bindings)  ; keybindings

(provide 'hemacs-init)
;;; hemacs-init.el ends here


