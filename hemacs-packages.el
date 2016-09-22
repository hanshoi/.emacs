;;; load-packages --- Auto install necessary packages

;;; Commentary:
; Auto installs all packages defined here.

;;; Code:

; A list of packages to ensure are installed at launch.
(setq package-selected-packages
  '(
    ;; language modes
    clojure-mode
    markdown-mode
    sass-mode
    scss-mode
    web-mode
    lua-mode
    yaml-mode

    ;; themes
    gruber-darker-theme
    monokai-theme
    solarized-theme
    zenburn-theme

    ;; project
    speedbar
    projectile

    ;; buffers
    buffer-move
    ibuffer

    ;; visual
    highlight-indentation

    ;; editing
    flycheck
    multiple-cursors
    auto-complete
    electric-operator
    yasnippet

    ;; python
    jedi
    jedi-core
    pymacs

    ;; ido
    flx-ido
    ido-yes-or-no
    ido-ubiquitous
    smex

    ;; utils
    url
    ))


;; install packages from that list if needed
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)


(package-refresh-contents)
(package-install-selected-packages)

(provide 'hemacs-packages)
;;; hemacs-packages.el ends here

