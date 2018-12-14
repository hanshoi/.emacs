;;; load-packages --- Auto install necessary packages

;;; Commentary:
; Auto installs all packages defined here.

;;; Code:

; A list of packages to ensure are installed at launch.
(setq package-selected-packages
  '(
    ;; language modes
    clojure-mode
    cython-mode
    markdown-mode
    sass-mode
    scss-mode
    go-mode
    web-mode
    lua-mode
    yaml-mode
    rust-mode
    toml-mode
    ponylang-mode

    ;; themes
    gruber-darker-theme
    monokai-theme
    solarized-theme
    zenburn-theme

    ;; project
    speedbar
    projectile
    org-jira
    magit

    ;; buffers
    buffer-move
    ibuffer

    ;; visual
    highlight-indentation

    ;; editing
    ergoemacs-mode
    xah-fly-keys
    flycheck
    multiple-cursors
    auto-complete
    electric-operator
    yasnippet

    ;; pony
    flycheck-pony
    flycheck-rust
    flycheck-mypy

    ;; python
    jedi
    jedi-core
    elpy
    python-pytest
    virtualenvwrapper

    ;; ido
    flx-ido
    ido-yes-or-no
    ido-completing-read+
    smex
    fzf

    ;; utils
    which-key
    url
    keyfreq
    ag
    projectile-ripgrep
    fuzzy
    ))


;; install packages from that list if needed
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)


(package-refresh-contents)
(package-install-selected-packages)

(provide 'hemacs-packages)
;;; hemacs-packages.el ends here

