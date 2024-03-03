;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(load! "functions.el")


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(setq xah-fly-use-meta-key nil) ; must come before loading xah-fly-keys, disables ALT keys
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "dvorak")
(xah-fly-keys 1)
(setq indent-tabs-mode nil)


;; refresh from disk when changes on disk
(global-auto-revert-mode t)

;; (setq doom-leader-key "<f8>")
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq doom-themes-treemacs-theme "doom-colors")
(after! treemacs
  (treemacs-git-mode 1)
  (treemacs-follow-mode 1)
  (treemacs-hide-gitignored-files-mode 1)
  (setq treemacs-RET-actions-config
        '((root-node-open   . treemacs-toggle-node)
          (root-node-closed . treemacs-toggle-node)
          (dir-node-open    . treemacs-toggle-node)
          (dir-node-closed  . treemacs-toggle-node)
          (file-node-open   . h-treemacs-open-file)
          (file-node-closed . h-treemacs-open-file)
          (tag-node-open    . treemacs-toggle-node-prefer-tag-visit)
          (tag-node-closed  . treemacs-toggle-node-prefer-tag-visit)
          (tag-node         . h-treemacs-open-file))))


(transient-define-prefix treemacs-menu ()
  "Menu for treemacs operations"
  [["File"
    ("fc" "Create" treemacs-create-file)
    ("fd" "Delete" treemacs-delete-file)
    ("fm" "Move" treemacs-move-file)
    ("fr" "Rename" treemacs-rename-file)
    ("fj" "Copy" treemacs-copy-file)]
   ["Directory"
    ("dc" "Create" treemacs-create-dir)
    ("dd" "Delete" treemacs-delete)]
   ["Project"
    ("pw" "Swich Workspace" treemacs-switch-workspace)]
   ["Other"
    ("w" "close" h-treemacs-close)
    ("g" "GTFO - quit" h-treemacs-toggle)
    ("SPC" "GTFO - quit" h-treemacs-toggle)]])


;; treemacs mode keymaps
(map! :after treemacs :map treemacs-mode-map
      "n" #'treemacs-RET-action
      "h" #'treemacs-TAB-action
      "c" #'treemacs-previous-line
      "t" #'treemacs-next-line
      "m" #'treemacs-previous-project
      "v" #'treemacs-next-project
      "a" #'execute-extended-command
      "SPC" #'treemacs-menu)

;; isearch mode keymaps
(map! :map isearch-mode-map
      "C-h" #'isearch-repeat-backward
      "C-n" #'isearch-repeat-forward)


(map! :after xah-fly-keys :map xah-fly-leader-key-map
      "4" #'split-window-below
      "k" #'vc-annotate
      "g" #'h-treemacs-toggle
      "i" #'+lookup/references
      "o" #'dired
      "a" #'execute-extended-command
      "rf" #'flycheck-buffer
      "rr" #'flycheck-buffer
      "rl" #'flycheck-list-errors
      "rn" #'flycheck-next-error
      "rh" #'flycheck-previous-error
      "a" #'projectile-ripgrep
      "mm" #'mc/edit-lines
      "me" #'mc/edit-lines
      "mc" #'mc/mark-previous-like-this
      "mt" #'mc/mark-next-like-this
      "f" #'terminal-toggle)

(map! :after xah-fly-keys :map xah-fly-key-map
      "<f8>" #'xah-fly-mode-toggle
      "DEL" #'nil)

(map! :after xah-fly-keys :map xah-fly-command-map
      "'" #'yas-insert-snippet
      "o" #'hippie-expand
      "e" #'delete-char
      "." #'backward-kill-word
      "4" #'split-window-right
      "b" #'xah-search-current-word
      "m" #'centaur-tabs-backward
      "v" #'centaur-tabs-forward
      "DEL" #'nil
      "SPC" #'xah-fly-leader-key-map
      "l" #'+format/buffer
      "i" #'+lookup/definition
      "x" #'better-jumper-jump-backward)

(defun my-command-mode-hook ()
  (if (eq major-mode 'python-mode)
      (setq jedi:complete-on-dot nil)))

(defun my-insert-mode-hook ()
  (if (eq major-mode 'python-mode)
      (setq jedi:complete-on-dot t)))

(add-hook 'xah-fly-command-mode-activate-hook 'my-command-mode-hook)
(add-hook 'xah-fly-insert-mode-activate-hook 'my-insert-mode-hook)

;; have ENTER (return) to indent next line as well
(global-set-key (kbd "RET") 'newline-and-indent)

;; format on save
(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
	sql-mode         ; sqlformat is currently broken
	tex-mode         ; latexindent is broken
	latex-mode))


(add-hook 'python-mode-hook 'pet-mode -10)
