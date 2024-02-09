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

;; (setq doom-leader-key "<f8>")

(setq doom-themes-treemacs-theme "doom-colors")
(after! treemacs
  (treemacs-git-mode 1)
  (treemacs-follow-mode 1))


;; What treemacs does on RET action
(setq treemacs-RET-actions-config
      '((root-node-open   . treemacs-toggle-node)
        (root-node-closed . treemacs-toggle-node)
        (dir-node-open    . treemacs-toggle-node)
        (dir-node-closed  . treemacs-toggle-node)
        (file-node-open   . h-treemacs-open-file)
        (file-node-closed . h-treemacs-open-file)
        (tag-node-open    . treemacs-toggle-node-prefer-tag-visit)
        (tag-node-closed  . treemacs-toggle-node-prefer-tag-visit)
        (tag-node         . h-treemacs-open-file)))


(transient-define-prefix treemacs-menu ()
  "Menu for treemacs operations"
  [["File"
    ("fc" "Create" treemacs-create-file)
    ("fd" "Delete" treemacs-delete-file)
    ("fm" "Move" treemacs-move-file)
    ("fr" "Rename" treemacs-rename)
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
(define-key treemacs-mode-map (kbd "n") 'treemacs-RET-action)
(define-key treemacs-mode-map (kbd "h") 'treemacs-TAB-action)
(define-key treemacs-mode-map (kbd "t") 'treemacs-next-line)
(define-key treemacs-mode-map (kbd "c") 'treemacs-previous-line)
(define-key treemacs-mode-map (kbd "v") 'treemacs-next-project)
(define-key treemacs-mode-map (kbd "m") 'treemacs-previous-project)
(define-key treemacs-mode-map (kbd "b") 'treemacs-previous-project)
(define-key treemacs-mode-map (kbd "a") 'execute-extended-command)
(define-key treemacs-mode-map (kbd "SPC") 'treemacs-menu)


;; isearch mode keymaps
(define-key isearch-mode-map (kbd "C-h") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)

;; (define-key xah-fly-leader-key-map (kbd "a") 'projectile-ripgrep)
(define-key xah-fly-leader-key-map (kbd "4") 'split-window-below)
(define-key xah-fly-leader-key-map (kbd "k") 'vc-annotate)
(define-key xah-fly-leader-key-map (kbd "g") 'h-treemacs-toggle)
(define-key xah-fly-leader-key-map (kbd "i") '+lookup/references)
(define-key xah-fly-leader-key-map (kbd "o") 'dired)
(define-key xah-fly-leader-key-map (kbd "a") 'execute-extended-command)
(define-key xah-fly-leader-key-map (kbd "rf") 'flycheck-buffer)
(define-key xah-fly-leader-key-map (kbd "rr") 'flycheck-buffer)
(define-key xah-fly-leader-key-map (kbd "rl") 'flycheck-list-errors)
(define-key xah-fly-leader-key-map (kbd "rn") 'flycheck-next-error)
(define-key xah-fly-leader-key-map (kbd "rh") 'flycheck-previous-error)
;; (define-key xah-fly-leader-key-map (kbd "j") 'xah-copy-file-path)
;; (define-key xah-fly-leader-key-map (kbd "o") 'projectile-find-file)
(define-key xah-fly-leader-key-map (kbd "f") 'terminal-toggle)

(define-key xah-fly-key-map (kbd "<f8>") 'xah-fly-mode-toggle)



(defun my-command-mode-hook ()
  (setq jedi:complete-on-dot nil)
  (xah-fly--define-keys
   xah-fly-key-map
   '(
     ("'" . yas-insert-snippet)
     ("o" . hippie-expand)
     ("e" . delete-char)
     ("." . backward-kill-word)
     ("4" . split-window-right)
     ("b" . xah-search-current-word)
     ("m" . centaur-tabs-backward)
     ("v" . centaur-tabs-forward)
     ("DEL" . nil)
     ("SPC" . xah-fly-leader-key-map)
     ("l" . +format/buffer)
     ("i" . +lookup/definition)
     ("x" . better-jumper-jump-backward))))

(defun my-insert-mode-hook ()
  (if (eq major-mode 'python-mode)
      (setq jedi:complete-on-dot t))
  (xah-fly--define-keys
   xah-fly-key-map
   '(
     ("DEL" . nil))))

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
