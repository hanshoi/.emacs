;;; settings -- General settings file

;;; Commentary:
; General and mode specific settings are added here.
;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;
;;  GENERAL SETTINGS  ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; load theme
(load-theme 'solarized-dark t)

;; delete currently selectd line
(delete-selection-mode 1)

;; highlight brackets
(show-paren-mode 1)

;; ido
(require 'ido)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(setq ido-enable-flex-matching t)

(require 'ido-completing-read+)
(ido-ubiquitous-mode t)

(require 'ido-yes-or-no)
(ido-yes-or-no-mode t)

(require 'smex)
(smex-initialize)

;; keyfreq (key combination analyzer)
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; projectile
(require 'projectile)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(projectile-global-mode)

;; auto-complete
(require 'auto-complete)
(ac-config-default)
(global-auto-complete-mode t)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; show filename in title
(setq-default frame-title-format "%f")

;; speedbar
;;(speedbar 1)

;; disable n00b bars
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; flycheck
(require 'flycheck)
(setq flycheck-display-errors-delay 0.1)
(global-flycheck-mode)

;; indent highlight
(require 'highlight-indentation)

;; electric spacing
(require 'electric-operator)

;; multiple cursors
(require 'multiple-cursors)

(require 'whitespace)
;; empty: removes empty lines in beginning and end
;; trailing: removes trailing whitespaces
;; lines: shows longer than 80char lines
;; tab: use space instead of tab
(setq whitespace-style '(face lines tab trailing))
(setq whitespace-line-column 79)

;; no backup files
(setq make-backup-files nil)

;; line numbers
(setq line-number-mode 1)
;; column numbers
(setq column-number-mode 1)

;; enable mouse wheel scrolling
(setq mouse-wheel-mode t)

;; spaces over tabs
(setq-default indent-tabs-mode nil)

;; buffer-move macros
(require 'buffer-move)

;; global auto revert mode. Useful when working with branches
(global-auto-revert-mode t)

;; yasnippet
(require 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE SPECIFIC SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; PYTHON

;; ropemacs
;(require 'pymacs)
;(pymacs-load "ropemacs" "rope-")
;(setq ropemacs-guess-project t)
;(setq ropemacs-enable-autoimport t)
;(setq ropemacs-autoimport-modules '("os" "shutil" "sys" "logging" "django.*" "flask*"))

;(defun auto-open-rope-project ()
;  "Adding hook to automatically open a rope project if there is one in the current or in the upper level directory."
;  (cond ((file-exists-p ".ropeproject")
;         (rope-open-project default-directory))
;        ((file-exists-p "../.ropeproject")
;         (rope-open-project (concat default-directory "..")))
;        ))

(defun hemacs-disable-electric-indent ()
  "Disable electric indent.
This is quite good in Python as electric-indent has traditionally been broken there."
  (set (make-local-variable 'electric-indent-mode) nil))


(defun hemacs-python-mode-hook ()
  "Initialize all python related stuff."
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (setq whitespace-mode t)
  (flycheck-select-checker 'python-flake8)
;  (hemacs-disable-electric-indent)
  (setq highlight-indentation-mode t)
  (jedi:setup)
  (setq electric-operator-mode t)
;;  (setq ropemacs-mode t)
;;  (auto-open-rope-project)
  )
(add-hook 'python-mode-hook 'hemacs-python-mode-hook)

;; jedi for python
(setq jedi:complete-on-dot t)
(setq jedi:setup-keys t)


;; GO
(defun hemacs-go-mode-hook ()
  "Initialize GO -langugage mode."
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq tab-width 2))
(add-hook 'go-mode-hook 'hemacs-go-mode-hook)


;; WEB
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))

(setq web-mode-engines-alist '(("php" . "\\.phtml\\'")
			       ("blade" . "\\.blade\\.")
			       ("django" . "\\.html?\\'")) )


; add react support
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  "Add tweak for jsx mode."
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

; jsx flychecker
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))

; web-mode hook
(defun hemacs-web-mode-hook ()
  "Initialize web-mode."
  (whitespace-mode -1)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-tag-auto-close-style 1)
  (when (equal web-mode-content-type "jsx")
    ;; enable flycheck
    (flycheck-select-checker 'jsxhint-checker)))
(add-hook 'web-mode-hook 'hemacs-web-mode-hook)

; set web-mode to work with {% %} syntax
(add-hook
   'web-mode-hook
   '(lambda ()
      (setq web-mode-enable-auto-pairing nil)
      (setq-local
       electric-pair-pairs
       (append electric-pair-pairs '((?% . ?%))))))


;; CSS
(defun hemacs-css-mode-hook ()
  "Initialize CSS -mode."
  (setq css-indent-offset 2))
(add-hook 'css-mode-hook 'hemacs-css-mode-hook)


;; SCSS
(defun hemacs-scss-mode-hook ()
  "Initialize SCSS-mode."
  (setq scss-compile-at-save nil))
(add-hook 'scss-mode-hook 'hemacs-scss-mode-hook)


;; JS
(defun hemacs-js-mode-hook ()
  "Initialize Javascript mode."
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'hemacs-js-mode-hook)


;; Bash
(add-to-list 'auto-mode-alist '("\\.rc\\'" . sh-mode))

(provide 'hemacs-settings)
;;; hemacs-settings.el ends here
