;;; bindings --- Load all keybindings

;;; Commentary:
; Load all keybindings that I have defined

;;; Code:
(define-key xah-fly-leader-key-map (kbd "a") 'projectile-ag)
(define-key xah-fly-leader-key-map (kbd "4") 'split-window-below)
(define-key xah-fly-leader-key-map (kbd "k") 'vc-annotate)
(define-key xah-fly-leader-key-map (kbd "i") 'jedi:goto-definition)
(define-key xah-fly-leader-key-map (kbd "x") 'jedi:goto-definition-pop-marker)
(define-key xah-fly-leader-key-map (kbd "j") 'xah-copy-file-path)
(define-key xah-fly-leader-key-map (kbd "o") 'projectile-find-file)
(define-key xah-fly-leader-key-map (kbd "b") 'xah-search-current-word)
(define-key xah-fly-key-map (kbd "<f8>") 'xah-fly-mode-toggle)


(defun my-command-mode-hook ()
  (xah-fly--define-keys
   xah-fly-key-map
     '(
       ("m" . xah-previous-user-buffer)
       ("v" . xah-next-user-buffer)
       ("DEL" . nil)
       ("i" . jedi:goto-definition)
       ("x" . jedi:goto-definition-pop-marker))))

(defun my-insert-mode-hook ()
(xah-fly--define-keys
   xah-fly-key-map
     '(
       ("DEL" . nil))))

(add-hook 'xah-fly-command-mode-activate-hook 'my-command-mode-hook)
(add-hook 'xah-fly-insert-mode-activate-hook 'my-insert-mode-hook)

;; have ENTER (return) to indent next line as well
(global-set-key (kbd "RET") 'newline-and-indent)

(provide 'hemacs-bindings)
;;; hemacs-bindings.el ends here
