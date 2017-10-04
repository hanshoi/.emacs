;;; bindings --- Load all keybindings

;;; Commentary:
; Load all keybindings that I have defined

;;; Code:

;; multiple cursors bindings
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this-in-defun)

;; kill buffers
(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)
(global-set-key (kbd "C-c z") 'kill-this-buffer)
(global-set-key (kbd "C-c C-a") 'projectile-kill-buffers)

;; move buffers in windowns
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; change windowns easily
(global-set-key (kbd "M-S-<left>") 'windmove-left)
(global-set-key (kbd "M-S-<right>") 'windmove-right)
(global-set-key (kbd "M-S-<up>") 'windmove-up)
(global-set-key (kbd "M-S-<down>") 'windmove-down)

;; Make use of F* keys
(global-set-key [f5]    'undo)
(global-set-key [f6]    'next-error)
(global-set-key [f7]    'goto-line)
(global-set-key [f8]    're-search-forward)
(global-set-key [f9]    'compile)
(global-set-key [f10]   'mark-whole-buffer)
(global-set-key [f11]   'revert-buffer)
(global-set-key [f12]   'ediff-buffers)

;; move texts place up and down
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;; projectile bindings
(global-set-key (kbd "M-d") 'projectile-find-dir)
(global-set-key (kbd "M-p") 'projectile-switch-project)
(global-set-key (kbd "M-f") 'projectile-find-file)
(global-set-key (kbd "M-g") 'projectile-ag)

;; browse buffers easily
(global-set-key (kbd "M-<right>") 'next-user-buffer)
(global-set-key (kbd "M-<left>")  'previous-user-buffer)

;; yasnippet
(global-set-key (kbd "C-c C-s") 'yas-insert-snippet)

;; have ENTER (return) to indent next line as well
(global-set-key (kbd "RET") 'newline-and-indent)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'hemacs-bindings)
;;; hemacs-bindings.el ends here
