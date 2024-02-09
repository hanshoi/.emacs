;;; functions --- Custom defined functions and actions

;;; Commentary:
;; Functions and actions created by me or copied from somewhere else

;;; Code:
;; Kill buffer and delete file
(defun delete-this-buffer-and-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; kill all buffers
(defun kill-all-buffers ()
  "Kill all buffers from Emacs."
  (interactive)
  (mapc
   (lambda (x)
     (let ((name (buffer-name x)))
       (unless (string-equal "*" (substring name 0 1))
         (kill-buffer x))))
   (buffer-list)))


;; move code to next line
;; code copied from http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(defun move-text-internal (arg)
  "Move text ARG value and to direction of ARG."
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (move-text-internal (- arg)))


;; move buffers
(defun validate-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command,
so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”."
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))


(defun next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `user-buffer-q'."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (validate-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `user-buffer-q'."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (validate-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))


;; start vterm - only vterm and kill all others
(defun start-vterm-only ()
  (interactive)
  (+vterm/here t))


(defun terminal-toggle ()
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (delete-window)
    (+vterm/toggle nil)))

(defun h-treemacs-toggle ()
  (interactive)
  (treemacs-select-window)
  (if (eq major-mode 'treemacs-mode)
      (xah-fly-insert-mode-activate)
    (xah-fly-command-mode-activate)))

(defun h-treemacs-close ()
  (interactive)
  (delete-window)
  (xah-fly-command-mode-activate))

(defun h-treemacs-open-file (&optional arg)
  (interactive)
  (treemacs-visit-node-default arg)
  (xah-fly-command-mode-activate))
