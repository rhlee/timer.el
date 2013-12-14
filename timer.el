(setq timer-buffer nil)
(setq timers nil)
(setq timer-next-id -1)

(defvar timer-mode-map (make-sparse-keymap) "timer mode keymap")
(define-key timer-mode-map [follow-link] 'mouse-face)

(defun timer ()
  (interactive)
  (setq timer-buffer (generate-new-buffer "timer"))
  (switch-to-buffer timer-buffer)
  (setq mode-name "Timer")
  (use-local-map timer-mode-map)
  (redraw-timers)
  (add-hook 'kill-buffer-query-functions 'timer-quit nil t))

(defun timer-quit()
  (print "hello")
  t)

(defun redraw-timers ()
  (interactive))
