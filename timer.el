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

(defun new-timer (name)
  (interactive "sname: ")
  (setq timers (append timers (list (timer-new name)))))

(defun timer-new (name)
  (let ((timer (list
      :name name
      :time 0
      :start nil)))
    timer))

(defun timer-quit()
  (print "hello")
  t)

(defun timer-draw-button (timer)
  (print (point))
  (insert "hello\n"))

(defun redraw-timers ()
  (interactive)
  (dolist (timer timers)
    (timer-draw-button timer)))

(timer)
;(insert (propertize "foo" 'face '(:foreground "red")))
;(with-temp-buffer)
;(insert (with-temp-buffer (insert-text-button "hello" 'action (lambda () nil))
;  (buffer-string)))
;(setq overwrite-mode overwrite-mode-textual)
(new-timer "hello")
(new-timer "apple")
(new-timer "bear")
(redraw-timers)
