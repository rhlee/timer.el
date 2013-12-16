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
  (add-hook 'kill-buffer-query-functions 'timer-quit nil t))

(defun new-timer (name)
  (interactive "sname: ")
  (setq timers (append timers (list (timer-new name)))))

(defun timer-new (name)
  (let ((timer (make-hash-table)))
    (puthash :name name timer)
    timer))

(defun timer-quit()
  (print "hello")
  t)

(defun timer-redraw-button (timer)
  (puthash :button (point) timer)
  (insert (timer-get-button timer)))

(defun timer-get-button (timer)
  (concat
    (propertize "[")
    (propertize " Start ")
    (propertize "]")))

(defun redraw-timers ()
  (interactive)
  (dolist (timer timers)
    (timer-redraw-button timer)))

(defun insert-and-mark (string)
  (let ((start (point)))
    (insert string)
    (list start (length string))))

(defun goto-and-delete (marks)
  (goto-char (car marks))
  (delete-char (car (cdr marks))))

(setq start-button
  (with-temp-buffer (insert-text-button "[ Start ]" 'face 'default)
  (add-text-properties 3 8 '(face (:foreground "red")))
  (buffer-string)))

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
(insert (propertize start-button 'action (lambda (e) (print "p"))))
