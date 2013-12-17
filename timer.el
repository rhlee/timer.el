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

(defun timer-redraw-button (timer &optional append)
  (let
      ((button (if (gethash :start timer)
        (propertize stop-button 'action `(lambda (e) (stop-timer ,timer)))
        (propertize start-button 'action `(lambda (e) (start-timer ,timer))))))
  (if append
    (puthash :button (insert-and-mark button) timer)
    (replace-marked (gethash :button timer) button))))

(defun redraw-timers ()
  (interactive)
  (switch-to-buffer timer-buffer)
  (erase-buffer)
  (dolist (timer timers)
    (timer-redraw-button timer t)
    (insert "\n")))

(defun insert-and-mark (string)
  (let ((start (point)))
    (insert string)
    (list start (length string))))

(defun replace-marked (mark string)
  (goto-char (car mark))
  (delete-char (car (cdr mark)))
  (if (not (eq (length string) (car (cdr mark))))
    (error "Replacement string length does not match"))
  (insert string))

(defun start-timer (timer)
  (puthash :start 0 timer)
  (timer-redraw-button timer))

(defun stop-timer (timer)
  (puthash :start nil timer)
  (timer-redraw-button timer))

(setq start-button
  (with-temp-buffer (insert-text-button "[ Start ]" 'face 'default)
  (add-text-properties 3 8 '(face (:foreground "red")))
  (buffer-string)))

(setq stop-button
  (with-temp-buffer (insert-text-button "[ Stop  ]" 'face 'default)
  (add-text-properties 3 8 '(face (:foreground "green")))
  (buffer-string)))

(timer)

(new-timer "hello")
(new-timer "apple")
(new-timer "bear")
(redraw-timers)

(setq txt (insert-and-mark "hello"))
(replace-marked txt "jlalo")
