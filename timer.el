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

(defun timer-redraw-display (timer &optional append)
  (let* (
      (time (gethash :time timer 0))
      (start (gethash :start timer))
      (display (format-time
        (if start
          (+ time (- (float-time) start))
          time))))
    (if append
      (puthash :display (insert-and-mark display) timer)
      (replace-marked (gethash :display timer) display))))

(defun redraw-timers ()
  (interactive)
  (switch-to-buffer timer-buffer)
  (erase-buffer)
  (dolist (timer timers)
    (timer-redraw-button timer t)
    (insert " ")
    (timer-redraw-display timer t)
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
  (puthash :start (float-time) timer)
  (timer-redraw-button timer)
  (timer-redraw-display timer)
  (puthash :timer
    (run-at-time
      (- 1 (mod (gethash :time timer 0) 1))
      0.5
      `(lambda () (timer-redraw-display ,timer)))
    timer)
  )

(defun stop-timer (timer)
  (puthash :time (+
      (- (float-time) (gethash :start timer))
      (gethash :time timer 0))
    timer)
  (puthash :start nil timer)
  (timer-redraw-button timer)
  (timer-redraw-display timer))

(defun format-time (time)
  (let (
    (seconds (floor (mod time 60)))
    (minutes (mod (floor (/ time 60)) 60))
    (hours (floor (/ time 3600))))
    (format "%02d:%02d:%02d" hours minutes seconds)))

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
