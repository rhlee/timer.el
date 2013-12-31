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
  (if (not (boundp 'timer-autosave))
    (setq timer-autosave
      (run-at-time 60 60 (lambda () (save-timers timers))))))

;;kill running timers

(defun new-timer (name)
  (interactive "sname: ")
  (setq timers (append timers (list (timer-new name)))))

(defun timer-new (name)
  (let ((timer (make-hash-table)))
    (puthash :name name timer)
    timer))

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
      (display (if start
        (format-time (+ time (- (float-time) start)))
        (format-time time t))))
    (if append
      (puthash :display (insert-and-mark display) timer)
      (replace-marked (gethash :display timer) display))))

(defun redraw-timers ()
  (interactive)
  (with-current-buffer timer-buffer
    (erase-buffer)
    (dolist (timer timers)
      (timer-redraw-button timer t)
      (insert " ")
      (timer-redraw-display timer t)
      (insert " ")
      (insert (propertize (gethash :name timer) 'face 'bold))
      (insert "\n"))
    (goto-char (point-max))))

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
  (with-current-buffer timer-buffer
    (timer-redraw-button timer)
    (timer-redraw-display timer)
    (goto-char (point-max)))
  (puthash :timer
    (run-at-time
      (- 0.5 (mod (gethash :time timer 0) 0.5))
      0.5
      `(lambda ()
        (with-current-buffer timer-buffer
          (timer-redraw-display ,timer)
          (goto-char (point-max)))))
    timer))

(defun stop-timer (timer)
  (puthash :time (+
      (- (float-time) (gethash :start timer))
      (gethash :time timer 0))
    timer)
  (puthash :start nil timer)
  (cancel-timer (gethash :timer timer))
  (with-current-buffer timer-buffer
    (timer-redraw-button timer)
    (timer-redraw-display timer)
    (goto-char (point-max))))

(defun format-time (time &optional stop)
  (let (
      (seconds (floor (mod time 60)))
      (minutes (mod (floor (/ time 60)) 60))
      (hours (floor (/ time 3600)))
      (sep (if stop ":"
        (if (< (mod time 1) 0.5) ":" " "))))
    (format (concat "%02d" sep "%02d" sep "%02d") hours minutes seconds)))

(defun save-timers (timers)
  (with-temp-file "~/timers"
    (prin1
      (let (strippeds)
        (dolist (timer timers strippeds)
          (let (
              (stripped (make-hash-table))
              (start (gethash :start timer)))
            (puthash :name (gethash :name timer) stripped)
            (puthash :time (+
                (gethash :time timer 0)
                (if start (- (float-time) start) 0))
              stripped)
            (setq strippeds
              (append strippeds (list stripped))))))
      (current-buffer))))

(defun load-timers ()
   (interactive)
   (let ((buffer (find-file-noselect "~/timers")))
     (setq timers (read buffer))
     (kill-buffer buffer))
   (redraw-timers))

(defun adjust-timer (timer minutes)
  (setq edebug-on-error t)
  (interactive (list
    (let ((timer-list (mapcar (lambda (timer) (gethash :name timer)) timers)))
      (nth
        (-
          (length timer-list)
          (length
            (member (completing-read "Select timer: " timer-list) timer-list)))
        timers))
    (read-from-minibuffer "Minutes: ")))
  (if (string-match "^\\(?1:[+-]\\)?\\(?2:[[:digit:]]+\\)$" minutes)
    (progn
      (let (
          (sign (match-string 1 minutes))
          (number (* (string-to-int (match-string 2 minutes)) 60))
          (start (gethash :start timer)))
        (puthash :time
          (max
            0
            (if sign
              (funcall (intern sign)
                (+
                  (gethash :time timer 0)
                  (if start
                    (let ((float-time-now (float-time)))
                      (puthash :start float-time-now timer)
                      (- float-time-now start))
                    0))
                number)
              number))
          timer)))
     (error
       "Please match enter a whole number, optionally prefixed by a sign."))
  (redraw-timers))
    

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
