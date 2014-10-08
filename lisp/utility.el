;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; user defined utilities
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert formated time string into current point
(defun insert-timestamp-at-point()
  (interactive)
  (insert
   (format-time-string 
    "%Y-%m-%d %H:%M")))
(global-set-key [(f8)] 'insert-timestamp-at-point)


