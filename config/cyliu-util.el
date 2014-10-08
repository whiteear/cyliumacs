;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id$
;;
;; utils introduced by cyliu
;;
;; any problem, please contact cyliu7@gmail.com
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils to popular the emacs running platform
(defun cyliu-is-gui-enabled ()
  "detect whether emacs is running on GUI interface"
  window-system)

(defun cyliu-is-windows ()
  "detect whether emacs is running on Microsoft Windows"
  (memq window-system '(w32)))

(defun cyliu-is-X-window ()
  "detect whether emacs is running on X"
  ;; emacs 23 mac version reture 'ns'
  (memq window-system '(x ns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize vc-cvs
(defun cyliu-cvs-config
  (setq vc-cvs-diff-switchs "-U4")
  (setq vc-diff-switches "-U4")
  )

(defun cyliu-add-to-env (env path)
  "add this new path to PATH env"
  (setenv env (concat (getenv env) ";" path))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun insert-timestamp-at-point()
  (interactive)
  (insert
   (format-time-string 
    "<%Y-%m-%d %H:%M %a>")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mark line
;;
(defun cyliu-mark-line()
  "mark a whole line as region"
  (interactive)
  (push-mark)
  (move-end-of-line nil)
  (push-mark nil nil t)
  (move-beginning-of-line nil)
  )

(defun cyliu-mark-word()
  "mark a whole word as region"
  (interactive)
  (push-mark)
  (backward-word nil)
  (push-mark nil nil t)
  (forward-word nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; maximize frame at startup
(defun cyliu-frame-initialize ()
  "Initialize the frame size. This function doesn't take effect immediately.
you should invoke it when initialize emacs (put it in dot emacs file)."
   (setq default-frame-alist
     (list
      (cons 'left                 30)
      (cons 'top                  0)
      (cons 'width                120)
      (cons 'height               40)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; following functions (starting with w32-) copied from maxframe.el
(defun w32-maximize-frame()
  "Maximize the current frame (Windows only)"
  (interactive)
  (w32-send-sys-command 61488))

(defun w32-restore-frame()
  "Restore the current frame (Windows only)"
  (interactive)
  (w32-send-sys-command 61728))

(defun x-maximize-frame-vert ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32          
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
(defun x-maximize-frame-horz ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32          
                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(defun x-maximize-frame ()
  "Mazimize the current frame (X Windows only)"
  (interactive)
  (x-maximize-frame-vert)
  (x-maximize-frame-horz))

(defun x-restore-frame ()
  "Restore the current frame (X Windows only)"
  (interactive)
  (x-maximize-frame))

(defun cyliu-frame-maximize ()
  (interactive)
  (if (cyliu-is-windows)
      (w32-maximize-frame)
    (x-maximize-frame)))

(defun cyliu-frame-restore ()
  (interactive)
  (if (cyliu-is-windows)
      (w32-restore-frame)
    (x-restore-frame)))

(provide 'cyliu-util)
;; end of cyliu-util
