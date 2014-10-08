;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id$
;; cyliu's emacs intergration config with cygwin
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; donot use default system PATH

(defun cyliu-cygwin-config ()

  (setenv "PATH" "")
  (cyliu-add-to-env "PATH" (concat WINEMACS "/bin"))
  (cyliu-add-to-env "PATH" (concat CYGWIN "/bin"))
  (cyliu-add-to-env "PATH" (concat CYGWIN "/usr/bin"))
  (cyliu-add-to-env "PATH" (concat CYGWIN "/usr/local/bin"))
  (cyliu-add-to-env "PATH" (concat CYGWIN "/usr/sbin"))
  
  (setq exec-path (list 
                   (concat WINEMACS "/bin")
                   (concat CYGWIN "/bin")
                   (concat CYGWIN "/usr/bin")
                   (concat CYGWIN "/usr/sbin")
                   (concat CYGWIN "/usr/local/bin")
                   ))

  (require 'cygwin-mount)
  (cygwin-mount-activate)
  
  (add-hook 'comint-output-filter-functions
            'shell-strip-ctrl-m nil t)
  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt nil t)
  (setq explicit-shell-file-name "bash.exe")
  ;; For subprocesses invoked via the shell
  ;; (e.g., "shell -c command")
  (setq shell-file-name explicit-shell-file-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if the system is Windows and cygwin home setted, 
;; i assume that cygwin environment has been installed.
;; otherwise not installed.
(if (and (cyliu-is-windows) CYGWIN)
    (cyliu-cygwin-config)
  )

(provide 'cyliu-cygwin)
;; end of cyliu-cygwin
