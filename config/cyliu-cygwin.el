;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id: cygwin.el,v 1.3 2009/11/03 03:05:27 cyliu7 Exp $
;; cyliu's emacs intergration config with cygwin
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; donot use default system PATH

(defun cyliu-cygwin-config ()

  (setenv "PATH" "")
  (cyliu-add-to-env "PATH" (concat EMACS-HOME "/bin"))
  (cyliu-add-to-env "PATH" (concat CYGWIN-HOME "/bin"))
  (cyliu-add-to-env "PATH" (concat CYGWIN-HOME "/usr/bin"))
  (cyliu-add-to-env "PATH" (concat CYGWIN-HOME "/usr/local/bin"))
  (cyliu-add-to-env "PATH" (concat CYGWIN-HOME "/usr/sbin"))
  
  (setq exec-path (list 
                   (concat EMACS-HOME "/bin")
                   (concat CYGWIN-HOME "/bin")
                   (concat CYGWIN-HOME "/usr/bin")
                   (concat CYGWIN-HOME "/usr/sbin")
                   (concat CYGWIN-HOME "/usr/local/bin")
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
(if (and (cyliu-is-windows) CYGWIN-HOME)
    (cyliu-cygwin-config)
  )

(provide 'cyliu-cygwin)
