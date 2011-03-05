;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common envirument setting

;; setup default path

(setenv "LANG" "C")
(set-locale-environment  "English")
(setq system-time-local "C")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global var defination, if not used, please set it nil
;; user home
(if (boundp 'UDISK)
    (defvar HOME UDISK)
  (defvar HOME (getenv "HOME")))
(defvar EHOME (concat HOME "/ehome"))
(defvar LHOME (concat HOME "/Linux"))
(defvar CFGHOME EHOME)
(defvar WORKSPACE (concat HOME "/workspace"))
(defvar WORKDOC (concat HOME "/workdoc"))
(defvar CYGWIN-HOME nil)
(defvar PUBLISH-DIR (concat HOME "/html_publish"))
(defvar ORGHOME (concat HOME "/org"))

;; add new load path
(add-to-list 'load-path (concat CFGHOME "/site-lisp"))
(add-to-list 'load-path (concat CFGHOME "/lisp"))
(add-to-list 'load-path (concat CFGHOME "/config"))

;;(load-file (concat CFGHOME "/config/cyliu-util.el"))
;;(load-file (concat CFGHOME "/config/01init.el"))
;;(load-file (concat CFGHOME "/config/02frame.el"))
;;(load-file (concat CFGHOME "/config/03muse.el"))
;;(load-file (concat CFGHOME "/config/04ide.el"))
;;(load-file (concat CFGHOME "/config/05ecb.el"))
;;(load-file (concat CFGHOME "/config/06vc.el"))
;;(load-file (concat CFGHOME "/config/08org.el"))
;;(load-file (concat CFGHOME "/config/cygwin.el"))

(require 'cyliu-util)
(require 'cyliu-init)
;;(require 'cyliu-frame)

;; (if (and (fboundp 'daemonp) (daemonp))
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (with-selected-frame frame
;;                   (cyliu-frame-initialize))))
;;   (cyliu-frame-initialize))
;;(cyliu-frame-initialize)
;;(require 'sr-speedbar)
