;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common envirument setting

;; setup default path

(setenv "LANG" "C")
(set-locale-environment  "English")
(setq system-time-local "C")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global var defination, if not used, please set it nil
;; 
;; the structure of emacs home
;; ehome
;;   |- .emacs
;;   |- config
;;   |- lisp
;;   |- site-lisp
;;
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

(require 'cyliu-util)
(require 'cyliu-init)
(require 'cyliu-ide)
(require 'cyliu-frame)

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (cyliu-init-frame))))
  (cyliu-init-frame))
