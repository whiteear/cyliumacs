;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common envirument setting

;; setup default path

(setenv "LANG" "C")
(set-locale-environment  "UTF-8")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal file system structure
;; /
;; |- workspace
;; |     |- ehome
;; |     |- source
;; |     |- repo
;; |
;; |- ibm-doc
;;       |- XXX
;; |- cyliu-doc
;;       |- YYY
;;
(defvar HOME (getenv "HOME"))
(defvar CFGHOME "/workspace/ehome")
(defvar WORKSPACE "/workspace")
(defvar IBMDOC "/ibm-doc")
(defvar CYLIUDOC "/cyliu-doc")
(defvar ORGHOME (concat CYLIUDOC "/org"))
(defvar CYGWIN nil)
(defvar WINEMACS nil) ;;where emacs installed on win32

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variables definition.
;;
;; NOTE: if not used, please set it nil

;; add new load path
(add-to-list 'load-path (concat CFGHOME "/config"))
(add-to-list 'load-path  (concat CFGHOME "/site-lisp"))
(add-to-list 'load-path  (concat CFGHOME "/lisp"))

(require 'cyliu-init)
(require 'cyliu-frame)
;;(require 'cyliu-ide)

(cyliu-frame-initialize)
