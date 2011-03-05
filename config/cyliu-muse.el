;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; configuration for muse mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting for muse
;; load muse
(defvar MUSE-PATH (concat CFGHOME "/lisp"))
(defvar MUSE-PUBLISH-PATH (concat WORKDOC "/html"))
(defvar MUSE-SRC-PATH WORKDOC)

(add-to-list 'load-path MUSE-PATH)
(require 'muse-mode)     ; load authoring mode
(require 'muse-html)     ; load publishing styles I use
;;(require 'muse-mode nil t) ; load authoring mode
;;(require 'muse-wiki nil t) ; load Wiki support
;;(require 'muse-html nil t) ; load (X)HTML publishing style
;;(require 'muse-colors nil t) ; load coloring/font-lock module
;;(require 'outline nil t)
;;(require 'sgml-mode)
;;(require 'tp-muse-highlight nil t) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto recognize setting
(add-to-list 'auto-mode-alist 
	     '("#title " . muse-mode))

(add-hook 'text-mode-hook
          (lambda ()
            (unless (eq major-mode 'muse-mode)
              (when (equal (file-name-extension buffer-file-truename) "txt")
                (save-excursion
                  (goto-line 5)
                  (if (re-search-backward "\* [A-Z][a-z]+.*" 1 t)
                      (muse-mode)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; muse project setting

(defun getsrcpath( proj &optional relatepath)
  "generate muse project src path"
  (if (stringp proj)
      (progn
        (concat MUSE-SRC-PATH
                (if (null relatepath) 
                    ""
                  (concat "/" relatepath))
                "/"
                proj))
    nil
    ))

(defun getpubpath( proj &optional relatepath)
  "generate muse project publish path"
  (if (stringp proj)
      (concat MUSE-PUBLISH-PATH 
              (if (null relatepath)
                  ""
                (concat "/" relatepath))
              "/"
              proj)
    nil
    ))

(defun generate-project (proj &optional relatepath)
  ""
  (if (stringp proj)
      (list proj
            (list (getsrcpath proj relatepath) :default "index")
            (list :base "html" :path (getpubpath proj))
            )
    ))


(require 'muse-project)
(add-to-list 'muse-project-alist 
             (generate-project "vmo"))
(add-to-list 'muse-project-alist
             (generate-project "linux"))
(add-to-list 'muse-project-alist 
             (generate-project "blog" "personal"))
(add-to-list 'muse-project-alist 
             (generate-project "info" "personal"))
(add-to-list 'muse-project-alist 
             (generate-project "novel" "personal"))
(add-to-list 'muse-project-alist 
             (generate-project "emacs"))
(add-to-list 'muse-project-alist 
             (generate-project "develop"))
(add-to-list 'muse-project-alist 
             (generate-project "xen"))
(add-to-list 'muse-project-alist 
             (generate-project "stock" "personal"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; publish style setting
(setq muse-html-charset-default "utf-8")
;;(setq muse-html-style-sheet "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"../css/brep.css\">")
(setq muse-html-style-sheet "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"../css/xingzhaopeng.css\" />")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc 
;; (define-key muse-mode-map [tab] 'indent-for-tab-command)

