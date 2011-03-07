;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; config emacs like an IDE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cscope setting
(require 'xcscope)

(require 'cc-mode)
(defun my-c-mode-hook() 
  (setq tab-width 4)
  (setq c-basic-offset 4)

;;
;; stroustrup gnu linux k&r bsd ...
  (c-set-style "stroustrup")
  (my-c-preprocess)
;;  (my-c-complete)
  (my-c-keybind)
  (my-c-compile)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cyliu c style
;; http://www.dotemacs.de/dotfiles/IngoKoch/IngoKoch.emacs-gnu-all.html
(defun my-c-style
  (add-c-sytle "cyliu-c" cyliu-c-style t))    

(defconst cyliu-c-style
  '((c-tab-always-indent        . t)
    (c-basic-offset             . 4)
    (c-comment-only-line-offset . (0 . 0))
    (c-hanging-braces-alist     . ((defun-open          before after)
                                   (defun-close         before after)  
                                   (class-open          before after)  
                                   (class-close         before after)  
                                   (brace-list-open     before after)  
                                   (brace-list-close    before after)  
                                   (block-open          before after)  
                                   (block-close         before after)  
                                   (substatement-open   before after)  
                                   (statement-case-open before after)  
                                   )) 
    (c-offsets-alist            . ((statement-block-intro . +)
				   (knr-argdecl-intro     . +)
				   (substatement-open     . 0)
				   (inline-open           . 0)
				   (label                 . 0)
				   (statement-case-open   . +)
				   (case-label            . +)
				   (statement-cont        . +)
				   (arglist-intro         . c-lineup-arglist-intro-after-paren)
				   (arglist-close         . c-lineup-arglist)
				   ))
    (c-echo-syntactic-information-p . t)
    ) 
  "cyliu C Programming Style")

(defun my-c-keybind ()
  (define-key c-mode-base-map (kbd "<return>") 'newline-and-indent)
  (define-key c-mode-base-map (kbd "M-.") 'semantic-ia-complete-symbol-menu) ;; completion
  (define-key c-mode-base-map (kbd "<f3>") 'cscope-find-global-definition)
  (define-key c-mode-base-map (kbd "<f4>") 'cscope-find-functions-calling-this-function)
  (define-key c-mode-base-map (kbd "<f2>") 'cscope-find-this-symbol)
  (define-key c-mode-base-map (kbd "<f5>") 'cscope-pop-mark)
  (define-key c-mode-base-map (kbd "C-TAB") 'my-indent-or-complete)
  )

(defun my-c-compile ()
  (setq compile-command "make")
  (define-key c-mode-base-map (kbd "<f7>") 'compile)
  )

(defun my-c-preprocess ()
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "cpp")
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)
  (setq hs-minor-mode t)
  (setq abbrev-mode t)
  )

;; (defun my-c-complete ()
;;   (setq semanticdb-project-roots 
;; 	(list
;; 	 (expand-file-name "/")))
;;   )

(defun my-indent-or-complete ()
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    (indent-for-tab-command))
  )

(autoload 'senator-try-expand-semantic "senator")
(setq hippie-expand-try-functions-list
      '(
	senator-try-expand-semantic
	try-expand-dabbrev
	try-expand-dabbrev-visible
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-expand-list
	try-expand-list-all-buffers
	try-expand-line
        try-expand-line-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill
        )
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add my customized hook to c-mode hook
(add-hook 'c-mode-common-hook 'my-c-mode-hook)
(setq comment-style 'extra-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; msf-abbrev settings
(require 'msf-abbrev)
(setq msf-abbrev-verbose t)
(setq msf-abbrev-root (concat CFGHOME "/config/mode-abbrevs"))
;;(msf-abbrev-load)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autopair setting
(require 'autopair)
(add-hook 'c-mode-common-hook #'(lambda () (autopair-mode)))
(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete setting
(add-to-list 'load-path (concat EHOME "/lisp/auto-complete/"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat EHOME "/ehome/lisp/auto-complete/ac-dict"))
(ac-config-default)
(setq ac-disable-faces nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pymacs setting
;; Python mode hook
(defun my-python-hook-mode ()
  (interactive)
  ;(setq ropemacs-enable-shortcuts nil)
  ;(setq ropemacs-local-prefix "C-c C-p")
  (require 'pymacs)
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-enable-shortcuts nil)
  (setq ropemacs-local-prefix "C-c C-p")

  (ac-ropemacs-setup)
  ;(pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil)
  (ropemacs-mode t)
  (define-key python-mode-map "\C-m" 'newline-and-indent)
  ;(setq ac-sources (append ac-sources '(ac-source-ropemacs)))
  )
;;(add-hook 'python-mode-hook 'my-python-hook-mode)

(provide 'cyliu-ide)
