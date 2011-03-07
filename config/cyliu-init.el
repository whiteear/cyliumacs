;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; emacs initialization 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debug when find errors
(setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boot message
(setq inhibit-startup-message t)
(setq initial-scratch-message "Hey, Chunyang Liu,\n  What a great day, it is! Have fun and be happy!\n:-)\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global font lock, highlight syntax
(global-font-lock-mode t)
;; delete whole line if invoke C-k at the start of line
(setq kill-whole-line t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu bar, tool bar
(tool-bar-mode 0)
(menu-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default directory, no auto backup files.
(setq default-directory WORKSPACE)
(setq make-backup-files nil)

;; Set major mode to text mode by default
(setq default-major-mode 'text-mode)
;; Turn on auto-fill-mode by default in unadorned files.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; using dired to find files.
(setq find-file-run-dired t)
(setq inhirit-startup-message t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coding-system
(set-language-environment 'utf-8)
(setq local-coding-system 'utf-8-unix)
(setq current-language-environment "utf-8-unix")
(prefer-coding-system 'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display time on mode bar
(setq display-time-24hr-format t)
(display-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kill-ring
;; (setq kill-ring-max 150)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert formated time string into current point
(global-set-key (kbd "<f8>") 'insert-timestamp-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enabel interactive with other apps.
(setq x-select-enable-clipboard t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; session
;;(require 'session)
;;(add-hook 'after-init-hook 'session-initialize)
;;读取默认desktop设置
;;(desktop-load-default)
;;读取当前目录保存的desktop设置
;;(desktop-read)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some favorite setting
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode nil)
(show-paren-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globak key bindings
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-o") 'occur)

;; COMMENT OR UNCOMMENT REGION
;; (global-set-key [f1] 'comment-region)
;; (global-set-key [f2] 'uncomment-region)
;; (define-key global-map (kbd "C-/") 'comment-region)
;; (define-key global-map (kbd "C-\\") 'uncomment-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set default tab width to 4 and instead TABs with blank.
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting for ispell
;; (setq ispell-dictionary "american")
;; (add-hook 'text-mode-hook 'flyspell-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show tab and other wide-space
(require 'show-wspace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; template when create file.
;; (require 'template)
;; (defvar template-dir 
;;   (cons (concat CFGHOME "/config/template")
;;         nil))
;; (setq template-default-directories template-dir)
;; (template-initialize nil)

(require 'cyliu-util)
(global-set-key (kbd "C-c m l") 'cyliu-mark-line)
(global-set-key (kbd "C-c m w") 'cyliu-mark-word)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ibuffer settings. ibuffer is a dired like buffer manager
(global-set-key (kbd "C-x C-b") 'ibuffer)
(require 'ibuf-ext)
(setq ibuffer-mode-hook
      (lambda ()
          (setq ibuffer-filter-groups
                '(
                  ("*buffer*" (name . "\\*.*\\*"))
                  ("dired" (mode . dired-mode))
                  ("perl" (or (mode . cperl-mode)
                              (mode . sepia-mode)
                              (mode . perl-mode)))
                  ("elisp" (or (mode . emacs-lisp-mode)
                               (mode . lisp-interaction-mode)))
                  ("dev" (or (mode . c++-mode)
                             (mode . c-mode)
                             (mode . java-mode)
                             (mode . python-mode)
                             (mode . shell-script-mode)
                             (mode . diff-mode)))
                  ("tags" (name . "^TAGS"))
                  ("org" (or (mode . org-mode)
                             (mode . muse-mode)
                             ))
                   ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido settting
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-file-extensions-order '(".org" ".txt" ".py" ".c" ".h" ".cpp" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
(ido-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tar the marked files in dired or untar a tar files
(defun ywb-dired-compress-dir ()
  (interactive)
  (let ((files (dired-get-marked-files t)))
    (if (and (null (cdr files))
             (string-match "\\.\\(tgz\\|tar\\.gz\\)" (car files)))
        (shell-command (concat "tar -xvf " (car files)))
      (let ((cfile (concat (file-name-nondirectory
                            (if (null (cdr files))
                                (car files)
                              (directory-file-name default-directory))) ".tgz")))
        (setq cfile
              (read-from-minibuffer "Compress file name: " cfile))
        (shell-command (concat "tar -zcf " cfile " " (mapconcat 'identity files " ")))))
    (revert-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; goto marching paren
(defun his-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (self-insert-command (or arg 1))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cvs settings
(setq vc-diff-switches "-U4")
(setq vc-cvs-diff-switches "-u4")
(put 'narrow-to-region 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired setttings, use extrernal prog to open files
(require 'dired-x)
(dolist (file '(("evince" "\\.pdf$")
                ("xdvi" "\\.dvi$")
                ("gv" "\\.ps$" "\\.eps$")
                ("unrar e" "\\.rar$")
                ("gnochm" "\\.chm$")
                ("mplayer" "\\.avi$" "\\.mpg$" "\\.wma$" )
                ("eog" "\\.gif$" "\\.jpg$" "\\.tif$" "\\.jpeg$")
                ("ooffice" "\\.doc$" "\\.ods$" "\\.odt$" "\\.ppt$" "\\.xls$")
                ("gnumeric" "\\.xls$")
                ("htmlview" "\\.html$" "\\.htm$" "\\.mht$")))
  (dolist (suf (cdr file))
    (let ((prg (assoc suf dired-guess-shell-alist-default)))
      (if prg
          (setcdr prg (delete-dups (cons (car file) (cdr prg))))
        (add-to-list 'dired-guess-shell-alist-default (list suf (car file)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight-symbol 
;; this extension is realy usefull, expecially when you are coding
;; it looks like a feature in eclipse, automatically highlight the symbol
;; user select. 
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [(meta n)] 'highlight-symbol-next)
(global-set-key [(meta p)] 'highlight-symbol-prev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify the buffer name
(require 'uniquify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight KEY words.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scroll smoothly, one line each time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun make-some-files-read-only ()
  "when file opened is of a certain mode, make it read only"
  (when (memq major-mode '(c-mode c++-mode python-mode 
                                  shell-script-mode text-mode
                                  emacs-lisp-mode tcl-mode java-mode
                                  org-mode xml-mode))
    (toggle-read-only 1)))

(add-hook 'find-file-hooks 'make-some-files-read-only)

(provide 'cyliu-init)
