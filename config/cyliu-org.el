;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; my org setting
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path  (concat CFGHOME "/site-lisp/org-mode/lisp"))
(add-to-list 'load-path (concat CFGHOME "/site-lisp/org-mode/contrib/lisp"))
(require 'org-checklist)

;; define org published html style
(defvar CYL-ORG-HTML-STYLE "<link rel=stylesheet href=\"css/orgmode.css\" type=\"text/css\">")

(defun cyliu-get-org-src-path (proj-name)
  (if (null proj-name)
      nil
    (concat ORGHOME "/" proj-name)))

(defun cyliu-get-org-pub-path (proj-name)
  (if (null proj-name)
      nil
    (concat PUBLISH-DIR "/" proj-name)))
    
;; open appt message function
(appt-activate t)
(setq appt-display-format 'window)
(add-hook 'diary-hook 'appt-make-list)

;; org to appt
(setq appt-display-duration 30)
(setq appt-audible t)
(setq appt-display-mode-line t)
;;(appt-activate 1)
(setq appt-msg-countdown-list '(10 0))
;(org-agenda)
;(org-agenda-to-appt)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cyliu org customization

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(defun cyl-org-keydef ()
  (define-key org-mode-map (kbd "C-c l") 'org-store-link)
  (define-key org-mode-map (kbd "C-c b") 'org-iswitchb)
  ;;(define-key org-mode-map "\C-c \C-o p" 'org-publish-current-project)
  )
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-M-r") 'org-remember)

(add-hook 'org-mode-hook 'cyl-org-keydef)
(setq org-log-done '(done))
(setq org-return-follows-link t)
(setq org-blank-before-new-entry
      '((heading . t) (plain-list-item . nil)))
      

;; OrgMode & Remember
(setq org-directory ORGHOME)
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(require 'org-publish)
(setq org-publish-project-alist 
      (list
       (list "plan-notes"
             :base-directory (cyliu-get-org-src-path "plan")
             :base-extension "org"
             :publishing-directory (cyliu-get-org-pub-path "plan")
             :recursive t
             :publishing-function 'org-publish-org-to-html
             :headline-levels 4             ; Just the default for this project.
             :auto-preamble t
             :style CYL-ORG-HTML-STYLE
             )
 
       (list "plan-static"
             :base-directory (cyliu-get-org-src-path "plan")
             :recursive t
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
             :publishing-directory (cyliu-get-org-pub-path "plan")
             :publishing-function 'org-publish-attachment)
       
       (list "plan" 
             :components (list "plan-notes" "plan-static"))
       
       (list "knowledge"
             :base-directory (cyliu-get-org-src-path "knowledge")
             :base-extension "org"
             :publishing-directory (cyliu-get-org-pub-path "knowledge")
             :recursive t
             :publishing-function 'org-publish-org-to-html
             :headline-levels 4             ; Just the default for this project.
             :auto-preamble t
             :style CYL-ORG-HTML-STYLE
             )
       
       (list "knowledge-static"
             :base-directory (cyliu-get-org-src-path "knowledge")
             :recursive t
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
             :publishing-directory (cyliu-get-org-pub-path "knowledge")
             :publishing-function 'org-publish-attachment)
       
       (list "knowledge" 
             :components (list "knowledge" "knowledge-static"))
       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org agenda files.
;; platform record all work related tasks.
;; life record all personal tasks.
;; refile is temporary task pool for remember
(setq org-agenda-files (list (concat ORGHOME "/plan/platform.org")
                             (concat ORGHOME "/plan/life.org")
                             (concat ORGHOME "/plan/refile.org")
                             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; task control
(setq org-todo-keywords 
      (quote ((sequence "TODO(t)" "START(s!)" "|" "DONE(d@/!)" "CANCEL(c@/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(o!)"))))

(setq org-tag-alist 
      (quote ((:startgroup)
              ("@Work" . ?W)
              ("@Life" . ?L)
              ("@Play" . ?P)
              (:endgroup)
              (:startgroup)
              ("NEXT" . ?N)
              ("CANCELLED" . ?C)
              ("WAITING" . ?T)
              ("DONE" . ?D)
              (:endgroup)
              ("@friend" . ?F)
              ("@vip" . ?V)
              ("@knowledge" . ?K)
              ("Linux")
              ("Emacs")
              ("Health")
              )))

(setq org-todo-state-tags-triggers
      (quote (("CANCEL" ("NEXT") ("WAITING") ("CANCELLED" . t))
              ("WAITING" ("NEXT") ("WAITING" . t))
              ("SOMEDAY" ("NEXT") ("CANCELLED") ("WAITING" . t))
              (done ("NEXT") ("WAITING") ("CANCELLED") ("DONE" . t))
              ("START" ("WAITING") ("CANCELLED") ("NEXT" . t)))))

;; org face customization
(setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
 ("STARTED" :foreground "blue" :weight bold)
 ("DONE" :foreground "forest green" :weight bold)
 ("WAITING" :foreground "orange" :weight bold)
 ("SOMEDAY" :foreground "magenta" :weight bold)
 ("CANCELLED" :foreground "forest green" :weight bold)
 ("QUOTE" :foreground "red" :weight bold)
 ("QUOTED" :foreground "magenta" :weight bold)
 ("APPROVED" :foreground "forest green" :weight bold)
 ("EXPIRED" :foreground "forest green" :weight bold)
 ("REJECTED" :foreground "forest green" :weight bold)
 ("OPEN" :foreground "blue" :weight bold))))

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize agenda view
(setq org-agenda-custom-commands
      (quote (("s" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
              ("w" "Tasks waiting on something" tags "WAITING/!" ((org-use-tag-inheritance nil)))
              ("r" "Refile New Notes and Tasks" tags "LEVEL=1+REFILE" ((org-agenda-todo-ignore-with-date nil)))
              ;;("N" "Notes" tags "NOTE" nil)
              ("n" "Next" tags "NEXT-WAITING-CANCELLED/!" nil))))

(setq org-use-fast-todo-selection t)

;; (require 'msf-abbrev)
;; (setq msf-abbrev-verbose t)
;; (setq msf-abbrev-root (concat CFGHOME "/config/mode-abbrevs"))
;; (msf-abbrev-load)

(setq org-insert-heading-respect-content t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org remember
(setq org-default-notes-file (concat ORGHOME "/plan/refile.org"))
(require 'remember)
(org-remember-insinuate)

;; Start clock if a remember buffer includes :CLOCK-IN:
(add-hook 'remember-mode-hook 'cyliu-start-clock-if-needed 'append)
(defun cyliu-start-clock-if-needed ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward " *:CLOCK-IN: *" nil t)
      (replace-match "")
      (org-clock-in))))

;; Keep clocks running
(setq org-remember-clock-out-on-exit nil)
;; C-c C-c stores the note immediately
(setq org-remember-store-without-prompt t)

;; I don't use this -- but set it in case I forget to specify a location in a future template
(setq org-remember-default-headline "Remember Tasks")
;; 3 remember templates for TODO tasks, Notes, and Phone calls
(setq org-remember-templates 
      (quote (("todo" ?t "* TODO %?
  %u
  %a" nil bottom nil)
              ("note" ?n "* %?                                        :NOTE:
  %u
  %a" nil bottom nil)
              ("phone" ?p "* PHONE %:name - %:company -                :PHONE:
  Contact Info: %a
  %u
  :CLOCK-IN:
  %?" nil bottom nil)
              ("mail" ?m "* MAIL: %:who - %:title -                   :MAIL:
  TITLE: %a
  :CLOCK-IN:
  %?" nil bottom nil))))

; Use IDO for target completion
(setq org-completion-use-ido t)

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; task estimates and column view
; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
; global Effort estimate values
(setq org-global-properties (quote (("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 8:00"))))
