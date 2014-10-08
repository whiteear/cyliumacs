;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; configuration for org mode
;;

(defvar org-home-dir (concat WORKDOC "/org"))
(defvar org-pub-dir (concat WORKDOC "/html/org"))

(defun org-get-src-path (name)
  (concat org-home-dir "/" name))

(defun org-get-home-path()
  org-home-dir)

(defun org-get-pub-path()
  org-pub-dir)

(defun generate-org-proj (pname)
  (if (stringp pname)
      (list pname
            :base-directory org-home-dir
            :publishing-directory org-pub-dir
            :section-numbers nil
            :table-of-contents nil
            :style "<link rel=stylesheethref=\"../css/core.css\"type=\"text/css\">"
            )))

(require 'org-install)
(setq org-agenda-files 
      (list (org-get-src-path "work")
            (org-get-src-path "stock")
            (org-get-src-path "life")
            ))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done '(done))

(setq org-directory org-home-dir)
(setq org-default-notes-file (org-get-src-path ".notes"))

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; org project
;;(setq org-publish-project-alist
       ;; '(("org"
       ;;    :base-directory org-home-dir
       ;;    :publishing-directory "~/org/public_html"
       ;;    :section-numbers nil
       ;;    :table-of-contents nil
      ;;          :style "<link rel=stylesheethref=\"../css/core.css\"type=\"text/css\">")))
  ;;    '((generate-org-proj "org")))
(setq org-publish-project-alist (list (generate-org-proj "org")))



(appt-activate t)
(setq appt-display-format 'window)
(add-hook 'diary-hook 'appt-make-list)

;. org to appt
(setq appt-display-duration 30)
(setq appt-audible t)
(setq appt-display-mode-line t)
;;(appt-activate 1)
(setq appt-msg-countdown-list '(10 0))
;(org-agenda)
;(org-agenda-to-appt)

(provide 'cyliu-org)
;; end of cyliu-org

