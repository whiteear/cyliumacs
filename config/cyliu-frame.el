;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; frame configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config frame title
(setq frame-title-format "%n%F@cyliu  %f")

;; use color-theme instead
(defun cyliu-init-color-theme()
  (progn
    (add-to-list 'load-path (concat CFGHOME "/site-lisp/color-theme-6.6.0"))
    (require 'color-theme)
    (color-theme-initialize)
    ;;(color-theme-classic)
    ;;(color-theme-dark-laptop)
    ;;(color-theme-gnome2)
    (color-theme-blackboard)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn transient-mark-mode
(transient-mark-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define fontset
(defun cyliu-create-fontset ()
  (progn
    (create-fontset-from-fontset-spec
     (concat
      "-unknown-Monaco-normal-normal-normal-*-14-*-*-*-m-0-fontset-monacoset,"
      "ascii:-unknown-Monaco-normal-normal-normal-*-14-*-*-*-m-0-fontset-auto1,"
      ;;"han:-microsoft-microsoft yahei-bold-r-normal--0-0-0-0-p-0-gb18030.2000-0,"
      "han:-wenquanyi-wenquanyi bitmap song-medium-r-normal--0-0-100-100-p-0-gbk-0,"
      ;;"chinese-gbk:-bitstream-bitstream charter-medium-i-normal--14-0-0-0-p-0-iso10646-1,"
      ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; maximum frame
(global-set-key (kbd "<f11>") 'cyliu-frame-maximize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tarbar mode
(defun cyliu-init-tabbar-mode ()
  (progn
    (require 'tabbar)
    ;;(tabbar-speedkey-use t)
    (setq tabbar-speedkey-prefix (kbd "<f1>"))
    (tabbar-mode 1)
    (global-set-key (kbd "<C-left>") 'tabbar-backward-tab)
    (global-set-key (kbd "<C-right>") 'tabbar-forward-tab)
    (global-set-key (kbd "<C-up>") 'tabbar-forward-group)
    (global-set-key (kbd "<C-down>") 'tabbar-forward-group)
    (setq tabbar-buffer-groups-function 'cyliu-tabbar-buffer-groups)
    ))

(defun cyliu-init-ctab-mode ()
  (progn
    (require 'ctab)
    (ctab-mode t)
    (setq ctab-smart t)))

(defface cyliu-tabbar-selected-face
  '(
    (t
     (:inherit tabbar-default-face
               :box (:line-width 3 :color "white" :style released-button)
               :foreground "red"
               )
     )
    )
     "Face used for selected tab."
     :group 'tabbar)


(defun cyliu-tabbar-buffer-groups (buffer)
  "Return the list of group names BUFFER belongs to.
Return only one group for each buffer."
  (with-current-buffer (get-buffer buffer)
    (cond
     ((string-equal "*" (substring (buffer-name) 0 1))
      '("Common")
      )
     ((eq major-mode 'dired-mode)
      '("Dired")
      )
     ((memq major-mode
            '(rmail-mode
              rmail-edit-mode vm-summary-mode vm-mode mail-mode
              mh-letter-mode mh-show-mode mh-folder-mode
              gnus-summary-mode message-mode gnus-group-mode
              gnus-article-mode score-mode gnus-browse-killed-mode))
      '("Mail")
      )
     ((memq major-mode
            '(c-mode c++-mode java-mode
              emacs-lisp-mode python-mode
              shell-script-mode awk-mode diff-mode shell-mode xml-mode))
      '("Dev"))
     (t
      '("Others"))
      ;; (list
      ;;  "default" ;; no-grouping
      ;;  (if (and (stringp mode-name) (string-match "[^ ]" mode-name))
      ;;      mode-name
      ;;    (symbol-name major-mode)))
      ;; )
     )))

(defface cyliu-underline-face
  '((((class color) (background dark)) (:underline t)) (t ()))
  "cyliu hl-mode customized face")

(require 'hl-line)
(setq hl-line-face 'cyliu-underline-face)   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell color setting
(setq ansi-color-for-comint-mode t)
(customize-group 'ansi-colors)
(kill-this-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; face
(defun color-theme-pmade ()
  (interactive)
  (custom-set-faces
   ;; Outline Mode and Org-Mode
   '(outline-1 ((t (:foreground "#D6B163" :bold t))))
   '(outline-2 ((t (:foreground "#A5F26E" :bold t))))
   '(outline-3 ((t (:foreground "#B150E7" :bold nil))))
   '(outline-4 ((t (:foreground "#529DB0" :bold nil))))
   '(outline-5 ((t (:foreground "#CC7832" :bold nil))))
   '(org-level-1 ((t (:inherit outline-1))))
   '(org-level-2 ((t (:inherit outline-2))))
   '(org-level-3 ((t (:inherit outline-3))))
   '(org-level-4 ((t (:inherit outline-4))))
   '(org-level-5 ((t (:inherit outline-5))))
   '(org-agenda-date ((t (:inherit font-lock-type-face))))
   '(org-agenda-date-weekend ((t (:inherit org-agenda-date))))
   '(org-scheduled-today ((t (:foreground "#ff6ab9" :italic t))))
   '(org-scheduled-previously ((t (:foreground "#d74b4b"))))
   '(org-upcoming-deadline ((t (:foreground "#d6ff9c"))))
   '(org-warning ((t (:foreground "#8f6aff" :italic t))))
   '(org-date ((t (:inherit font-lock-constant-face))))
   '(org-tag ((t (:inherit font-lock-comment-delimiter-face))))
   '(org-hide ((t (:foreground "#191919"))))
   '(org-todo ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))
   '(org-done ((t (:background "DarkGreen" :foreground "white" :box (:line-width 1 :style released-button)))))
   '(org-column ((t (:background "#222222"))))
   '(org-column-title ((t (:background "DarkGreen" :foreground "white" :bold t :box (:line-width 1 :style released-button)))))

   ;; Diff Mode
   '(diff-added ((t (:foreground "#d7ffaf"))))
   '(diff-changed ((t (:foreground "#ffc28d"))))
   '(diff-removed ((t (:foreground "#ff9999"))))
   '(diff-indicator-added ((t (:background "#d7ffaf" :foreground "#000000"))))
   '(diff-indicator-chnaged ((t (:background "#ffc28d" :foreground "#000000"))))
   '(diff-indicator-removed ((t (:background "#ff9999" :foreground "#000000"))))
   '(diff-context ((t (:foreground "#888888"))))

   ;; Modeline and Things in the Modeline
   '(modeline ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))
   '(mode-line-inactive ((t (:background "#4D4D4D" :foreground "#FFFFFF" :box (:line-width 1 :style released-button)))))
   '(modeline-buffer-id ((t (:background "DarkRed" :foreground "white"))))
   '(modeline-mousable ((t (:background "DarkRed" :foreground "white"))))
   '(modeline-mousable-minor-mode ((t (:background "DarkRed" :foreground "white"))))
   '(window-number-face ((t (:foreground "#FF7777"))))
   ))

(defun color-theme-pmade-gui ()
  (color-theme-pmade)
  (custom-set-faces
   '(default ((t (:background "#191919" :foreground "#FFFFFF"))))))

(defun color-theme-pmade-terminal ()
  (color-theme-pmade)
  (custom-set-faces
   '(default ((t (:background nil :foreground "brightwhite"))))
   '(font-lock-variable-name-face ((t (:foreground "blue"))))
   '(font-lock-string-face ((t (:foreground "green"))))
   '(font-lock-builtin-face ((t (:foreground "blue" :bold t))))
   '(font-lock-constant-face ((t (:foreground "cyan"))))
   '(font-lock-type-face ((t (:foreground "green" :bold t))))
   '(show-paren-match ((t (:background "red" :foreground "yellow" :bold t))))
   '(font-lock-comment-delimiter-face ((t (:foreground "white"))))
   '(font-lock-comment-face ((t (:italic t :foreground "magenta"))))))

;;(color-theme-pmade-gui)

(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font))
      nil t))

(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size) 
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-size)
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl)                         ; for find if
  (let ((en-font (qiang-make-font-string
                  (find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
                            :size chinese-font-size)))
 
    ;; Set the default English font
    ;; 
    ;; The following 2 method cannot make the font settig work in new frames.
    ;; (set-default-font "Consolas:pixelsize=18")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
    ;; We have to use set-face-attribute
    (message "Set English Font to %s" en-font)
    (set-face-attribute
     'default nil :font en-font)
 
    ;; Set Chinese font 
    ;; Do not use 'unicode charset, it will cause the english font setting invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        zh-font))))

(if (cyliu-is-windows)
    (progn
      ;; For Windows
      (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
      (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
      )
  (progn
    ;; For Linux
    (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
    (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open the file in read-only mode.
(defun make-some-files-read-only ()
  "when file opened is of a certain mode, make it read only"
  (when (memq major-mode '(c++-mode tcl-mode text-mode python-mode emacs-list-mode lisp-mode c-mode java-mode shell-script-mode))
    (toggle-read-only 1)))

(add-hook 'find-file-hooks 'make-some-files-read-only)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set preferences which only apply to X window
(if (cyliu-is-gui-enabled)
    (progn
      (cyliu-init-color-theme)
      (global-hl-line-mode t)
      (cyliu-init-tabbar-mode)
      (qiang-set-font
       '("Consolas" "Monaco" "DejaVu Sans Mono" "Monospace" "Courier New") ":pixelsize=16"
       '("Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))
      )
  (progn
    (cyliu-init-ctab-mode)))

(provide 'cyliu-frame)
