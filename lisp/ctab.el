;;; ctab.el --- Display a new tab bar in the header line

;; Copyright (C) 2011 Wu Xi

;; Author: Wu Xi <wuxi.cn@gmail.com>
;; Created: Feb 1, 2011
;; Keywords: convenience
;; Revision: $Id$

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides a minor mode to display tabs in the header
;; line.
;;
;; INSTALL:
;;   Copy linum.el to your load-path and add to your .emacs:
;;
;;     (require 'ctab)
;;     (ctab-mode t)
;;     (setq ctab-smart t) ;; smart inserting, use as wish
;;
;; SHORTCUT KEYS:
;;   M-<right>  `ctab-next'      Select next tab.
;;   M-<left>   `ctab-previous'  Select previous tab.
;;
;;
;; Toggle display of ctab in header-line with M-x `ctab-mode'.
;;

;;; Changelist:
;; * v0.2.0, 2011.02.14 by wuxi: implement smart inserting.
;;

;;; Code:

(defconst ctab-version "0.2.0")

(defgroup ctab nil
  "Display a tab bar for normal buffers in the header line."
  :group 'convenience)


;;;
;;; Faces: {{{

(defface ctab-default-face
  '((t (:inherit default
                 :foreground "white"
                 :background "grey20"
                 :underline t)))
  "Default face used in the tab bar."
  :group 'ctab)

(defface ctab-head-face
  '((t (:inherit ctab-default-face
                 :background "black")))
  "Face for displaying head of cTab bar."
  :group 'ctab)

(defface ctab-current-face
  '((t (:inherit ctab-default-face
                 :foreground "brightwhite"
                 :background "black"
                 :underline nil)))
  "Face for displaying tab of current buffer."
  :group 'ctab)

;;; face }}}

;;;
;;; Implementation: {{{

(defvar ctab-smart nil
  "Smart inserting for new tabs if it's non-nil.")

(defvar ctab-tabset nil
  "The all tabs.")

(defvar ctab-index 0
  "Index of current buffer in tabset.")

(defvar ctab-old-header nil
  "Old default-value of `header-line-format'.")

(defvar ctab-offset 0
  "Offset of header line.")

(defvar ctab-deleted-tab nil
  "Deleted tab.")

(defun ctab-filter (bname)
  "Filter buffer names. Return t if `bname' can go into tabset."
  (let ((c (substring bname 0 1)))
    (or (string= bname "*Messages*")
        (string= bname "*scratch*")
        (not (or (string= c "*") (string= c " "))))))

(defun ctab-in-tabset (bname)
  "Check if `bname' is in ctab-tabset."
  (member bname ctab-tabset))

(defun ctab-remove (bname)
  "Remove tab from tabset which name is `bname'."
  (if (equal bname (car ctab-tabset))
      ;; delete first element
      (pop ctab-tabset)
    ;; else:
    (delete bname ctab-tabset)))

(defun ctab-insert-at (bname tabset)
  "Insert `bname' before or after `cur' according to the extention of `bname'
 and `cur'."
  (let ((ext (file-name-extension bname))
        (cur-ext (file-name-extension (car tabset)))
        tmp-set)
    ;; change ".h" into no extension, so that .h file can come first
    (when (or (not ext) (string= (downcase ext) "h"))
      (setq ext ""))
    (when (or (not cur-ext) (string= (downcase cur-ext) "h"))
      (setq cur-ext ""))
    ;; insert:
    (if (string< ext cur-ext)
        ;; insert before:
        (progn
          (setq tmp-set (cons (car tabset) (cdr tabset)))
          (setcar tabset bname)
          (setcdr tabset tmp-set))
      ;; else: insert after:
      (setq tmp-set (cons bname (cdr tabset)))
      (setcdr tabset tmp-set))))

(defun ctab-smart-insert (bname tabset)
  "Insert `bname' into tabset at the RIGHT place it needs to be.
The `right' place is: if bname=='file.cpp', then the right place is the place
next to 'file.h'."
  (let ((name (file-name-sans-extension bname)) cur-name inserted)
    (while tabset
      ;; get name of current tab
      (setq cur-name (car tabset))
      ;; compare filename without extension:
      (if (string= name (file-name-sans-extension cur-name))
          (progn
            ;; insert `bname' before or after `cur':
            (ctab-insert-at bname tabset)
            (setq tabset nil)
            (setq inserted t))
        ;; else:
        (setq tabset (cdr tabset))))
    ;; insert at end if no similar tab found:
    (when (and (not inserted) (ctab-filter bname))
      (add-to-list 'ctab-tabset bname t))))

(defun ctab-insert (bname)
  "Insert `bname' into tabset.
If `ctab-smart' is non-nil, insert bname into the right place it needs to be."
  (if (not ctab-smart)
      ;; normal inserting: append
      (when (ctab-filter bname)
        (add-to-list 'ctab-tabset bname t))
    ;; else: smart inserting:
    (ctab-smart-insert bname ctab-tabset)))

(defsubst ctab-free-tabset ()
  "Free the tab set store."
  (setq ctab-tabset nil
        ctab-index nil))

(defun ctab-init-tabset ()
  "Init tabset of ctab."
  ;; release previous tabset:
  (ctab-free-tabset)
  ;; reset offset
  (setq ctab-offset 0)
  ;; insert all existing buffer into tabset:
  (dolist (bname (mapcar (function buffer-name) (buffer-list)))
    (ctab-insert bname)))

;; format tab line and get offset:
(defun ctab-format-line (list curbuf)
  "Format output line for `header-line-format'.
Returns a list: (head-str line-str begin-offset end-offset), if curbuf
 is in list; otherwise, returns nil."
  (let ((n 0) (tabset list) index (offset 0) beg end (str "") line)
    (dolist (buf tabset)
      ;; compute offset:
      (setq offset (+ offset (length str)))
      ;; format tab:
      (if (string= buf curbuf)
          ;; format tab of current buffer:
          (progn
            ;; save current tab index:
            (setq index n)
            ;; format string:
            (setq str (propertize (format "|%s|" buf)
                                  'face 'ctab-current-face))
            ;; set offsets:
            (setq beg offset)
            (setq end (+ offset (length str))))
        ;; else: format normal tab:
        (setq str (propertize (format " %s " buf)
                              'face 'ctab-default-face)))
      ;; append to line:
      (setq line (concat line str))
      ;; count tabs:
      (setq n (1+ n)))

    ;; check if curbuf is in list:
    (when index
      ;; `curbuf' is in `list':
      ;; set new index:
      (setq ctab-index index)
      ;; return the result list: (head-str line-str begin-offset end-offset)
      (list
       ;; head string: "[i/n]"
       (propertize (format "[%d/%d]:" (1+ index) n) 'face 'ctab-head-face)
       ;; line string:
       line
       ;; begin-offset:
       beg
       ;; end-offset:
       end)
      )
    )
  )

(defun ctab-compute-offset (win-width head-width beg end)
  "Compute tab line offset according to line length and window width."
  (let ((width (- win-width head-width)) (offset (- end ctab-offset)))
    (cond
     ;; case 1: left-margin of current tab is out of display region:
     ((< beg ctab-offset) (setq ctab-offset beg))
     ;; case 2: right-margin of current tab is out of display region:
     ((> offset width) (setq ctab-offset
                             (+ ctab-offset (- offset width))))
     ;; case 3: current tab is in display region: do nothing
     )
))

(defun ctab-display-header ()
  "Insert current buffer into tabset, if current buffer is not in tabset, and
 then display the header-line."
  ;; delete tab if needed:
  (when ctab-deleted-tab
    (ctab-remove ctab-deleted-tab)
    (setq ctab-deleted-tab nil))

  ;; insert buffer if needed:
  (let ((bname (buffer-name)))
    (unless (ctab-in-tabset bname)
      ;; insert buffer-name into tabset:
      (ctab-insert (buffer-name))))

  ;; display:
  (let ((line (ctab-format-line ctab-tabset (buffer-name))))
    (if line
        ;; non-nil, display ctab in header:
        (progn
          ;; move display offset:
          (ctab-compute-offset
           (window-width) (length (car line)) (nth 2 line) (nth 3 line))
          ;; output:
          (concat (car line) (substring (nth 1 line) ctab-offset)))
      ;; else: nil, display orginal header:
      ctab-old-header)))

(defsubst ctab-move (index)
  "Move buffer to index-th buffer in tabset."
  (when (ctab-in-tabset (buffer-name))
    (switch-to-buffer (nth index ctab-tabset))))

;;; public function:
;;;###autoload
(defun ctab-previous ()
  "Move to previous buffer."
  (interactive)
  (if (> ctab-index 0)
      (ctab-move (1- ctab-index))
    (ctab-move (1- (length ctab-tabset)))))

;;; public function:
;;;###autoload
(defun ctab-next ()
  "Move to next buffer."
  (interactive)
  (ctab-move (% (1+ ctab-index) (length ctab-tabset))))

;;; Bind/unbind shortcut keys:
(defvar ctab-old-key-next nil "Old key-binding of `M-<right>'.")
(defvar ctab-old-key-pre nil "Old key-binding of `M-<left>'.")

(defun ctab-bind-keys ()
  "Bind shortcut keys for moving."
  ;; save old key-bindings:
  (setq ctab-old-key-next (global-key-binding (kbd "M-<right>")))
  (setq ctab-old-key-pre (global-key-binding (kbd "M-<left>")))
  ;; bind keys:
  (define-key global-map (kbd "M-<right>") 'ctab-next)
  (define-key global-map (kbd "M-<left>") 'ctab-previous))

(defun ctab-unbind-keys ()
  "Unbind shortcut keys."
  (define-key global-map (kbd "M-<right>") 'ctab-old-key-next)
  (define-key global-map (kbd "M-<left>") 'ctab-old-key-pre))

;;; hook:
(defun ctab-kill-buffer-hook ()
  "Hook run just before actually killing a buffer."
  (setq ctab-deleted-tab (buffer-name))
  t)

(defconst ctab-header-line '(:eval (ctab-display-header))
  "The tab bar header line format.")

;;;###autoload
(define-minor-mode ctab-mode
  "Toggle display of tabs in the header line."
  :global t
  :group 'ctab
  ;; body:
  (if ctab-mode
      ;; ON:
      (progn
        (message "ctab-mode: ON")
        (unless (eq header-line-format ctab-header-line)
          ;; save current default-value of `header-line-format'.
          (setq ctab-old-header (default-value 'header-line-format))
          ;; add hook:
          (add-hook 'kill-buffer-hook 'ctab-kill-buffer-hook)
          ;; bind keys:
          (ctab-bind-keys)
          ;; init:
          (ctab-init-tabset)
          (setq-default header-line-format ctab-header-line))
        ) ;; END OF progn

    ;; else: OFF:
    (message "ctab-mode: OFF")
    ;; restore previous `header-line-format' if needed:
    (when (eq (default-value 'header-line-format) ctab-header-line)
      ;; restore default-value of `header-line-format':
      (setq-default header-line-format ctab-old-header)
      ;; remove hook:
      (remove-hook 'kill-buffer-hook 'ctab-kill-buffer-hook)
      ;; unbind keys:
      (ctab-unbind-keys)
      ;; release vars:
      (ctab-free-tabset))
    ) ;; END OF 'else'
  )

;;; implementation }}}

(provide 'ctab)
;;; ctab.el ends here