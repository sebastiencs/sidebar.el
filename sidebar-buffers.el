;;; sidebar-buffers.el --- sidebar-buffers

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/sidebar.el
;; Keywords: project, sidebar, projectile, file explorer
;; Version: 0.0.1
;; Package-Requires: ((dash "2.13.0") (projectile "0.11.0"))

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Major mode of the sidebar for buffers
;;

;;; Code:

(require 'dash)
(require 'sidebar-utils)
(require 'sidebar-filemapping)
(require 'icons-in-terminal)
(require 's)

(declare-function sidebar-find-file-from-line "ext:sidebar.el")
(declare-function sidebar-refresh "ext:sidebar.el")
(declare-function sidebar-init-mode "ext:sidebar.el")
(declare-function sidebar-open "ext:sidebar.el")
(declare-function sidebar-adjust-window-width "ext:sidebar.el")
(declare-function sidebar-reset-window-width "ext:sidebar.el")

(defvar buffers-contexts)

(defgroup sidebar-buffers nil
  "Sidebar mode to view a list of buffers."
  :group 'tools
  :group 'convenience
  :link '(custom-manual "(sidebar-buffers) Top")
  :link '(info-link "(sidebar-buffers) Customizing"))

(defcustom sidebar-buffers-modified-icon-all nil
  "When set, it makes print the modified icon on all buffers (others ...
and hidden included).
If nil, the icon will be insert only with visited buffers."
  :type 'boolean
  :group 'sidebar-buffers)

(defcustom sidebar-buffers-width 30
  "Width of the sidebar with buffers."
  :type 'integer
  :group 'sidebar-buffers)

(defcustom sidebar-buffers-show-hidden nil
  "If non-nil, show the list of hidden buffers."
  :type 'boolean
  :group 'sidebar-buffers)

(defface sidebar-buffers-headers-face
  '((t :foreground "#2196F3"
       :height 1.4))
  "Face used with headers."
  :group 'sidebar-buffers)

;; (defcustom sidebar-buffers-bookmark-icon 'fa_bookmark
;;   "Icon to use with bookmarks.
;; To get a list of the icons names, you can run:
;;  `~/.local/share/icons-in-terminal/print_icons.sh --names'
;; More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
;;   :type 'symbol
;;   :group 'sidebar-buffers)

(defun sidebar-buffers-item-builder (item)
  "Return an association list from ITEM.
Function similar to `sidebar-file-struct' adapted for buffers data."
  (list (cons 'data item)
	(cons 'type (cond ((bufferp item) 'buffer)
			  (t 'separator)))
	(cons 'visiting (and (-> item bufferp) (-> item buffer-file-name)))
	(cons 'line 0)))

(defsubst sidebar-buffers-hidden? (buffer)
  "Return non-nil if the BUFFER is hidden (start with a space)."
  (-> buffer buffer-name string-to-char (equal (elt " " 0))))

(defsubst sidebar-buffers-separator (name &optional first)
  "Return the header NAME between two separators.
if FIRST is non-nil, do not insert a separator before the header."
  (-flatten
   (list (unless first 'separator)
	 (propertize name 'face 'sidebar-buffers-headers-face)
	 'separator)))

(sidebar-content-provider buffers (&rest _)
  "Return a list of buffers to print in the sidebar.
The list will be mapped with `sidebar-buffers-item-builder' to make them
easily usable."
  (let* ((buffers (buffer-list))
	 (visiting (-filter 'buffer-file-name buffers))
	 (others (->> buffers (-remove 'buffer-file-name) (-remove 'sidebar-buffers-hidden?)))
	 (hidden (-filter 'sidebar-buffers-hidden? buffers)))
    (-concat
     (when (> (length visiting) 0)
       (-concat (sidebar-buffers-separator "Visiting buffers" t)
		visiting))
     (when (> (length others) 0)
       (-concat (sidebar-buffers-separator "Others buffers" (= (length visiting) 0))
		others))
     (when (and sidebar-buffers-show-hidden
		(> (length hidden) 0))
       (-concat (sidebar-buffers-separator "Hidden buffers")
		hidden)))))

(setq sidebar-buffers-show-hidden nil)
;; get-buffer-process

(defun sidebar-buffers-insert-icon (icon)
  "ICON."
  (concat " " (icons-in-terminal icon)))

(defun sidebar-buffers-format-name (buffer name visiting)
  "BUFFER NAME VISITING."
  (let ((read-only (with-current-buffer buffer buffer-read-only))
	(modified (and (buffer-modified-p buffer)
		       (or visiting sidebar-buffers-modified-icon-all))))
    (concat
     (if (not visiting)
	 (icons-in-terminal 'file_emacs :foreground "#607D8B")
       (-let (((&plist :icon icon :color color) (sidebar-filemapping-lookup name)))
	 (icons-in-terminal icon :foreground color)))
     " "
     (s-trim name)
     (when read-only (sidebar-buffers-insert-icon 'fa_lock))
     (when modified (sidebar-buffers-insert-icon 'md_whatshot)))))

(defun sidebar-buffers-print-item (item _)
  "Function to print ITEM in sidebar.
It doesn't print anything if ITEM is a separator.
ITEM is an object created with `sidebar-buffers-item-builder'."
  (-let (((&alist 'data data 'type type 'visiting visiting) item))
    (pcase type
      ('separator (insert (if (stringp data) data "")))
      (_ (insert (concat " " (sidebar-buffers-format-name data (buffer-name data) visiting)))))))

(defun sidebar-buffers-open-line ()
  "Open the maildir or bookmark on the current line."
  (interactive)
  (-let* (((&alist 'data data) (sidebar-find-file-from-line)))
    (message "Open buffer: %s" (buffer-name data))))

(defun sidebar-buffers-add-mark (buffer mark)
  "BUFFER MARK."
  (let ((list-marks (sidebar-get buffers-marks)))
    (if (not list-marks)
	(sidebar-set buffers-marks (list (cons buffer `(,mark))))
      (let ((found (alist-get buffer list-marks)))
	)
      )
    )
  )

(sidebar-set buffers-marks nil)
(sidebar-get buffers-marks)

(defun sidebar-buffers-mark-delete ()
  "."
  (interactive)
  (-let* (((buffer &as &alist 'data data) (sidebar-find-file-from-line))
	  (list-marks (sidebar-get buffers-marks)))
    (sidebar-buffers-add-mark data 'delete)
    ;; (add-to-list list-marks 'delete)
    ;; (message (format "%s" list-marks))
    )
  )

(defun sidebar-buffers? ()
  "Return non-nil if we have to use `sidebar-buffers-mode' on the sidebar creation."
  t)
;; (prog1 (sidebar-get buffers-force)
;;   (sidebar-set buffers-force nil)))

(defun sidebar-buffers-make-header ()
  "Return the string to insert in the sidebar header."
  (propertize (concat " Buffers list")
	      'display '(raise 0.12)))

(defun sidebar-buffers-make-modeline-left ()
  "Return the string to insert in the modeline (left side)."
  " buffers")

(defun sidebar-buffers-make-modeline-right ()
  "Return the string to insert in the modeline (right side)."
  "Buffers")

(defun sidebar-buffers-pre-command ()
  "See `sidebar-buffers-post-command'."
  (sidebar-set buffers-pre-line (line-number-at-pos)))

(defsubst sidebar-buffers-not-sep? (item)
  "Return non-nil if ITEM is NOT a separator."
  (not (equal 'separator (alist-get 'type item))))

(defun sidebar-buffers-jump-after (line)
  "LINE."
  (sidebar-goto-line (-some->> (--remove (<= (--getline it) line) (sidebar-get files))
			       (-first 'sidebar-buffers-not-sep?)
			       (--getline))))

(defun sidebar-buffers-jump-before (line)
  "LINE."
  (sidebar-goto-line (-some->> (--remove (>= (--getline it) line) (sidebar-get files))
			       (-last 'sidebar-buffers-not-sep?)
			       (--getline))))

(defun sidebar-buffers-post-command ()
  "Function to ensure that the cursor is never on a separator."
  (-when-let* ((pre-line (sidebar-get buffers-pre-line))
	       (line (line-number-at-pos))
	       ((&alist 'type type) (sidebar-find-file-from-line)))
    (when (equal type 'separator)
      (cond ((< line 3) (sidebar-goto-line 3))
	    ((< pre-line line) (sidebar-buffers-jump-after line))
	    (t (sidebar-buffers-jump-before line))))))

(defvar sidebar-buffers-mode-map nil
  "Keymap used with sidebar-buffers-mode.")
(unless sidebar-buffers-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q") 'sidebar-close)
    (define-key map (kbd "g") 'sidebar-refresh)
    (define-key map (kbd "d") 'sidebar-buffers-mark-delete)
    (define-key map (kbd "RET") 'sidebar-buffers-open-line)
    (define-key map (kbd "<right>") 'sidebar-adjust-window-width)
    (define-key map (kbd "<left>") 'sidebar-reset-window-width)
    (define-key map (kbd "?") 'sidebar-help)
    (setq sidebar-buffers-mode-map map)))

(define-derived-mode sidebar-buffers-mode nil "Sidebar-buffers"
  "Major mode for Sidebar-buffers.

\\{sidebar-buffers-mode-map}"
  ::group sidebar-buffers

  (make-local-variable 'post-command-hook)
  (make-local-variable 'pre-command-hook)

  (sidebar-init-mode)

  (add-hook 'pre-command-hook 'sidebar-buffers-pre-command nil)
  (add-hook 'post-command-hook 'sidebar-buffers-post-command nil)
  (add-hook 'post-command-hook 'sidebar-post-command t)
  (add-hook 'pre-command-hook 'sidebar-pre-command)
  (add-hook 'delete-frame-functions 'sidebar-delete-buffer-on-kill)
  (add-hook 'before-make-frame-hook 'sidebar-before-make-frame-hook)

  (sidebar-set-window))

(provide 'sidebar-buffers)

;;; sidebar-buffers.el ends here
