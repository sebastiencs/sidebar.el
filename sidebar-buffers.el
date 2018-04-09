;;; sidebar-buffers.el --- sidebar-buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/sidebar.el
;; Keywords: files, convenience, frames
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (dash "2.11.0") (projectile "0.10.0"))

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
(require 'sidebar-select)
(require 'sidebar-filemapping)
(require 'icons-in-terminal nil t)
(require 's)

(declare-function sidebar-find-file-from-line 'sidebar)
(declare-function sidebar-refresh 'sidebar)
(declare-function sidebar-init-mode 'sidebar)
(declare-function sidebar-open 'sidebar)
(declare-function sidebar-adjust-window-width 'sidebar)
(declare-function sidebar-reset-window-width 'sidebar)
(declare-function sidebar-list-windows-others-frame 'sidebar)
(declare-function sidebar-close 'sidebar)
(declare-function sidebar-switch-to-buffers 'sidebar)
(declare-function sidebar--getline 'sidebar)

(defvar sidebar-select-icon-before-window)
(defvar sidebar--streched-spaces-p)

(defgroup sidebar-buffers nil
  "Sidebar mode to view a list of buffers."
  :group 'tools
  :group 'convenience
  :group 'sidebar
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

(defcustom sidebar-buffers-action-after-open 'return-to-files
  "Action to execute when a buffer has been open after `sidebar-buffers-open'.

The following values are possible:

- `close' Close the sidebar.

- `return-to-files' If the sidebar was already open in the default
                    mode (listing files), it will return to that state.
                    If the sidebar wasn't open, it will close the sidebar.

- `nothing' Let the sidebar open.

Default: `return-to-files'."
  :type '(choice (const :tag "Close" close)
                 (const :tag "Return to files" return-to-files)
                 (const :tag "Nothing" nothing))
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

(defun sidebar-buffers-item-builder (item)
  "Return an association list from ITEM.
Function similar to `sidebar-file-struct' adapted for buffers data."
  (list (cons 'data item)
	    (cons 'type (if (bufferp item) 'buffer 'separator))
        (cons 'visiting (and (-> item bufferp) (-> item buffer-file-name)))
        (cons 'line 0)))

(defsubst sidebar-buffers-hidden-p (buffer)
  "Return non-nil if the BUFFER is hidden (start with a space)."
  (-> buffer buffer-name string-to-char (equal (elt " " 0))))

(defsubst sidebar-buffers-separator (name &optional project)
  "Return the header NAME between two separators.
if FIRST is non-nil, do not insert a separator before the header.
PROJECT non nil means to add an icon."
  (list
   (if project
       (concat (icons-in-terminal 'oct_repo :face 'sidebar-buffers-headers-face :height 0.9)
               (unless (display-graphic-p) " ")
               (propertize name 'face 'sidebar-buffers-headers-face))
     (propertize name 'face 'sidebar-buffers-headers-face))))

(defun sidebar-buffers-root-project (buffer)
  "BUFFER."
  (with-current-buffer buffer
    (-some->> (or (sidebar-get-lsp-root)
                  (ignore-errors (projectile-project-root)))
              directory-file-name
              file-name-nondirectory)))

(defun sidebar-buffers--sort-buffers-in-project (buffers)
  (--> (--map (cons it (sidebar-buffers--buffer-name it)) (cdr buffers))
       (--sort (string< (cdr it) (cdr other)) it)
       (cons (car buffers) (--map (car it) it))))

(sidebar-content-provider buffers (&rest _)
  "Return a list of buffers to print in the sidebar.
The list will be mapped with `sidebar-buffers-item-builder' to make them
easily usable."
  (let* ((buffers (buffer-list))
	     (visiting (-filter 'buffer-file-name buffers))
	     (others (->> buffers (-remove 'buffer-file-name) (-remove 'sidebar-buffers-hidden-p)))
	     (hidden (-filter 'sidebar-buffers-hidden-p buffers))
         (by-projects (-group-by 'sidebar-buffers-root-project visiting))
         (by-projects (-map 'sidebar-buffers--sort-buffers-in-project by-projects))
         (visiting (alist-get nil by-projects)))
    (-concat
     (-flatten (--map (and (setcar it (sidebar-buffers-separator (car it) t)) it)
                      (sort (--filter (car it) by-projects) (lambda (a b) (string< (car a) (car b))))))
     (when (> (length visiting) 0)
       (-concat (sidebar-buffers-separator "Visiting buffers")
		        visiting))
     (when (> (length others) 0)
       (-concat (sidebar-buffers-separator "Others buffers")
		        others))
     (when (and sidebar-buffers-show-hidden
		        (> (length hidden) 0))
       (-concat (sidebar-buffers-separator "Hidden buffers")
		        hidden)))))

(defun sidebar-buffers-insert-icon (&rest props)
  "Return icon with PROPS."
  (concat " " (apply 'icons-in-terminal props)))

(defun sidebar-buffers-insert-marks (buffer)
  "BUFFER."
  (-when-let (marks (alist-get buffer (sidebar-get buffers-marks)))
    (concat
     (when (member 'delete marks)
       (sidebar-buffers-insert-icon 'oct_trashcan :foreground "brown"))
     (when (member 'save marks)
       (sidebar-buffers-insert-icon 'md_save :foreground "sea green")))))

(defun sidebar-buffers-format-name (buffer name visiting)
  "BUFFER NAME VISITING."
  (let ((read-only (with-current-buffer buffer buffer-read-only))
	    (modified (and (buffer-modified-p buffer)
		               (or visiting sidebar-buffers-modified-icon-all))))
    (concat
     (if (not visiting)
	     (icons-in-terminal 'file_emacs :foreground "#607D8B")
       (-let* (((icon . color) (sidebar-filemapping-lookup name)))
	     (icons-in-terminal icon :foreground color)))
     ;; https://gist.github.com/sebastiencs/2f066f8d12b71f40fda9bdb979fe971d
     (propertize " " 'display '(space :re-align t))
     (when sidebar--streched-spaces-p
       (propertize " " 'display '(space :width 0.6)))
     (s-trim name)
     (when read-only (sidebar-buffers-insert-icon 'md_lock))
     (when modified (sidebar-buffers-insert-icon 'md_whatshot))
     (sidebar-buffers-insert-marks buffer))))

(defun sidebar-buffers--buffer-name (data)
  "DATA."
  (or (with-current-buffer data
        (and (bound-and-true-p lsp--cur-workspace)
             (fboundp 'lsp-ui--workspace-path)
             (lsp-ui--workspace-path (buffer-file-name data))))
      (buffer-name data)))

(sidebar-print-function buffers (item)
  "Function call to print a line.
ITEM is the object to print."
  (-let* (((&alist 'data data 'type type 'visiting visiting) item))
    (if (eq type 'separator)
        (not (overlay-put (make-overlay (point) (point))
                          'after-string (concat (propertize "\n" 'face '(:height 0.8))
                                                data
                                                (propertize "\n" 'face '(:height 1.0)))))
      (concat " " (sidebar-buffers-format-name data
                                               (sidebar-buffers--buffer-name data)
                                               visiting)
              "\n"))))

(defun sidebar-buffers-open-in-window2 (buffer)
  "Helper function for `sidebar-buffers-open-in-window'.
BUFFER."
  (interactive)
  (let* ((windows-in-frame (-remove 'window-dedicated-p (window-list)))
	     (windows-in-others-frame (sidebar-list-windows-others-frame (frame-list))))
    (sidebar-select-make-buffer (list windows-in-frame windows-in-others-frame)
				                " Select a window "
				                " Others frames "
				                (lambda (x) (s-chop-suffix ">" (s-replace "#<window " "#" (format "%s" x))))
				                sidebar-select-icon-before-window
				                'sidebar-open-file-in-window
				                buffer)))

(defun sidebar-buffers-open-in-window ()
  "Open BUFFER in a selected window.
A list of windows will be shown to the user to select the one in which to
open the buffer.
Only the windows non dedicated are shown."
  (interactive)
  (-let [(&alist 'data buffer) (sidebar-find-file-from-line)]
    (sidebar-buffers-open-in-window2 buffer)))

(defun sidebar-buffers-open-line ()
  "Open the buffer on the current line."
  (interactive)
  (-let* (((&alist 'data buffer) (sidebar-find-file-from-line))
	      (window (sidebar-get window-origin)))
    (if (window-live-p window)
	    (set-window-buffer window buffer)
      (sidebar-set window-origin (sidebar-buffers-open-in-window2 buffer))))
  (when (sidebar-get buffers-hide)
    (sidebar-close))
  (when (sidebar-get buffers-return-to-files)
    (sidebar-buffers-switch-to-files))
  (sidebar-set buffers-return-to-files nil)
  (sidebar-set buffers-hide nil))

(defun sidebar-buffers-add-mark (buffer mark)
  "BUFFER MARK."
  (let ((buffers-marks (sidebar-get buffers-marks)))
    (sidebar-set buffers-marks
      (cond
       ((not buffers-marks) (list (cons buffer (list mark))))
       ((alist-get buffer buffers-marks) (--map-when (equal (car it) buffer) (add-to-list 'it mark t) buffers-marks))
       (t (-concat buffers-marks (list (cons buffer (list mark)))))
       ))))

(defun sidebar-buffers-mark-execute ()
  "."
  (interactive)
  (-when-let (buffers-marks (sidebar-get buffers-marks))
    (--each buffers-marks
      (-when-let* ((buffer (car it))
		           (_ (buffer-live-p buffer)))
	    (when (member 'save it)
	      (with-current-buffer buffer
	        (save-buffer)))
	    (when (member 'delete it)
	      (kill-buffer buffer))))
    (sidebar-set buffers-marks nil)
    (sidebar-refresh)))

(defun sidebar-buffers-mark-delete ()
  "."
  (interactive)
  (-let* (((buffer &as &alist 'data data) (sidebar-find-file-from-line)))
    (sidebar-buffers-add-mark data 'delete)
    (sidebar-refresh)
    (forward-line)))

(defun sidebar-buffers-mark-save ()
  "."
  (interactive)
  (-let* (((buffer &as &alist 'data data) (sidebar-find-file-from-line)))
    (sidebar-buffers-add-mark data 'save)
    (sidebar-refresh)
    (forward-line)))

(defun sidebar-buffers-unmark ()
  "."
  (interactive)
  (-let* (((&alist 'data data) (sidebar-find-file-from-line))
	      (list-marks (sidebar-get buffers-marks)))
    (sidebar-set buffers-marks (--remove (equal data (car it)) list-marks))
    (sidebar-refresh)
    (forward-line)))

(defun sidebar-buffers? ()
  "Return non-nil if we have to use `sidebar-buffers-mode' on the sidebar creation."
  (prog1 (sidebar-get buffers-force)
    (sidebar-set buffers-force nil)))

(defun sidebar-buffers-make-header ()
  "Return the string to insert in the sidebar header."
  (concat
   " "
   (icons-in-terminal 'fa_list_ul :raise 0.12)
   (propertize " Buffers list"
	           'display '(raise 0.12))))

(defun sidebar-buffers-make-modeline-left ()
  "Return the string to insert in the modeline (left side)."
  nil)

(defun sidebar-buffers-make-modeline-right ()
  "Return the string to insert in the modeline (right side)."
  (concat
   (number-to-string (length (sidebar--list-all)))
   " Buffers"))

(defun sidebar-buffers-pre-command ()
  "See `sidebar-buffers-post-command'."
  (sidebar-set buffers-pre-line (line-number-at-pos)))

(defsubst sidebar-buffers-not-sep? (item)
  "Return non-nil if ITEM is NOT a separator."
  (not (equal 'separator (alist-get 'type item))))

(defun sidebar-buffers-jump-after (line)
  "LINE."
  (sidebar-goto-line (-some->> (--remove (<= (sidebar--getline it) line) (sidebar-get files))
			                   (-first 'sidebar-buffers-not-sep?)
			                   (sidebar--getline))))

(defun sidebar-buffers-jump-before (line)
  "LINE."
  (sidebar-goto-line (-some->> (--remove (>= (sidebar--getline it) line) (sidebar-get files))
			                   (-last 'sidebar-buffers-not-sep?)
			                   (sidebar--getline))))

(defun sidebar-buffers-toggle-hidden ()
  "."
  (interactive)
  (setq sidebar-buffers-show-hidden (not sidebar-buffers-show-hidden))
  (sidebar-refresh))

(defun sidebar-buffers-switch-to-files (&optional no-save-line)
  "NO-SAVE-LINE."
  (interactive)
  (unless no-save-line
    (sidebar-set buffers-line (line-number-at-pos)))
  (sidebar-set buffers-hide nil)
  (sidebar-set buffers-return-to-files nil)
  (ignore-errors (kill-buffer (sidebar-cons-buffer-name)))
  (sidebar-set mode-to-use 'sidebar-mode)
  (sidebar-open))

(defun sidebar-buffers-open ()
  "Open the sidebar with the list of buffers.
Once a buffer has been open with `sidebar-buffers-open-line',
it hides the sidebar or return to the files listing (customizable).
This behavior occurs only with this function (`sidebar-buffers-open')
followed by `sidebar-buffers-open-line'."
  (interactive)
  (pcase sidebar-buffers-action-after-open
    ('close (sidebar-set buffers-hide t)))
  (cond ((and (get-buffer (sidebar-cons-buffer-name)) (not (sidebar-get-window t)))
	     (progn (sidebar-set buffers-hide t)
		        (sidebar-switch-to-buffers)))
	    ((not (sidebar-get-window t))
	     (progn (sidebar-set buffers-force t)
		        (sidebar-set buffers-hide t)
		        (sidebar-open)))
	    ((equal (sidebar-get mode-to-use) 'sidebar-buffers-mode)
	     (sidebar-open))
	    (t (progn (pcase sidebar-buffers-action-after-open
		            ('return-to-files (sidebar-set buffers-return-to-files t)))
		          (sidebar-switch-to-buffers)))))

(defun sidebar-buffers-close ()
  "."
  (interactive)
  (sidebar-set buffers-line (line-number-at-pos))
  (sidebar-set buffers-hide nil)
  (sidebar-set buffers-return-to-files nil)
  (sidebar-close))

(defmacro sidebar-buffers-protect (name time &rest body)
  "NAME TIME BODY."
  (declare (indent 2))
  (let ((n (intern (format "sidebar-repet-%s" name))))
    `(progn
       (unless (sidebar-get ,n)
	     (sidebar-set ,n t)
	     (run-at-time ,time nil (lambda ()
				                  (sidebar-set ,n nil)
				                  (progn ,@body)))))))

(defun sidebar-buffers-list-update ()
  "."
  (interactive)
  (sidebar-buffers-protect buffers-list-update 2
    (-when-let* ((_ (sidebar-get-window t))
		         (sidebar-buffer (get-buffer (sidebar-cons-buffer-name))))
      (with-current-buffer sidebar-buffer
	    (when (equal major-mode 'sidebar-buffers-mode)
	      (sidebar-refresh nil t))))))

(defvar sidebar-buffers-mode-map nil
  "Keymap used with ‘sidebar-buffers-mode’.")
(unless sidebar-buffers-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "C-q") 'sidebar-kill)
    (define-key map (kbd "M-RET") 'sidebar-buffers-open-in-window)
    (define-key map (kbd "RET") 'sidebar-buffers-open-line)
    (define-key map (kbd "q") 'sidebar-buffers-close)
    (define-key map (kbd "g") 'sidebar-refresh)
    (define-key map (kbd "x") 'sidebar-buffers-mark-execute)
    (define-key map (kbd "u") 'sidebar-buffers-unmark)
    (define-key map (kbd "d") 'sidebar-buffers-mark-delete)
    (define-key map (kbd "s") 'sidebar-buffers-mark-save)
    (define-key map (kbd "<tab>") 'sidebar-buffers-switch-to-files)
    (define-key map (kbd "TAB") 'sidebar-buffers-switch-to-files)
    (define-key map (kbd "h") 'sidebar-buffers-toggle-hidden)
    (define-key map (kbd "<right>") 'sidebar-adjust-window-width)
    (define-key map (kbd "<left>") 'sidebar-reset-window-width)
    (define-key map (kbd "?") 'sidebar-help)
    (setq sidebar-buffers-mode-map map)))

(define-derived-mode sidebar-buffers-mode special-mode "Sidebar-buffers"
  "Major mode for Sidebar-buffers.

\\{sidebar-buffers-mode-map}"
  ::group sidebar-buffers

  (make-local-variable 'post-command-hook)

  (sidebar-init-mode)

  (add-hook 'pre-command-hook 'sidebar-buffers-pre-command nil)
  (add-hook 'post-command-hook 'sidebar-post-command t)
  (add-hook 'delete-frame-functions 'sidebar-delete-buffer-on-kill)
  (add-hook 'before-make-frame-hook 'sidebar-before-make-frame-hook)
  (add-hook 'buffer-list-update-hook 'sidebar-buffers-list-update)

  (remove-hook 'post-command-hook 'global-hl-line-highlight)


  (sidebar-set-window))

(provide 'sidebar-buffers)

;;; sidebar-buffers.el ends here
