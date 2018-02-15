;;; sidebar-mu4e.el --- sidebar-mu4e  -*- lexical-binding: t; -*-

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
;; Major mode of the sidebar for mu4e
;;

;;; Code:

(require 'dash)
(require 'sidebar-utils)
(require 'icons-in-terminal nil t)

(declare-function mu4e-get-maildirs "ext:mu4e-utils.el")
(declare-function mu4e~headers-jump-to-maildir "ext:mu4e-headers.el")
(declare-function mu4e-headers-search "ext:mu4e-headers.el")
(declare-function mu4e-context-name "ext:mu4e-context.el")
(declare-function mu4e-context-current "ext:mu4e-context.el")
(declare-function mu4e-bookmark-name "ext:mu4e-bookmark.el")
(declare-function mu4e-bookmark-query "ext:mu4e-bookmark.el")
(declare-function mu4e-bookmarks "ext:mu4e-bookmark.el")
(declare-function sidebar-find-file-from-line "ext:sidebar.el")
(declare-function sidebar-refresh "ext:sidebar.el")
(declare-function sidebar-init-mode "ext:sidebar.el")
(declare-function sidebar-open "ext:sidebar.el")
(declare-function sidebar-adjust-window-width "ext:sidebar.el")
(declare-function sidebar-reset-window-width "ext:sidebar.el")

(defvar mu4e-contexts)

(defgroup sidebar-mu4e nil
  "Sidebar mode to view its maildirs and bookmarks."
  :group 'tools
  :group 'convenience
  :group 'sidebar
  :link '(custom-manual "(sidebar-mu4e) Top")
  :link '(info-link "(sidebar-mu4e) Customizing"))

(defcustom sidebar-mu4e-autostart t
  "If non-nil, sidebar-mu4e is started with mu4e.
More precisely, it is started when `mu4e' is called."
  :type 'boolean
  :group 'sidebar-mu4e)

(defcustom sidebar-mu4e-width 30
  "Width of the sidebar with mu4e."
  :type 'integer
  :group 'sidebar-mu4e)

(defcustom sidebar-mu4e-maildir-icon 'fa_inbox
  "Icon to use with maildirs.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar-mu4e)

(defcustom sidebar-mu4e-bookmark-icon 'fa_bookmark
  "Icon to use with bookmarks.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar-mu4e)

(defun sidebar-mu4e-item-builder (item)
  "Return an association list from ITEM.
Function similar to `sidebar-file-struct' adapted for mu4e data."
  (list (cons 'data item)
	    (cons 'type (cond ((listp item) 'bookmark)
			              ((stringp item)'maildir)
			              (t 'separator)))
	    (cons 'line 0)))

(defun sidebar-mu4e-get-maildirs ()
  "Return a list of maildirs.
It can be advised to modify the list."
  (mu4e-get-maildirs))

(defun sidebar-mu4e-get-bookmarks (bookmarks)
  "Return a list of BOOKMARKS.
It can be advised to modify the list."
  (--map `(,(mu4e-bookmark-name it) ,(mu4e-bookmark-query it))
	     bookmarks))

(sidebar-content-provider mu4e (&rest _)
  "Return a list of maildirs and bookmarks to print in the sidebar.
The list will be mapped with `sidebar-mu4e-item-builder' to make them
easily usable."
  (let ((maildirs (sidebar-mu4e-get-maildirs))
	    (bookmarks (sidebar-mu4e-get-bookmarks (mu4e-bookmarks))))
    (sidebar-set mu4e-bookmarks-count (length bookmarks))
    (sidebar-set mu4e-maildirs-count (length maildirs))
    (-concat maildirs '(separator) bookmarks)))

(sidebar-print-function mu4e (item)
  "ITEM."
  (-let* (((&alist 'data data 'type type) item))
    (if (eq type 'separator)
        (ignore (overlay-put (make-overlay (point) (point)) 'after-string "\n"))
      (concat
       " "
       (pcase type
	     ('bookmark (icons-in-terminal sidebar-mu4e-bookmark-icon :height 1.1))
	     ('maildir  (icons-in-terminal sidebar-mu4e-maildir-icon :height 1.1)))
       " "
       (pcase type
	     ('bookmark (car data))
	     ('maildir  data))
       "\n"))))

(defun sidebar-mu4e-open-maildir (maildir)
  "Open MAILDIR.
This is a small subfunction of `sidebar-mu4e-open-line' to let the
user advise it and easily access the parameter MAILDIR."
  (mu4e~headers-jump-to-maildir maildir))

(defun sidebar-mu4e-open-bookmark (bookmark)
  "Open BOOKMARK.
This is a small subfunction of `sidebar-mu4e-open-line' to let the
user advise it and easily access the parameter BOOKMARK."
  (mu4e-headers-search bookmark))

(defun sidebar-mu4e-open-line ()
  "Open the maildir or bookmark on the current line."
  (interactive)
  (-let* (((&alist 'data data 'type type) (sidebar-find-file-from-line)))
    (select-window (sidebar-get window-origin))
    (pcase type
      ('maildir  (sidebar-mu4e-open-maildir data))
      ('bookmark (sidebar-mu4e-open-bookmark (cadr data))))))

(defun sidebar-mu4e? ()
  "Return non-nil if we have to use `sidebar-mu4e-mode' on the sidebar creation."
  (prog1
      (with-selected-window (sidebar-get window-origin)
	    (or (sidebar-get mu4e-force)
	        (derived-mode-p 'mu4e-compose-mode
			                'mu4e-main-mode
			                'mu4e-headers-mode)))
    (sidebar-set mu4e-force nil)))

(defun sidebar-mu4e-make-header ()
  "Return the string to insert in the sidebar header."
  (let* ((context (or (and mu4e-contexts
			               (mu4e-context-name (mu4e-context-current)))
		              "mu4e")))
    (concat
     " "
     (icons-in-terminal 'oct_mail
			            :face 'sidebar-icon-header-project
			            :background (face-background 'sidebar-header-line nil t)
			            :raise -0.07
			            :height 1.3)
     " "
     (propertize
      (concat (upcase (substring context 0 1)) (substring context 1))
      'display '(raise 0.12)))))

(defun sidebar-mu4e-make-modeline-left ()
  "Return the string to insert in the modeline (left side)."
  " mu4e")

(defun sidebar-mu4e-make-modeline-right ()
  "Return the string to insert in the modeline (right side)."
  (concat
   (number-to-string (sidebar-get mu4e-maildirs-count))
   " "
   (icons-in-terminal sidebar-mu4e-maildir-icon)
   " | "
   (number-to-string (sidebar-get mu4e-bookmarks-count))
   " "
   (icons-in-terminal sidebar-mu4e-bookmark-icon)
   "  "))

(defun sidebar-mu4e-context-switch (&rest _)
  "Function called when the context on mu4e has been changed.
Refresh the content of the sidebar (maildirs and bookmarks)."
  (when (sidebar-get-window t)
    (sidebar-refresh)))

(defun sidebar-mu4e-quit (&rest _)
  "Function called when mu4e quits.
It removes the sidebar."
  (interactive)
  (advice-remove 'mu4e-context-switch 'sidebar-mu4e-context-switch)
  (advice-remove 'mu4e-quit 'sidebar-mu4e-quit)
  (ignore-errors (kill-buffer (sidebar-cons-buffer-name))))

(defun sidebar-mu4e-autostart (&rest _)
  "If variable `sidebar-mu4e-autostart' is non-nil, sidebar-mu4e is open ..
automatically with mu4e."
  (when sidebar-mu4e-autostart
    (sidebar-set mu4e-force t)
    (run-with-timer 0.1 nil #'sidebar-open)))
;;    (sidebar-open)))

(defvar sidebar-mu4e-mode-map nil
  "Keymap used with ‘sidebar-mu4e-mode’.")
(unless sidebar-mu4e-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd ";") 'mu4e-context-switch)
    (define-key map (kbd "q") 'sidebar-mu4e-quit)
    (define-key map (kbd "RET") 'sidebar-mu4e-open-line)
    (define-key map (kbd "<right>") 'sidebar-adjust-window-width)
    (define-key map (kbd "<left>") 'sidebar-reset-window-width)
    (define-key map (kbd "?") 'sidebar-help)
    (setq sidebar-mu4e-mode-map map)))

(advice-add 'mu4e :after 'sidebar-mu4e-autostart)

(define-derived-mode sidebar-mu4e-mode special-mode "Sidebar-mu4e"
  "Major mode for Sidebar-mu4e.

\\{sidebar-mu4e-mode-map}"
  ::group sidebar-mu4e

  (make-local-variable 'post-command-hook)
  (make-local-variable 'pre-command-hook)

  (sidebar-set default-width sidebar-mu4e-width)

  (advice-add 'mu4e-context-switch :after 'sidebar-mu4e-context-switch)
  (advice-add 'mu4e-quit :before 'sidebar-mu4e-quit)

  (sidebar-init-mode)

  (add-hook 'post-command-hook 'sidebar-post-command t)
  (add-hook 'delete-frame-functions 'sidebar-delete-buffer-on-kill)
  (add-hook 'before-make-frame-hook 'sidebar-before-make-frame-hook)

  (sidebar-set-window))

(provide 'sidebar-mu4e)

;;; sidebar-mu4e.el ends here
