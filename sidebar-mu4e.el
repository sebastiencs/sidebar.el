;;; sidebar-mu4e.el --- sidebar-mu4e

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
;; Major mode of the sidebar for mu4e
;;

;;; Code:

(require 'dash)
(require 'sidebar-utils)
(require 'icons-in-terminal)
(require 's)

(declare-function mu4e-get-maildirs "ext:mu4e-utils.el")
(declare-function mu4e~headers-jump-to-maildir "ext:mu4e-headers.el")
(declare-function mu4e-context-name "ext:mu4e-context.el")
(declare-function mu4e-context-current "ext:mu4e-context.el")
(declare-function sidebar-find-file-from-line "ext:sidebar.el")
(declare-function --getpath "ext:sidebar.el")

(defvar mu4e-contexts)
(defvar sidebar-icon-header-end)
(defvar sidebar-header-line-height)
(defvar sidebar-width)
(defvar sidebar-icons-branches-modeline)
(defvar sidebar-mode-line-height)
(defvar sidebar-files-number)

(defun sidebar-mu4e-load-maildirs (&rest p)
  "P."
  (-map 'sidebar-file-struct (mu4e-get-maildirs)))

(defun sidebar--mu4e-open-maildir-1 (maildir)
  "MAILDIR."
  (mu4e~headers-jump-to-maildir maildir))

(defun sidebar-mu4e-open-maildir ()
  "."
  (interactive)
  (let* ((maildir (sidebar-find-file-from-line)))
    (sidebar--mu4e-open-maildir-1 (--getpath maildir))))

(defun sidebar-mu4e-insert-icon (&rest p)
  "P."
  (insert (icons-in-terminal 'oct_inbox :height 1.1))
  )

(defun sidebar-mu4e? ()
  "."
  (with-selected-window (--get-in-frame 'sidebar-window-origin)
    (derived-mode-p 'mu4e-compose-mode
		    'mu4e-main-mode
		    'mu4e-headers-mode)))

(defun sidebar-mu4e-set-header ()
  "."
  (let* ((context (or (and mu4e-contexts
			   (mu4e-context-name (mu4e-context-current)))
		      "mu4e"))
	 (length (- (window-width (sidebar-get-window)) (+ (length context) 4))))
    (concat
     (propertize " " 'face 'sidebar-header-line-face)
     (icons-in-terminal 'oct_mail
			:face 'sidebar-icon-header-project-face
			:background (face-background 'sidebar-header-line-face)
			:raise -0.07
			:height 1.3)
     (propertize
      (concat " "
	      context
	      (s-repeat length " "))
      'face 'sidebar-header-line-face
      'display '(raise 0.12))
     (icons-in-terminal (car sidebar-icon-header-end)
			:foreground (face-background 'sidebar-header-line-face)
			:height sidebar-header-line-height))))

(defun sidebar-mu4e-set-modeline ()
  "."
  (let* ((maildirs nil)
	 (space-to-add 0))
    (setq maildirs (concat
		    (icons-in-terminal (cadr sidebar-icons-branches-modeline)
				       :foreground (face-background 'sidebar-remotebranch-face)
				       :raise -0.1
				       :height sidebar-mode-line-height)
		    (propertize (concat " "
					(number-to-string sidebar-files-number)
					(if (> sidebar-files-number 1) " maildirs " " maildir "))
				'face 'sidebar-remotebranch-face)))
    (setq space-to-add (- (window-width (sidebar-get-window)) (length maildirs)))
    (when (sidebar-gui?)
      (setq space-to-add (- (+ (- space-to-add 3)
			       (* sidebar-mode-line-height 2))
			    (car (cddr sidebar-icons-branches-modeline)))))
    (concat (s-repeat space-to-add " ")
	    maildirs
	    (propertize "    " 'face 'sidebar-remotebranch-face))))

(defvar sidebar-mu4e-mode-map nil
  "Keymap uses with sidebar-mu4e-mode.")
(unless sidebar-mu4e-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q") 'sidebar-close)
    (define-key map (kbd "RET") 'sidebar-mu4e-open-maildir)
    (define-key map (kbd "<right>") 'sidebar-resize-window)
    (define-key map (kbd "?") 'sidebar-help)
    (setq sidebar-mu4e-mode-map map)))

(define-derived-mode sidebar-mu4e-mode nil "Sidebar-mu4e"
  "Major mode for Sidebar-mu4e.

\\{sidebar-mu4e-mode-map}"
  ::group sidebar

  (setq-local sidebar-width 30)

  (if (sidebar-gui?)
      (progn
	;; (setq sidebar-status-on-directory sidebar-gui-status-on-directory)
	;; (setq sidebar-filename-colored sidebar-gui-filename-colored)
	;; (setq sidebar-status-on-file sidebar-gui-status-on-file)
	(copy-face 'sidebar-powerline-gui-face 'sidebar-powerline-face)
	(copy-face 'sidebar-file-gui-face 'sidebar-file-face)
	(copy-face 'sidebar-dir-gui-face 'sidebar-dir-face)
	(copy-face 'sidebar-untracked-dir-gui-face 'sidebar-untracked-dir-face)
	(copy-face 'sidebar-untracked-file-gui-face 'sidebar-untracked-file-face)
	(copy-face 'sidebar-ignored-dir-gui-face 'sidebar-ignored-dir-face)
	(copy-face 'sidebar-ignored-file-gui-face 'sidebar-ignored-file-face)
	(copy-face 'sidebar-not-updated-gui-face 'sidebar-not-updated-face)
	(copy-face 'sidebar-updated-gui-face 'sidebar-updated-face)
	(copy-face 'sidebar-changed-gui-face 'sidebar-changed-face)
	(copy-face 'sidebar-added-gui-face 'sidebar-added-face)
	(copy-face 'sidebar-renamed-gui-face 'sidebar-renamed-face)
	(copy-face 'sidebar-header-line-gui-face 'sidebar-header-line-face)
	(copy-face 'sidebar-branch-gui-face 'sidebar-branch-face)
	(copy-face 'sidebar-remotebranch-gui-face 'sidebar-remotebranch-face)
	(copy-face 'sidebar-icon-branch-gui-face 'sidebar-icon-branch-face)
	(copy-face 'sidebar-icon-remotebranch-gui-face 'sidebar-icon-remotebranch-face)
	(copy-face 'sidebar-icon-header-project-gui-face 'sidebar-icon-header-project-face)
	(copy-face 'sidebar-icon-header-directory-gui-face 'sidebar-icon-header-directory-face)
	(copy-face 'sidebar-match-gui-face 'sidebar-match-face))
    ;; (setq sidebar-status-on-directory sidebar-terminal-status-on-directory)
    ;; (setq sidebar-filename-colored sidebar-terminal-filename-colored)
    ;; (setq sidebar-status-on-file sidebar-terminal-status-on-file)
    (copy-face 'sidebar-powerline-terminal-face 'sidebar-powerline-face)
    (copy-face 'sidebar-file-terminal-face 'sidebar-file-face)
    (copy-face 'sidebar-dir-terminal-face 'sidebar-dir-face)
    (copy-face 'sidebar-untracked-dir-terminal-face 'sidebar-untracked-dir-face)
    (copy-face 'sidebar-untracked-file-terminal-face 'sidebar-untracked-file-face)
    (copy-face 'sidebar-ignored-dir-terminal-face 'sidebar-ignored-dir-face)
    (copy-face 'sidebar-ignored-file-terminal-face 'sidebar-ignored-file-face)
    (copy-face 'sidebar-not-updated-terminal-face 'sidebar-not-updated-face)
    (copy-face 'sidebar-updated-terminal-face 'sidebar-updated-face)
    (copy-face 'sidebar-changed-terminal-face 'sidebar-changed-face)
    (copy-face 'sidebar-added-terminal-face 'sidebar-added-face)
    (copy-face 'sidebar-renamed-terminal-face 'sidebar-renamed-face)
    (copy-face 'sidebar-header-line-terminal-face 'sidebar-header-line-face)
    (copy-face 'sidebar-branch-terminal-face 'sidebar-branch-face)
    (copy-face 'sidebar-remotebranch-terminal-face 'sidebar-remotebranch-face)
    (copy-face 'sidebar-icon-branch-terminal-face 'sidebar-icon-branch-face)
    (copy-face 'sidebar-icon-remotebranch-terminal-face 'sidebar-icon-remotebranch-face)
    (copy-face 'sidebar-icon-header-project-terminal-face 'sidebar-icon-header-project-face)
    (copy-face 'sidebar-icon-header-directory-terminal-face 'sidebar-icon-header-directory-face)
    (copy-face 'sidebar-match-terminal-face 'sidebar-match-face))

  ;; (make-local-variable 'sidebar-pre-hook-line-number)
  ;; (make-local-variable 'sidebar-saved-line-number)
  ;; (make-local-variable 'sidebar-git-branches)
  ;; (make-local-variable 'sidebar-files)
  ;; (make-local-variable 'sidebar-current-path)
  ;; (make-local-variable 'sidebar-closed-directories)
  ;; (make-local-variable 'sidebar-root-project)
  ;; (make-local-variable 'sidebar-git-hashtable)
  ;; (make-local-variable 'sidebar-git-dir)
  ;; (make-local-variable 'sidebar-icon-inserted-on-line)
  ;; (make-local-variable 'sidebar-file-to-copy)
  (setq cursor-type nil)
  (add-to-list 'display-buffer-alist '(" SIDEBAR-SELECT" display-buffer-in-side-window (side . left) (slot . 1)))
  ;; (push '("SIDEBAR-CHOICE" display-buffer-in-side-window (side . left) (slot . -1))
  ;; 	display-buffer-alist)
  ;; (display-buffer (get-buffer-create "buff2"))

  (make-local-variable 'post-command-hook)
  (make-local-variable 'pre-command-hook)
  (add-hook 'post-command-hook 'sidebar-post-command)
  (add-hook 'pre-command-hook 'sidebar-pre-command)
  (add-hook 'after-save-hook 'sidebar-refresh-on-save t)
  (add-hook 'delete-frame-functions 'sidebar-delete-buffer-on-kill)
  (add-hook 'before-make-frame-hook 'sidebar-before-make-frame-hook)
  (add-hook 'window-configuration-change-hook 'sidebar-config-change-hook)
  (face-remap-add-relative 'header-line '((:inherit sidebar-header-face :background "")))
  (face-remap-add-relative 'mode-line '((:inherit sidebar-header-face :foreground "" :background "" :box nil)))
  (face-remap-add-relative 'mode-line-inactive '((:inherit sidebar-header-face :foreground "" :background "" :box nil)))
  (setq header-line-format nil
	buffer-read-only nil
	mode-line-format nil)
  (setq mode-line-format (list '(:eval (sidebar-mu4e-set-modeline))))
  (setq header-line-format (list '(:eval (sidebar-mu4e-set-header))))
  )

(provide 'sidebar-mu4e)

;;; sidebar-mu4e.el ends here
