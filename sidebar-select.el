;;; sidebar-select.el --- sidebar-select -*- lexical-binding: t; -*-

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
;; sidebar-select is the small window below the sidebar when we
;; have to select another window

;;; Code:

(require 's)
(require 'dash)
(require 'icons-in-terminal nil t)
(require 'sidebar-utils)

(defface sidebar-select-line
  '((((min-colors 16777216))
     :foreground "white"
     :box nil
     :background "#1A237E")
    (((min-colors 256))
     :foreground "white"
     :box nil
     :background "#005fff")
    (t
     :foreground "black"
     :box nil
     :background "white"))
  "Face used for the current line with `sidebar-select'."
  :group 'sidebar-faces)

(defface sidebar-select-header
  '((((min-colors 16777216))
     :foreground "white"
     :box nil
     :overline "#1A237E"
     :background "#1A237E")
    (((min-colors 256))
     :foreground "white"
     :box nil
     :overline "#005fff"
     :background "#005fff")
    (t
     :foreground "black"
     :box nil
     :background "white"))
  "Face used for the headers with `sidebar-select'."
  :group 'sidebar-faces)

(defcustom sidebar-select-icon-left-header 'myicons_0006
  "Icon to use on the left of the header.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-select-icon-right-header 'myicons_0008
  "Icon to use on the right of the header.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-select-icon-before-window 'oct_device_desktop
  "Icon to use before the window name.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-select-icon-before-directory 'oct_file_directory
  "Icon to use before the directory name.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defconst sidebar-select-buffer-name " SIDEBAR-SELECT")
(defvar sidebar-select-header "")
(defvar sidebar-select-args nil)
(defvar sidebar-select-mapping nil)
(defvar sidebar-select-window nil)

(defun sidebar-select-set-header (string raise height)
  "STRING RAISE HEIGHT."
  (let* ((face (face-background 'sidebar-select-header nil t))
         (width (window-total-width))
         (len (length string)))
    (concat
     (propertize " " 'face `(:overline ,face)
                 'display `(space :align-to (- right-margin ,(/ width 2) ,(/ len 2) 3)))
     (icons-in-terminal sidebar-select-icon-left-header :foreground face
	    	            :overline face :raise raise :height height)
     (propertize string 'display '(raise 0.25)
	             'face `(:inherit sidebar-select-header :height 1.0 :overline ,face))
     (icons-in-terminal sidebar-select-icon-right-header :foreground face
	    	            :overline face :raise raise :height height)
     (propertize " " 'face `(:overline ,face)
                 'display `(space :align-to (- right-margin 1))))))

(defun sidebar-select-insert-item (name icon item callback)
  "NAME ICON ITEM CALLBACK."
  (let ((string (concat (s-truncate (- (window-width) 2)
	                                (concat " "
		                                    (icons-in-terminal icon)
		                                    " "
		                                    name))
                        "\n")))
    (add-text-properties 0 (length string)
                         `(select-callback ,callback select-item ,item)
                         string)
    (insert string)))

(defun sidebar-select-insert-list (list func-on-string icon callback)
  "LIST FUNC-ON-STRING ICON CALLBACK."
  (--each list
    (let ((string (funcall func-on-string it)))
      (sidebar-select-insert-item string icon it callback))))

(defun sidebar-select-new-window (&rest _)
  "."
  (-when-let (buffer (car sidebar-select-args))
    (display-buffer-pop-up-window buffer nil)))

(defun sidebar-select-new-frame (&rest _)
  "."
  (-when-let (buffer (car sidebar-select-args))
    (display-buffer-pop-up-frame buffer nil)))

(defun sidebar-select-make-buffer (list header1 header2 func-on-string icon callback &rest args)
  "LIST HEADER1 HEADER2 FUNC-ON-STRING ICON CALLBACK ARGS."
  (select-window
   (display-buffer-in-side-window (get-buffer-create sidebar-select-buffer-name)
                                  '((side . left) (slot . 1))))
  (with-current-buffer sidebar-select-buffer-name
    (sidebar-select-mode)
    (setq-local scroll-margin 1)
    (setq-local sidebar-select-window (get-buffer-window))
    (setq-local sidebar-select-header header1)
    (setq-local sidebar-select-args args)
    (setq header-line-format (list '(:eval (sidebar-select-set-header sidebar-select-header 0.0 2.0))))
    (overlay-put (make-overlay (point) (point)) 'after-string "\n")
    (sidebar-select-insert-list (car list) func-on-string icon callback)
    (when (car (cdr list))
      (overlay-put (make-overlay (point) (point))
                   'after-string (concat "\n" (sidebar-select-set-header header2 0.12 1.7) "\n\n"))
      (sidebar-select-insert-list (cadr list) func-on-string icon callback))

    (overlay-put (make-overlay (point) (point)) 'after-string "\n")
    (sidebar-select-insert-item "Create window" icon t 'sidebar-select-new-window)
    (sidebar-select-insert-item "Create frame" icon t 'sidebar-select-new-frame)

    (set-window-margins nil 0 0)
    (fit-window-to-buffer nil 1000 7)
    (window-resize nil 1 nil)
    (setq buffer-read-only t)
    (goto-char 1)))

(defun sidebar-select-killed-hook ()
  "."
  (when (s-equals? sidebar-select-buffer-name (buffer-name))
    (sidebar-set select-active nil)))

(defun sidebar-select-select ()
  "."
  (interactive)
  (-when-let* ((callback (get-text-property (point) 'select-callback))
               (item (get-text-property (point) 'select-item)))
    (if (car sidebar-select-args)
        (apply callback item sidebar-select-args)
      (funcall callback item))
    (sidebar-select-cancel)))

(defun sidebar-select-cancel ()
  "."
  (interactive)
  (ignore-errors (kill-buffer sidebar-select-buffer-name))
  (advice-remove 'select-window 'sidebar-select-window-changed)
  (remove-hook 'kill-buffer-hook 'sidebar-select-killed-hook t)
  (remove-hook 'buffer-list-update-hook 'sidebar-select-on-change)
  (remove-hook 'pre-command-hook 'sidebar-select-pre-command t)
  (remove-hook 'focus-out-hook 'sidebar-select-focus-out t)
  (-some-> (sidebar-get-window t) select-window))

(defvar sidebar-select-mode-map nil
  "Keymap use with ‘sidebar-select-mode’.")
(unless sidebar-select-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
;;;    (define-key map [t] 'ignore)

    (define-key map (kbd "RET") 'sidebar-select-select)
    (define-key map (kbd "<return>") 'sidebar-select-select)
    (define-key map "<return>" 'sidebar-select-select)
    (define-key map (kbd "q") 'sidebar-select-cancel)
    (define-key map (kbd "C-g") 'sidebar-select-cancel)

    (setq sidebar-select-mode-map map)))

(defun sidebar-select-on-change ()
  "."
  (when (and (not (memq this-command '(sidebar-open-in-window)))
	         (not sidebar-select-window)
	         this-command)
    (sidebar-select-cancel)))

(defun sidebar-select-focus-out ()
  "."
  (sidebar-select-cancel))

(defun sidebar-select-pre-command ()
  "."
  (when (equal this-command 'handle-switch-frame)
    (sidebar-select-cancel)))

(defun sidebar-select-post-command ()
  "."
  (set-window-margins nil 0 0)
  (when (eobp)
    (ignore-errors (forward-line -1)))
  (set-window-start nil 1))

(define-derived-mode sidebar-select-mode special-mode "Sidebar"
  "Major mode for Sidebar select.
\\{sidebar-select-mode-map}"
  ::group sidebar-select
  (setq cursor-type nil
	    mode-line-format "Type 'q' or C-g to cancel"
	    cursor-type nil
	    buffer-read-only nil)
  (sidebar-set select-active t)
  (internal-show-cursor nil nil)
  (face-remap-add-relative 'hl-line 'sidebar-select-line)
  (setq truncate-partial-width-windows nil)
  (setq truncate-lines t)
  (add-hook 'kill-buffer-hook 'sidebar-select-killed-hook nil t)
  (add-hook 'buffer-list-update-hook 'sidebar-select-on-change)
  (add-hook 'pre-command-hook 'sidebar-select-pre-command nil t)
  (add-hook 'post-command-hook 'sidebar-select-post-command nil t)
  (add-hook 'focus-out-hook 'sidebar-select-focus-out nil t)
  (hl-line-mode))

(provide 'sidebar-select)

;;; sidebar-select.el ends here
