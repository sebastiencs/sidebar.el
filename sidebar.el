;;; sidebar.el --- Sidebar major mode

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
;; Todo: remove -face suffix https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Faces.html#Defining-Faces
;; my project
;;
;; Features that are required by this library:
;;
;;  `projectile'
;;  `loop'
;;  `s'
;;  `dash'
;;

;;; Code:

(require 'projectile)
(require 'loop)
(require 's)
(require 'dash)
(require 'icons-in-terminal)
(require 'sidebar-filemapping)

(eval-after-load 'dash '(dash-enable-font-lock))

(defgroup sidebar nil
  "Customizable file explorer with git integration."
  :group 'tools
  :group 'convenience
  :link '(custom-manual "(sidebar) Top")
  :link '(info-link "(sidebar) Customizing"))

(defgroup sidebar-terminal-faces nil
  "Faces uses in sidebar on terminals."
  :prefix "sidebar-"
  :link '(info-link "(sidebar) Frames and Faces")
  :group 'sidebar
  :group 'faces)

(defgroup sidebar-gui-faces nil
  "Faces uses in sidebar with gui."
  :prefix "sidebar-"
  :link '(info-link "(sidebar) Frames and Faces")
  :group 'sidebar
  :group 'faces)

(defmacro --getpath (file)
  "Return the path from FILE."
  `(alist-get 'path ,file))

(defmacro --dir? (file)
  "Return non-nil if FILE is a directory."
  `(alist-get 'dir ,file))

(defmacro --opened? (file)
  "Return non-nil if FILE is opened.  Open means extended (only directory can be open)."
  `(alist-get 'opened ,file))

(defmacro --getline (file)
  "Return the line where is FILE."
  `(alist-get 'line ,file))

(defconst sidebar-buffer-name "SIDEBAR"
  "Name of a Sidebar buffer, followed by the frame name.")

(defcustom sidebar-resize-auto-window nil
  "If activated, the sidebar's window will automatically be resize if the..
filename on the current line is longer than the window.
This can be done manually by calling the function `\\[sidebar-resize-window]' or
by binding a key to it."
  :type 'boolean
  :group 'sidebar)

(defcustom sidebar-message-current nil
  "If activated, print the full path of the current file in the echo area."
  :type 'boolean
  :group 'sidebar)

(defcustom sidebar-width 40
  "Default window width of `sidebar'."
  :type 'integer
  :group 'sidebar)

(defcustom sidebar-character-dir-closed "📁"
  "Character to insert before a closed directory.
Examples: '+' '▸' '🞊' '🞄' '⚬' '•' '⚫' '⚛' '◦' '●' '▫' '▢' '+' '▸'"
  :type 'character
  :group 'sidebar)

(defcustom sidebar-character-dir-opened "📂"
  "Character to insert before an opened directory.
Examples: '↳' '-' '▾'"
  :type 'character
  :group 'sidebar)

(defcustom sidebar-character-git-not-updated "✗"
  "Character to use with a not-updated file (with git)."
  :type 'character
  :group 'sidebar)

(defcustom sidebar-character-git-updated "✓"
  "Character to use with an updated file (with git)."
  :type 'character
  :group 'sidebar)

(defcustom sidebar-character-git-changed "✓"
  "Character to use with file that has changed since index (with git)."
  :type 'character
  :group 'sidebar)

(defcustom sidebar-character-git-added "+"
  "Character to use with an added file (with git)."
  :type 'character
  :group 'sidebar)

(defcustom sidebar-character-git-renamed "✂"
  "Character to use with a renamed file (with git)."
  :type 'character
  :group 'sidebar)

(defcustom sidebar-character-git-match "✓"
  "Character to use with a file that matches the index (with git)."
  :type 'character
  :group 'sidebar)

;;;"Insert the remaining spaces and a '' to make a powerline effect."

(defcustom sidebar-character-powerline ""
  "Character to insert at the end of the current line.

Default: \"\"."
  :type 'character
  :group 'sidebar)

(defcustom sidebar-terminal-status-on-file t
  "TERMINAL.  Insert icon on the filename according to its git status.

Default: t."
  :type 'boolean
  :group 'sidebar)

(defcustom sidebar-terminal-status-on-directory 'on-closed
  "TERMINAL.  Control when to place icon of git status on directories.

The icons represent the git status of all the subfiles of the directory.
Each icon is followed by a number: The number of times the status is present.

The following values are possible:

- `never' Never insert icon after directories;

- `on-closed' Insert icons on closed directories only.

- `on-opened' Insert icons on opened directories only.

' `always' Always insert icon.

Default: `on-closed'."
  :type '(choice (const :tag "Never" never)
                 (const :tag "On closed directories only" on-closed)
                 (const :tag "On opened directories only" on-opened)
                 (const :tag "Always" 'always))
  :group 'sidebar)

(defcustom sidebar-terminal-filename-colored nil
  "TERMINAL.  The filename will be colored according to its git status.
Untracked and ignored files will always be colored.

Default: nil."
  :type 'boolean
  :group 'sidebar)

(defcustom sidebar-gui-status-on-file t
  "GUI.  Insert icon on the filename according to its git status.

Default: t."
  :type 'boolean
  :group 'sidebar)

(defcustom sidebar-gui-status-on-directory 'on-closed
  "GUI.  Control when to place icon of git status on directories.

The icons represent the git status of all the subfiles of the directory.
Each icon is followed by a number: The number of times the status is present.

The following values are possible:

- `never' Never insert icon after directories;

- `on-closed' Insert icons on closed directories only.

- `on-opened' Insert icons on opened directories only.

' `always' Always insert icon.

Default: `on-closed'."
  :type '(choice (const :tag "Never" never)
                 (const :tag "On closed directories only" on-closed)
                 (const :tag "On opened directories only" on-opened)
                 (const :tag "Always" 'always))
  :group 'sidebar)

(defcustom sidebar-gui-filename-colored nil
  "GUI.  The filename will be colored according to its git status.
Untracked and ignored files will always be colored.

Default: nil."
  :type 'boolean
  :group 'sidebar)

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defface sidebar-file-terminal-face
  '((t :foreground "grey"))
  "Face used with files."
  :group 'sidebar-terminal-faces)

(defface sidebar-dir-terminal-face
  '((t :foreground "#005fff"))
  "Face used with directories."
  :group 'sidebar-terminal-faces)

(defface sidebar-untracked-dir-terminal-face
  '((t :foreground "#FF8C00"))
  "Face used with untracked directories."
  :group 'sidebar-terminal-faces)

(defface sidebar-untracked-file-terminal-face
  '((t :foreground "#FF8C00"))
  "Face used with untracked files."
  :group 'sidebar-terminal-faces)

(defface sidebar-ignored-dir-terminal-face
  '((t :foreground "#3f3f3f"))
  "Face used with ignored (on git) directories."
  :group 'sidebar-terminal-faces)

(defface sidebar-ignored-file-terminal-face
  '((t :foreground "#3f3f3f"))
  "Face used with ignored files."
  :group 'sidebar-terminal-faces)

(defface sidebar-not-updated-terminal-face
  '((t :foreground "red"))
  "Face used with icon for files not updated."
  :group 'sidebar-terminal-faces)

(defface sidebar-updated-terminal-face
  '((t :foreground "green"))
  "Face used with icon for updated files."
  :group 'sidebar-terminal-faces)

(defface sidebar-changed-terminal-face
  '((t :foreground "orange"))
  "Face used with icon for changed files."
  :group 'sidebar-terminal-faces)

(defface sidebar-added-terminal-face
  '((t :foreground "green"))
  "Face used with icon for added files."
  :group 'sidebar-terminal-faces)

(defface sidebar-renamed-terminal-face
  '((t :foreground "orange"))
  "Face used with icon for renamed files."
  :group 'sidebar-terminal-faces)

(defface sidebar-match-terminal-face
  '((t :foreground "green"))
  "Face used with icon for matched files."
  :group 'sidebar-terminal-faces)

(defface sidebar-powerline-terminal-face
  '((t :background "#005fff"
       :foreground "black"))
  "Face used for the powerline."
  :group 'sidebar-terminal-faces)

(defface sidebar-powerline-gui-face
  '((t :background "#005fff"
       :foreground "black"))
  "Face used for the powerline."
  :group 'sidebar-gui-faces)

(defface sidebar-file-gui-face
  '((t :foreground "grey"))
  "Face used with files."
  :group 'sidebar-gui-faces)

(defface sidebar-dir-gui-face
  '((t :foreground "#005fff"))
  "Face used with directories."
  :group 'sidebar-gui-faces)

(defface sidebar-untracked-dir-gui-face
  '((t :foreground "purple"))
  "Face used with untracked directories."
  :group 'sidebar-gui-faces)

(defface sidebar-untracked-file-gui-face
  '((t :foreground "purple"))
  "Face used with untracked files."
  :group 'sidebar-gui-faces)

(defface sidebar-ignored-dir-gui-face
  '((t :foreground "#3f3f3f"))
  "Face used with ignored (on git) directories."
  :group 'sidebar-gui-faces)

(defface sidebar-ignored-file-gui-face
  '((t :foreground "#3f3f3f"))
  "Face used with ignored files."
  :group 'sidebar-gui-faces)

(defface sidebar-not-updated-gui-face
  '((t :foreground "brown"))
  "Face used for files not updated."
  :group 'sidebar-gui-faces)

(defface sidebar-updated-gui-face
  '((t :foreground "forest green"))
  "Face used for updated files."
  :group 'sidebar-gui-faces)

(defface sidebar-changed-gui-face
  '((t :foreground "orange"))
  "Face used for changed files."
  :group 'sidebar-gui-faces)

(defface sidebar-added-gui-face
  '((t :foreground "green"))
  "Face used for added files."
  :group 'sidebar-gui-faces)

(defface sidebar-renamed-gui-face
  '((t :foreground "orange"))
  "Face used for renamed files."
  :group 'sidebar-gui-faces)

(defface sidebar-match-gui-face
  '((t :foreground "forest green"))
  "Face used for matched files."
  :group 'sidebar-gui-faces)


(defvar sidebar-status-on-directory nil)
(defvar sidebar-filename-colored nil)
(defvar sidebar-status-on-file nil)

(defface sidebar-powerline-face nil "" :group nil)
(defface sidebar-file-face nil "" :group nil)
(defface sidebar-dir-face nil "" :group nil)
(defface sidebar-untracked-dir-face nil "" :group nil)
(defface sidebar-untracked-file-face nil "" :group nil)
(defface sidebar-ignored-dir-face nil "" :group nil)
(defface sidebar-ignored-file-face nil "" :group nil)
(defface sidebar-not-updated-face nil "" :group nil)
(defface sidebar-updated-face nil "" :group nil)
(defface sidebar-changed-face nil "" :group nil)
(defface sidebar-added-face nil "" :group nil)
(defface sidebar-renamed-face nil "" :group nil)
(defface sidebar-match-face nil "" :group nil)

(defvar sidebar-files nil
  "List where are stored all files printed in the sidebar.
The format of each element is the one created by `\\[sidebar-file-struct]'.
It's buffer local.
The list is sorted the way it's printed on the sidebar.")

(defvar sidebar-current-path nil
  "Current directory.
Buffer local.")

(defvar sidebar-closed-directories nil
  "List of dirs and their file previously opened.
See `\\[sidebar-expand-dir]' and `\\[sidebar-close-dir]' for more info
Buffer local.")

(defvar sidebar-root-project nil
  "Root of the current project.
Buffer local.")

(defvar sidebar-git-hashtable nil
  "Hashtable of each file with a git status.
See `\\[sidebar-git-parse-buffer] for more info'
Buffer local.")

(defvar sidebar-git-dir nil
  "Path where the last time git has been running.
Buffer local.")

(defvar sidebar-header-text ""
  "Text to insert in the header line.
Buffer local.")

(defvar sidebar-window-origin nil
  "Window where sidebar has been called.
This is used to know where to open the file selected.
It's a frame parameter (Or Frame local).")

(defun sidebar-get-root-project ()
  "Return the project directory, nil if there is no project."
  (ignore-errors (projectile-project-root)))

(defun sidebar-project-root ()
  "Return the project root using projectile.
If it's not a project, return the file's directory.
If it's not a file, return the home directory."
  (interactive)
  (or (sidebar-get-root-project)
      (when buffer-file-name (file-name-directory buffer-file-name))
      "~"))

(defun sidebar-cons-buffer-name ()
  "Construct the buffer name from 'sidebar-buffer-name' and the frame name.
The return value should be unique for each frame.
On terminals instance, we use the frame parameter `\\[name]'
On Graphics ones, the name isn't unique for each frame, so we use
`\\[window-id]' that isn't available on terminals instance."
  (concat "*" sidebar-buffer-name "-" (or (frame-parameter nil 'window-id)
					  (frame-parameter nil 'name))"*"))
;;;(concat "*" sidebar-buffer-name "-" (frame-parameter nil 'name) "*"))

(defun sidebar-get-buffer ()
  "Return the existing/created sidebar buffer for the current frame."
  (get-buffer-create (sidebar-cons-buffer-name)))

(defun sidebar-cons-git-buffer-name ()
  "Construct the git buffer name from 'sidebar-buffer-name' and the frame name.
See `\\[sidebar-cons-buffer-name]' for more info."
  (concat "*" sidebar-buffer-name "-" (or (frame-parameter nil 'window-id)
					  (frame-parameter nil 'name))"-GIT*"))
;;;(concat "*" sidebar-buffer-name "-" (frame-parameter nil 'name) "-GIT*"))

(defun sidebar-get-git-buffer ()
  "Return the existing/created sidebar git buffer for the current frame."
  (get-buffer-create (sidebar-cons-git-buffer-name)))

(defun sidebar-exists? ()
  "Check if a sidebar for the frame exists."
  (get-buffer (sidebar-cons-buffer-name)))

(defun sidebar-gui? ()
  "Return non-nil if we're on a graphic instance."
  (display-graphic-p))

(defun sidebar-file-struct (file)
  "Return an association list from FILE.
In this project (sidebar.el) every parameters named 'file' are
object using this structure.
- 'path is the FILE's path
- 'dir is non-nil if FILE is a directory
- 'line is the number line where FILE is printed on the sidebar
- 'opened is non-nil it it's a directory and is expanded.

Example: (('path . \"/tmp/dir1/dir2\") ('dir . t) ('line . 8) ('opened . nil))"
  (list (cons 'path file)
	(cons 'dir (file-directory-p file))
	(cons 'line 0)
	(cons 'opened nil)))

(defun sidebar-dir-arrow (dirname opened)
  "Return the character to insert before the directory DIRNAME.
OPENED is non-nil if the directory is opened."
  (if opened
      (concat sidebar-character-dir-opened " " dirname)
    (concat sidebar-character-dir-closed " " dirname)))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-calc-depth (file status)
  "Calcul the depth of FILE from the current directory point of view.
This is uses to count the number of space to insert before the filename.
STATUS is the status of the FILE."
  (let* ((path-from-current (s-chop-prefix sidebar-current-path (--getpath file)))
	 (depth (s-count-matches "/" path-from-current))) ; TODO: Support Windows (replace '/')
    (when (> depth 0)
      (setq depth (* depth 2))
      (unless (--dir? file)
	(setq depth (+ depth 1))))
    (setq depth (+ depth 1))
    (when (not (sidebar-gui?))
      (when (or (and status
		     (not (equal 'ignored status))
		     (not (equal 'untracked status))
		     (not (--dir? file)))
		(not sidebar-status-on-file))
	(setq depth (- depth 2)))
      (when (< depth 1)
	(setq depth 1)))
    depth))

;; (defun sidebar-calc-depth (file status)
;;   "Calcul the depth of FILE from the current directory point of view.
;; This is uses to count the number of space to insert before the filename.
;; STATUS is the status of the FILE."
;;   (let* ((path-from-current (s-chop-prefix sidebar-current-path (--getpath file)))
;; 	 (depth (s-count-matches "/" path-from-current))) ; TODO: Support Windows (replace '/')
;;     (when (> depth 0)
;;       (setq depth (* depth 2))
;;       (unless (--dir? file)
;; 	(setq depth (+ depth 1))))
;;     (setq depth (+ depth 1))
;;     (when (and (not (sidebar-gui?))
;; 	       status
;; 	       (not (equal 'ignored status))
;; 	       (not (equal 'untracked status))
;; 	       (not (--dir? file))
;; 	       (not sidebar-status-on-directory))
;;       (setq depth (- depth 2)))
;;     depth))

(defun sidebar-get-filename-dir-indicator (file)
  "If FILE is a directory, return the filename with its icon, otherwise it just return the filename."
  (or (and (--dir? file)
	   (not (sidebar-gui?))
	   (sidebar-dir-arrow (file-name-nondirectory (--getpath file)) (--opened? file)))
      (file-name-nondirectory (--getpath file))))

(defun sidebar-child-of-status? (file-path status)
  "Return non-nil if FILE-PATH is a child of a STATUS directory."
  (when sidebar-git-hashtable
    (catch 'stop-map
      (maphash (lambda (key value)
		 (when (equal value status)
		   (when (and (s-starts-with? key file-path) (not (string= key file-path)))
		     (throw 'stop-map t))))
	       sidebar-git-hashtable))))

(defun sidebar-get-icon-from-status (status)
  "Return the icon character to insert from STATUS."
  (if (or (equal 'ignored status) (equal 'untracked status))
      ""
    (concat
     (cond ((equal 'not-updated status) sidebar-character-git-not-updated)
	   ((equal 'updated status) sidebar-character-git-updated)
	   ((equal 'changed status) sidebar-character-git-changed)
	   ((equal 'added status) sidebar-character-git-added)
	   ((equal 'renamed status) sidebar-character-git-renamed)
	   ((equal 'match status) sidebar-character-git-match)
	   (t ""))
     " ")))

;; (defun sidebar-get-icon-color (status current-line)
;;   "Return the icon face from STATUS, if CURRENT-LINE is non-nil, add the powerline background."
;;   (if current-line
;;       (cond ((equal 'not-updated status) `(sidebar-not-updated-face :background ,(face-background 'sidebar-powerline-face)))
;; 	    ((equal 'updated status) `(sidebar-updated-face :background ,(face-background 'sidebar-powerline-face)))
;; 	    ((equal 'changed status) `(sidebar-changed-face :background ,(face-background 'sidebar-powerline-face)))
;; 	    ((equal 'added status) `(sidebar-added-face :background ,(face-background 'sidebar-powerline-face)))
;; 	    ((equal 'renamed status) `(sidebar-renamed-face :background ,(face-background 'sidebar-powerline-face)))
;; 	    ((equal 'match status) `(sidebar-match-face :background ,(face-background 'sidebar-powerline-face)))
;; 	    (t `(:background ,(face-background 'sidebar-powerline-face))))
;;     (cond ((equal 'not-updated status) 'sidebar-not-updated-face)
;; 	  ((equal 'updated status) 'sidebar-updated-face)
;; 	  ((equal 'changed status) 'sidebar-changed-face)
;; 	  ((equal 'added status) 'sidebar-added-face)
;; 	  ((equal 'renamed status) 'sidebar-renamed-face)
;; 	  ((equal 'match status) 'sidebar-match-face)
;; 	  (t nil))))

;; (defun sidebar-get-filename-color (file path status current-line)
;;   "Return the face to use for FILE.
;; PATH is the path of the file relative to the project root directory
;; STATUS is the status from git
;; if CURRENT-LINE is non-nil, the function returns `\\[side-powerline-terminal-face]'."
;;   (if current-line
;;       'sidebar-powerline-terminal-face
;;     (cond ((and (equal 'ignored status)
;; 		(cond ((--dir? file) 'sidebar-ignored-dir-terminal-face)
;; 		      (t 'sidebar-ignored-file-terminal-face))))
;; 	  ((and (sidebar-child-of-status? path 'ignored)
;; 		(cond ((--dir? file) 'sidebar-ignored-dir-terminal-face)
;; 		      (t 'sidebar-ignored-file-terminal-face))))
;; 	  ((and (equal 'untracked status)
;; 		(cond ((--dir? file) 'sidebar-untracked-dir-terminal-face)
;; 		      (t 'sidebar-untracked-file-terminal-face))))
;; 	  ((and (sidebar-child-of-status? path 'untracked)
;; 		(cond ((--dir? file) 'sidebar-untracked-dir-terminal-face)
;; 		      (t 'sidebar-untracked-file-terminal-face))))
;; 	  ((--dir? file) 'sidebar-dir-terminal-face)
;; 	  (t 'sidebar-file-terminal-face))))

(defun sidebar-color-from-status (status &optional default current-line)
  "STATUS DEFAULT CURRENT-LINE."
  (let ((face (cond
	       ((and (equal 'not-updated status) 'sidebar-not-updated-gui-face))
	       ((and (equal 'updated status) 'sidebar-updated-gui-face))
	       ((and (equal 'changed status) 'sidebar-changed-gui-face))
	       ((and (equal 'added status) 'sidebar-added-gui-face))
	       ((and (equal 'renamed status) 'sidebar-renamed-gui-face))
	       ((and (equal 'match status) 'sidebar-match-gui-face))
	       (t default))))
    (if current-line
	`(,face :background ,(face-background 'sidebar-powerline-face))
      face)))

(defun sidebar-get-color (file path status current-line &optional icon no-color)
  "Return the face to use for FILE on a graphic instance.
PATH is the path of the file relative to the project root directory
STATUS is the status from git
if CURRENT-LINE is non-nil, the function returns `\\[side-powerline-face]'.
ICON
NO-COLOR."
  (cond (current-line 'sidebar-powerline-face)
	((and (equal 'ignored status)
	      (if (--dir? file) 'sidebar-ignored-dir-face 'sidebar-ignored-file-face)))
	((and (sidebar-child-of-status? path 'ignored)
	      (if (--dir? file) 'sidebar-ignored-dir-face 'sidebar-ignored-file-face)))
	((and (equal 'untracked status) (not icon)
	      (if (--dir? file) 'sidebar-untracked-dir-face 'sidebar-untracked-file-face)))
	((and (sidebar-child-of-status? path 'untracked)
	      (if (--dir? file) 'sidebar-untracked-dir-face 'sidebar-untracked-file-face)))
	((--dir? file) 'sidebar-dir-face)
	((and no-color 'sidebar-file-face))
	((and (not icon) (sidebar-color-from-status status 'sidebar-file-face)))))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-term-insert-status-subfiles (status number current-line &optional file path)
  "Insert the icon and numbers after a directory.
STATUS is use to know which icon to insert
NUMBER is the number to insert
CURRENT-LINE is non-nil if we have to insert the powerline background.
FILE unused.
PATH unused."
  (insert (if current-line (propertize " " 'font-lock-face 'sidebar-powerline-face) " "))
  (insert (propertize (sidebar-get-icon-from-status status) 'font-lock-face (sidebar-color-from-status status nil current-line)))
  (insert (if current-line (propertize (number-to-string number) 'font-lock-face 'sidebar-powerline-face) (number-to-string number))))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-gui-insert-status-subfiles (status number current-line file path)
  "STATUS NUMBER CURRENT-LINE FILE PATH."
  (sidebar-gui-insert-status file path status current-line t)
  (sidebar-insert " " (and current-line 'sidebar-powerline-face))
  (sidebar-insert (number-to-string number) (and current-line 'sidebar-powerline-face)))

(defun sidebar-status-subfiles (path)
  "PATH."
  (let ((not-updated 0) (updated 0) (untracked 0) (changed 0) (added 0) (renamed 0) (match 0))
    (when sidebar-git-hashtable
      (maphash (lambda (key value)
		 (when (s-starts-with? path key)
		   (cond ((equal 'not-updated value) (setq not-updated (+ not-updated 1)))
			 ((equal 'updated value) (setq updated (+ updated 1)))
			 ((equal 'changed value) (setq changed (+ changed 1)))
			 ((equal 'added value) (setq added (+ added 1)))
			 ((equal 'match value) (setq match (+ match 1)))
			 (t nil))))
	       sidebar-git-hashtable))
    `((not-updated . ,not-updated) (updated . ,updated) (untracked . ,untracked)
      (changed . ,changed) (added . ,added) (renamed . ,renamed) (match . ,match))))

(defun sidebar-insert-status-subfiles? (file)
  "FILE."
  (when (--dir? file)
    (cond ((equal sidebar-status-on-directory 'always) t)
	  ((and (equal sidebar-status-on-directory 'on-closed) (not (--opened? file))) t)
	  ((and (equal sidebar-status-on-directory 'on-opened) (--opened? file)) t))))

(defun sidebar-insert-status-subfiles (file path current-line)
  "FILE PATH CURRENT-LINE."
  (when (sidebar-insert-status-subfiles? file)
    (let* ((status-alist (sidebar-status-subfiles path))
	   (not-updated (alist-get 'not-updated status-alist))
	   (updated (alist-get 'updated status-alist))
	   (changed (alist-get 'changed status-alist))
	   (added (alist-get 'added status-alist))
	   (match (alist-get 'match status-alist))
	   (func (if (sidebar-gui?) 'sidebar-gui-insert-status-subfiles
		   'sidebar-term-insert-status-subfiles)))
      (when (> not-updated 0)
	(funcall func 'not-updated not-updated current-line file path))
      (when (> updated 0)
	(funcall func 'updated updated current-line file path))
      (when (> changed 0)
	(funcall func 'changed changed current-line file path))
      (when (> added 0)
	(funcall func 'added added current-line file path))
      (when (> match 0)
	(funcall func 'match match current-line file path)))))

(defun sidebar-insert (str face)
  "Small function to insert STR with FACE if non-nil."
  (if face
      (insert (propertize str 'font-lock-face face))
    (insert str)))

(defun sidebar-insert-icon (icon face)
  "Insert ICON with FACE if non-nil."
  (insert (icons-in-terminal icon :face face :height 1.2)))

(defun sidebar-insert-fileicon (filename face)
  "FILENAME FACE."
  (let* ((icon-and-color (sidebar-filemapping-lookup filename))
	 (icon (plist-get icon-and-color :icon))
	 (color (plist-get icon-and-color :color)))
    (if face
	(insert (icons-in-terminal icon :face face :height 1.1))
      (insert (icons-in-terminal icon :foreground color :height 1.1)))))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-insert-powerline ()
  "Insert the remaining spaces and a '' to make a powerline effect."
  (insert (propertize (s-repeat (- (window-width (sidebar-get-window)) (+ (current-column) 3)) " ")
		      'font-lock-face 'sidebar-powerline-face))
  (insert (icons-in-terminal 'powerline_left_hard_divider :foreground (face-background 'sidebar-powerline-face))))

(defun sidebar-gui-insert-icon-filename (file filename status path current-line)
  "FILE FILENAME STATUS PATH CURRENT-LINE."
  (if (--dir? file)
      (sidebar-insert-icon (if (--opened? file) 'fa_folder_open_o 'fa_folder_o)
			   (sidebar-get-color file path status current-line))
    (sidebar-insert-fileicon filename
			     (sidebar-get-color file path status current-line t)))
  (sidebar-insert " " (and current-line 'sidebar-powerline-face))
  (sidebar-insert filename (sidebar-get-color file path status current-line nil (not sidebar-filename-colored))))

(defun sidebar-gui-insert-status (file path status current-line &optional dir)
  "FILE PATH STATUS CURRENT-LINE DIR."
  (when (or sidebar-status-on-file dir)
    (let ((func (lambda (name face)
		  (sidebar-insert " " (and current-line 'sidebar-powerline-face))
		  (funcall 'sidebar-insert-icon name face)))
	  (face (sidebar-color-from-status status nil current-line)))
      (cond ((equal 'not-updated status) (funcall func 'oct_flame face))
	    ((equal 'updated status) (funcall func 'oct_git_commit face))
	    ((equal 'changed status) (funcall func 'oct_beaker face))
	    ((equal 'added status) (funcall func 'oct_pulse face))
	    ((equal 'renamed status) (funcall func 'oct_git_renamed face))
	    ((equal 'match status) (funcall func 'oct_git_commit face))))))

(defun sidebar-print-file (file &optional current-line)
  "Insert FILE on the current line.
The function inserts the filename without parents directories.

First, it inserts ' ' x times, depending on the file depth (relative to
 the current directory).
If the file has a git status and is not a directory, it inserts the icon
 associated to the status.
Then it inserts the filename.
If FILE is a directory and closed (not expanded), it inserts the icons of
 all the files it contains just after its name, still on the same line.
Finally, if CURRENT-LINE is non-nil, it insert the powerline.

FILE is a associated list created from `\\[sidebar-file-struct]'."
  (let* ((filename (sidebar-get-filename-dir-indicator file))
	 (path-in-project (s-chop-prefix sidebar-root-project (--getpath file)))
	 (path-fixed-dirname (if (--dir? file) (file-name-as-directory path-in-project) path-in-project))
	 (status (and sidebar-git-hashtable (gethash path-fixed-dirname sidebar-git-hashtable)))
	 (depth (sidebar-calc-depth file status)))
    (sidebar-insert (s-repeat depth " ") (and current-line 'sidebar-powerline-face))
    (when (sidebar-gui?)
      (sidebar-gui-insert-icon-filename file filename status path-fixed-dirname current-line)
      (sidebar-gui-insert-status file path-fixed-dirname status current-line))
    (unless (sidebar-gui?)
      (when (and status (not (--dir? file)) sidebar-status-on-file)
	(sidebar-insert (sidebar-get-icon-from-status status) (sidebar-color-from-status status nil current-line)))
      (sidebar-insert filename (sidebar-get-color file path-fixed-dirname status current-line nil (not sidebar-filename-colored))))
;;;      (sidebar-insert filename (sidebar-get-filename-color file path-fixed-dirname status current-line)))
    (sidebar-insert-status-subfiles file path-fixed-dirname current-line)
    (when current-line
      (sidebar-insert-powerline))))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-print-plain (file)
  "Unused.
Insert FILE without any decoration."
  (let ((filename (sidebar-get-filename-dir-indicator file)))
    (insert (s-repeat (sidebar-calc-depth file nil) " ") filename)))

(defun sidebar-print-listfiles (list)
  "Insert all files in LIST from the current line.
It updates the associated value 'line for each file.  This is used to
keep track of which file is on which line."
  (let ((func-insert 'sidebar-print-file))
    ;; (let ((func-insert (or (and sidebar-git-hashtable 'sidebar-print-file)
    ;; 			 'sidebar-print-plain)))
    (loop-for-each file list
      (setf (--getline file) (line-number-at-pos))
      (funcall func-insert file)
      (newline))))

(defun sidebar-sort-files-by-line ()
  "Sort `sidebar-files' by the associated value 'line."
  (setq sidebar-files (-sort (lambda (first second)
			       (< (--getline first) (--getline second)))
			     sidebar-files)))

(defun sidebar-print ()
  "Prints Sidebar."
  (setq sidebar-header-text (abbreviate-file-name sidebar-current-path))
  (sidebar-print-listfiles sidebar-files)
  (sidebar-sort-files-by-line))

(defun sidebar-dots-file (file)
  "Return t if FILE is '.' or '..'."
  (let ((file (file-name-nondirectory file)))
    (or (string= "." file) (string= ".." file))))

(defvar it)      ; Declared to avoid warning for this variable from dash.el
(defvar other)   ; Declared to avoid warning for this variable from dash.el

;;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-load-dir (path)
  "Return a list of files/directories in PATH.
It removes '.' and '..'
Sort the directories first
The list returned is a list of association list for each file created
with `\\[sidebar-file-struct]'"
  (let* ((files-and-dirs (-remove 'sidebar-dots-file (directory-files path t)))
	 (dirs-sorted (--sort (string< it other) (-filter 'file-directory-p files-and-dirs)))
	 (files-sorted (--sort (string< it other) (--filter (not (file-directory-p it)) files-and-dirs))))
    (-map 'sidebar-file-struct (-concat dirs-sorted files-sorted))))

(defun sidebar-get-window ()
  "Return the created/existing window displaying the sidebar buffer."
  (let ((sidebar-window (get-buffer-window (sidebar-cons-buffer-name))))
    (unless sidebar-window
      (setq sidebar-window (split-window (selected-window) (- (frame-width) sidebar-width) 'left nil)))
    sidebar-window))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-goto-buffername (buffer-name)
  "This function jump the cursor to BUFFER-NAME (string) in the sidebar.
If there is no filename equal to the BUFFER-NAME, it put the cursor
on the first line.
This is use when the sidebar is created."
  (let ((file (--first (string= (--getpath it) buffer-name) sidebar-files)))
    (if file
	(progn
	  (sidebar-goto-line (--getline file))
	  (sidebar-show-current))
      (sidebar-goto-line 1)
      (sidebar-show-current))))

(defun sidebar-open ()
  "Open or create a sidebar for the current frame."
  (interactive)
  (set-frame-parameter nil 'sidebar-window-origin (get-buffer-window))
  (setq sidebar-root-project (sidebar-get-root-project))
  (let ((sidebar-exists (sidebar-exists?))
	(sidebar-buffer (sidebar-get-buffer))
	(sidebar-window (sidebar-get-window))
	(project-path-root (sidebar-project-root))
	(buffer-name-current (buffer-file-name)))
    (set-window-buffer sidebar-window sidebar-buffer)
    (set-buffer sidebar-buffer)
    (select-window sidebar-window)
    (internal-show-cursor (sidebar-get-window) nil)
    (unless sidebar-exists
      (setq sidebar-current-path project-path-root)
      (setq sidebar-files (sidebar-load-dir project-path-root))
      (sidebar-print)
      (sidebar-goto-buffername buffer-name-current)
      (sidebar-mode)
      (sidebar-git-run))))

(defun sidebar-close ()
  "Close the sidebar for the current frame, you still can reopen it."
  (interactive)
  (let ((sidebar-window (get-buffer-window (sidebar-cons-buffer-name))))
    (when sidebar-window (delete-window sidebar-window))))

;;(kill-buffer (sidebar-cons-buffer-name))

(defun sidebar-disable-current ()
  "Delete everything on the current line and reprint file without the powerline."
  (save-excursion
    (let ((file (nth (- (line-number-at-pos) 1) sidebar-files)))
      (when file
	(delete-region (line-beginning-position) (line-end-position))
	(sidebar-print-file file)))))

(defun sidebar-count-chars-on-line ()
  "Return the number of character on the current line."
  (- (line-end-position) (line-beginning-position)))

(defun sidebar-resize-window ()
  "Resize the sidebar window if the filename on the current line is longer than the window's width."
  (interactive)
  (when (> (sidebar-count-chars-on-line) (window-total-width (get-buffer-window (sidebar-cons-buffer-name))))
    (window-resize (get-buffer-window (sidebar-cons-buffer-name)) (+ 5 (- (sidebar-count-chars-on-line) (window-total-width))) t)))

(defun sidebar-show-current ()
  "Delete everything on current line and reprint file with powerline.
Resize the window if necessary (customizable)."
  (save-excursion
    (let ((file (nth (- (line-number-at-pos) 1) sidebar-files)))
      (when file
	(when (> (window-total-width (get-buffer-window (sidebar-cons-buffer-name))) sidebar-width)
	  (window-resize (get-buffer-window (sidebar-cons-buffer-name)) (- sidebar-width (window-total-width)) t))
	(when sidebar-resize-auto-window
	  (sidebar-resize-window))
	(delete-region (line-beginning-position) (line-end-position))
	(sidebar-print-file file t))
      (when sidebar-message-current
	(message (--getpath file) sidebar-files)))))

(defun sidebar-previous-line ()
  "Go the the previous line.
If we're at the top, go to the last line."
  (interactive)
  (sidebar-disable-current)
  (if (= (line-number-at-pos) 1)
      (forward-line (- (count-lines (point-min) (point-max)) 1))
    (forward-line -1))
  (sidebar-show-current))

(defun sidebar-next-line ()
  "Go the the next line.
If we're at the bottom, got to the first"
  (interactive)
  (sidebar-disable-current)
  (forward-line)
  (let ((numbers-of-line (count-lines (point-min) (point-max))))
    (if (> (line-number-at-pos) numbers-of-line)
	(forward-line (- numbers-of-line))))
  (sidebar-show-current))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-update-to-opened (list path-old)
  "Return LIST with PATH-OLD's 'opened value to t."
  (let ((file (--first (string= (--getpath it) path-old) list)))
    (setf (--opened? file) t))
  list)

(defun sidebar-up-directory ()
  "Go to the parent directory.

Brief:
This function erase the sidebar buffer, print files of the parent
directory, set the current directory to the new one.
Print the previous directory files at the right place

A bit more explained:
The function load files of the parent directory.
It saves all files and directories (including their state) of the current list.
Erases the buffer.
In the list of new files, the current directory (not the new one)'s associated
value `opened' is set to t
Change the current directory to the parent
Print files of the the current directory.
It founds in the new list of printed files the old directory (the one
where we were before).
Once it founds it, jump the cursor to the line just after.
Print all files saved previously.
Update the lines number in the new list of files
Push the saved files in the new list
Sort the list by line number
."
  (interactive)
  (let* ((new-directory (file-name-directory (directory-file-name sidebar-current-path)))
	 (new-files (sidebar-load-dir new-directory))
	 (old-files sidebar-files)
	 (old-dir (directory-file-name sidebar-current-path)))
    (if (string= old-dir new-directory)
	(message "Sidebar: You're at the top")
      (erase-buffer)
      (setq sidebar-files (sidebar-update-to-opened new-files old-dir))
      (setq sidebar-current-path (file-name-as-directory new-directory))
      (setq default-directory sidebar-current-path)
      (setq sidebar-root-project (sidebar-get-root-project))
      (setq sidebar-header-text (abbreviate-file-name new-directory))
      (sidebar-print-listfiles sidebar-files)
      (let* ((old-dir- (--first (string= (--getpath it) old-dir) sidebar-files))
	     (line-to-put-old-files (--getline old-dir-)))
	(sidebar-goto-line (+ line-to-put-old-files 1) t)
	(sidebar-update-line-number (length old-files) line-to-put-old-files)
	(sidebar-print-listfiles old-files)
	(setq sidebar-files (-concat sidebar-files old-files))
	(sidebar-sort-files-by-line)
	(sidebar-goto-line line-to-put-old-files)
	(sidebar-show-current)))
    (sidebar-git-run)))

(defun sidebar-open-directory (file)
  "Set the current directory to FILE.

If FILE is opened (expanded), we filter the list of files to get only the ones
in the new directory.
If FILE it not opened, we load the dir with `\\[sidebar-load-dir]'
."
  (let ((files nil))
    (if (--opened? file)
	(let ((dirname (file-name-as-directory (--getpath file))))
	  (setq files (--filter (s-starts-with? dirname (--getpath it)) sidebar-files)))
      (setq files (sidebar-load-dir (--getpath file))))
    (erase-buffer)
    (setq sidebar-files files)
    (setq sidebar-current-path (file-name-as-directory (--getpath file)))
    (setq default-directory sidebar-current-path)
    (setq sidebar-root-project (sidebar-get-root-project))
    (setq sidebar-header-text (abbreviate-file-name (--getpath file)))
    (sidebar-print-listfiles files)
    (sidebar-goto-line 1)
    (sidebar-show-current))
  (sidebar-git-run))

(defun sidebar-find-file-from-line (line)
  "Return the file on the LINE.
Because sidebar-files is always sorted, it's easy to get it"
  (nth (- line 1) sidebar-files))

(defun sidebar-open-file (file)
  "Open FILE in the buffer where `\\[sidebar-open]' has been called."
  (let ((buffer-file (find-file-noselect (--getpath file))))
    (set-window-buffer (frame-parameter nil 'sidebar-window-origin) buffer-file)))

(defun sidebar-open-line ()
  "Open file or directory of the current line.
If it's a directory, open it in the sidebar.
If it's a file, open it on the window where `\\[sidebar-open]' has been called"
  (interactive)
  (let* ((line (line-number-at-pos))
	 (file (sidebar-find-file-from-line line)))
    (if (--dir? file)
	(sidebar-open-directory file)
      (sidebar-open-file file))))

(defun sidebar-update-line-number (num line)
  "Add NUM to every file where 'line is > than LINE."
  (let ((list sidebar-files))
    (loop-for-each file list
      (let ((line-for-this-file (--getline file)))
	(when (> line-for-this-file line)
	  (setf (--getline file) (+ line-for-this-file num)))))))

(defun sidebar-search-closed-dir (file)
  "Search FILE in the list of previously closed directory.
If found, it is extracted and remove from the list
Return the found element."
  (let ((found (-filter (lambda (list)
			  (string= (car list) (file-name-as-directory (--getpath file))))
			sidebar-closed-directories)))
    (when found
      (setq sidebar-closed-directories
	    (-remove (lambda (list)
		       (string= (car list) (file-name-as-directory (--getpath file))))
		     sidebar-closed-directories)))
    (cdr (car found))))

;;(kill-buffer (sidebar-cons-buffer-name))

(defun sidebar-expand-dir (file line)
  "Expand the directory FILE on the LINE.
If the directory has already been expanded, it get the list of files
from the saved list `\\[sidebar-closed-directories]'.
Otherwise it load the dir with `\\[sidebar-load-dir]'."
  (setf (--opened? file) t)
  (save-excursion
    (let ((new-files (or (sidebar-search-closed-dir file)
			 (sidebar-load-dir (--getpath file)))))
      (forward-line)
      (sidebar-update-line-number (length new-files) line)
      (sidebar-print-listfiles new-files)
      (setq sidebar-files (-concat sidebar-files new-files))
      (sidebar-sort-files-by-line)))
  (save-excursion
    (beginning-of-line)
    (delete-region (line-beginning-position) (line-end-position))
    (sidebar-print-file file t)))

(defun sidebar-delete-line ()
  "Delete the whole line (including \n)."
  (delete-region (line-beginning-position) (line-end-position))
  (delete-char 1))

(defun sidebar-update-closed-dirs (dir list)
  "Insert DIR at the begining of LIST."
  (setq sidebar-closed-directories (-insert-at 0 (-concat (list dir) list) sidebar-closed-directories)))

(defun sidebar-close-dir (file line)
  "Close the opened (expanded) directory FILE on LINE.
All the files in the closed dir are saved in the
list `\\[sidebar-closed-directories]' to reuse them later if
the directory is re-opened"
  (setf (--opened? file) nil)
  (save-excursion
    (let* ((dir-to-close (--getpath (nth (- (line-number-at-pos) 1) sidebar-files)))
	   (dir-to-close (file-name-as-directory dir-to-close))
	   (files-to-remove (--filter (s-starts-with? dir-to-close (--getpath it)) sidebar-files))
	   (new-sidebar-files (--remove (s-starts-with? dir-to-close (--getpath it)) sidebar-files)))
      (sidebar-update-closed-dirs dir-to-close files-to-remove)
      (setq sidebar-files new-sidebar-files)
      (forward-line)
      (sidebar-update-line-number (- (length files-to-remove)) line)
      (dotimes (unused (length files-to-remove))
	(sidebar-delete-line))))
  (save-excursion
    (beginning-of-line)
    (delete-region (line-beginning-position) (line-end-position))
    (sidebar-print-file file t)))

(defun sidebar-expand-or-close-dir ()
  "Expand or close the directory on the current line."
  (interactive)
  (let* ((line (line-number-at-pos))
	 (file (sidebar-find-file-from-line line)))
    (when (--dir? file)
      (if (--opened? file)
	  (sidebar-close-dir file line)
	(sidebar-expand-dir file line)))))

(defun sidebar-goto-line (line &optional force)
  "Go to LINE.
The function checks to not go at the last line (there is no
filename on this line)
if FORCE is non-nil, there is no check."
  (if force
      (forward-line (- line (line-number-at-pos)))
    (let ((max (count-lines (point-min) (point-max))))
      (when (> line max)
	(setq line max))
      (forward-line (- line (line-number-at-pos))))))

(defun sidebar-update-from-opened-dirs (list opened)
  "Set the associated value `opened' to t for all files of LIST present in the list OPENED."
  (loop-for-each file opened
    (let ((found (--first (string= (--getpath it) (--getpath file)) list)))
      (when found
	(setf (--opened? found) t))))
  list)

(defun sidebar-refresh ()
  "Update the list of files in the Sidebar.

The function saves all the directories opened (expanded) in the current sidebar.
Then it load the files of the current directory with `\\[sidebar-load-dir]'
Print them.
For each directory in the list previously saved, it reload the dir
with `\\[sidebar-load-dir]' and print them on the sidebar at the right place."
  (interactive)
  (with-current-buffer (sidebar-get-buffer)
    (let ((opened-dirs (--filter (--opened? it) sidebar-files))
	  (current-line (line-number-at-pos)))
      (setq sidebar-files (sidebar-update-from-opened-dirs (sidebar-load-dir sidebar-current-path) opened-dirs))
      (erase-buffer)
      (sidebar-print-listfiles sidebar-files)
      (sidebar-sort-files-by-line)
      (loop-for-each dir opened-dirs
	(let ((found (--first (string= (--getpath it) (--getpath dir)) sidebar-files)))
	  (when found
	    (setf (--opened? found) t)
	    (sidebar-goto-line (+ (--getline found) 1) t)
	    (let* ((new-files (sidebar-load-dir (--getpath found)))
		   (new-files (sidebar-update-from-opened-dirs new-files opened-dirs)))
	      (sidebar-print-listfiles new-files)
	      (sidebar-update-line-number (length new-files) (--getline found))
	      (setq sidebar-files (-concat sidebar-files new-files))
	      (sidebar-sort-files-by-line)))))
      (sidebar-goto-line current-line)
      (sidebar-show-current))
    (message "Sidebar refreshed")))

(defun sidebar-refresh-on-save-after-timer ()
  "Function called when a buffer is saved, it refreshes the sidebar."
  (let ((sidebar-window (get-buffer-window (sidebar-cons-buffer-name))))
    (when sidebar-window
      (with-current-buffer (sidebar-get-buffer)
	(sidebar-git-run t)))))

(defun sidebar-refresh-on-save ()
  "Function called when a buffer is saved, it refreshes the sidebar.
I'm using a timer because, with my config, flycheck write a temporary
file in the current directory (I don't know why) and it appears in the Sidebar.
So I'm just waiting for it to be delete :/"
  (run-with-idle-timer 2 nil 'sidebar-refresh-on-save-after-timer))

(defun sidebar-delete-buffer-on-kill (frame)
  "When the FRAME is deleted, this function kill the Sidebar buffer associated to it."
  (ignore-errors (kill-buffer (sidebar-cons-buffer-name))))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-git-parse-branch (line)
  "Parse the first LINE of git status with the option `-b'.
The format is `## branchname tracking info'"
  (let* ((str (substring line 3 nil))
	 (str (s-split "\\.\\.\\." str t)))
    (car str)))

(defun sidebar-git-match-status (status)
  "Return the status from the string STATUS according to the man-page git-status."
  (cond ((s-matches? "^ M$" status) 'not-updated)
	((s-matches? "^[MARC] $" status) 'match)
	((s-matches? "^[ MARC]M$" status) 'changed)
	((s-matches? "^M[ MD]$" status) 'updated)
	((s-matches? "^A[ MD]$" status) 'added)
;;;	((s-matches? "^D[ M]$" status) 'deleted) We don't care about deleted files
	((s-matches? "^D[ M]$" status) 'renamed)
;;;	((s-matches? "^[ MARC]D$" status) 'deleted) We don't care about deleted files
	((s-matches? "^\\?\\?$" status) 'untracked)
	((s-matches? "^!!$" status) 'ignored)
	(t 'unknown)))

;; (s-matches? "^ M$" " M") ; not updated
;; (s-matches? "^M[ MD]$" "MM") ; updated in index
;; (s-matches? "^A[ MD]$" "A ") ; added to index
;; (s-matches? "^D[ M]$" "D ") ; deleted from index
;; (s-matches? "^R[ MD]$" "RD") ; renamed in index
;; (s-matches? "^[MARC] $" "M ") ; index and workspace matches
;; (s-matches? "^[ MARC]M$" " M") ; workspace changed since index
;; (s-matches? "^[ MARC]D$" "") ; deleted in workspace
;; (s-matches? "^\\?\\?$" "??") ; untracked file
;; (s-matches? "^!!$" "!!") ; ignored file

(defun sidebar-git-parse-buffer ()
  "Parse the buffer from the git output process.  Return a hashtable.
The key in the hashtable is the filepath, the value is its status."
  (with-current-buffer (sidebar-get-git-buffer)
    (let* ((data (buffer-substring-no-properties (point-min) (point-max)))
	   (str-table (s-split "\0" data t))
	   (table (make-hash-table :test 'equal :size (- (length str-table) 1))))
      (sidebar-git-parse-branch (car str-table))
      (setq str-table (cdr str-table))
      (loop-for-each line str-table
	(let ((status (sidebar-git-match-status (substring line 0 2)))
	      (filepath (substring line 3 nil)))
	  (puthash filepath status table)))
      table)))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-git-sentinel (process change)
  "Sentinel for the PROCESS running git.  Handle exit.
Once the output is parsed, it refreshes the sidebar.
CHANGE is unused"
  (when (eq (process-status process) 'exit)
    (if (/= (process-exit-status process) 0)
	(sidebar-refresh)
      (let ((table (sidebar-git-parse-buffer))
	    (point 0)
	    (sidebar-window (get-buffer-window (sidebar-cons-buffer-name))))
	(with-current-buffer (sidebar-get-buffer)
	  (setq sidebar-git-hashtable table)
	  (setq point (point))
	  (sidebar-refresh))
	(set-window-point sidebar-window point))
      (ignore-errors (kill-buffer (sidebar-get-git-buffer))))))

(defun sidebar-git-run (&optional force)
  "Run git status in the current directory.
The output is parsed to print information of each file in the sidebar.
The process is run only once per project.
Once done, it refresh the sidebar.
if FORCE is non-nil, force to run the process."
  (interactive)
  (if (or force
	  (and sidebar-root-project (not (s-equals? sidebar-root-project sidebar-git-dir))))
      (progn
	(setq sidebar-git-dir sidebar-root-project)
	(let ((process (get-buffer-process (sidebar-get-git-buffer))))
	  (when (and process (process-live-p process))
	    (kill-process process)))
	(with-current-buffer (sidebar-get-git-buffer)
	  (erase-buffer)
	  (let ((process (start-process "sidebar-git" (sidebar-get-git-buffer) "git" "status" "--porcelain" "--ignored" "-z" "-b" ".")))
	    (set-process-query-on-exit-flag process nil)
	    (set-process-sentinel process 'sidebar-git-sentinel))))
    (sidebar-refresh)))

(defun sidebar-refresh-cmd ()
  "Refresh the sidebar.
See `\\[sidebar-git-run]' and `\\[sidebar-refresh]'"
  (interactive)
  (sidebar-git-run t))

(defvar sidebar-mode-map nil
  "Keymap use with sidebar-mode.")
(unless sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "C-x a") 'sidebar-test)
    (define-key map (kbd "q") 'sidebar-close)
    (define-key map (kbd "g") 'sidebar-git-run)
    (define-key map (kbd "SPC") 'sidebar-expand-or-close-dir)
    (define-key map (kbd "DEL") 'sidebar-up-directory)
    (define-key map (kbd "RET") 'sidebar-open-line)
    (define-key map (kbd "h") 'sidebar-refresh-cmd)
    (define-key map (kbd "<up>") 'sidebar-previous-line)
    (define-key map (kbd "C-p") 'sidebar-previous-line)
    (define-key map (kbd "<down>") 'sidebar-next-line)
    (define-key map (kbd "C-n") 'sidebar-next-line)
    (setq sidebar-mode-map map)))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(define-derived-mode sidebar-mode nil "Sidebar"
  "Major mode for Sidebar.

\\{sidebar-mode-map}"
  ::group sidebar

  (if (sidebar-gui?)
      (progn
	(setq sidebar-status-on-directory sidebar-gui-status-on-directory)
	(setq sidebar-filename-colored sidebar-gui-filename-colored)
	(setq sidebar-status-on-file sidebar-gui-status-on-file)
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
	(copy-face 'sidebar-match-gui-face 'sidebar-match-face))
    (setq sidebar-status-on-directory sidebar-terminal-status-on-directory)
    (setq sidebar-filename-colored sidebar-terminal-filename-colored)
    (setq sidebar-status-on-file sidebar-terminal-status-on-file)
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
    (copy-face 'sidebar-match-terminal-face 'sidebar-match-face))

  (make-local-variable 'sidebar-header-text)
  (make-local-variable 'sidebar-files)
  (make-local-variable 'sidebar-current-path)
  (make-local-variable 'sidebar-closed-directories)
  (make-local-variable 'sidebar-root-project)
  (make-local-variable 'sidebar-git-hashtable)
  (make-local-variable 'sidebar-git-dir)
  (setq cursor-type nil)
  (add-hook 'after-save-hook 'sidebar-refresh-on-save t)
  (add-hook 'delete-frame-functions 'sidebar-delete-buffer-on-kill)
  ;;  (use-local-map sidebar-mode-map) ; no need
  (setq header-line-format '(list "-" sidebar-header-text)
	buffer-read-only nil
	mode-line-format nil))

(provide 'sidebar)

;;; sidebar.el ends here
