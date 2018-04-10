;;; sidebar.el --- Sidebar major mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/sidebar.el
;; Keywords: files, convenience, frames
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (dash "2.11.0") (projectile "0.10.0") (s "1.10.0") (dash-functional "1.2.0") (ov "1.0.6") (frame-local "0.0.1"))

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
;; Librairies that are required by this project:
;;
;;  `projectile'
;;  `s'
;;  `dash'
;;  `icons-in-terminal'
;;  `ov'

;;; Code:

(require 'projectile)
(require 's)
(require 'dash)
(require 'dash-functional)
(require 'ov)
(require 'sidebar-filemapping)
(require 'sidebar-select)
(require 'sidebar-utils)
(require 'sidebar-face)
(require 'sidebar-mu4e)
(require 'sidebar-buffers)

(defvar sidebar-insert-fileicon-function 'sidebar-insert-fileicon)

(defmacro sidebar--dir-p (file)
  "Return non-nil if FILE is a directory."
  `(alist-get 'dir ,file))

(defmacro sidebar--open-p (file)
  "Return non-nil if FILE is opened.  Open means extended (only directory can be open)."
  `(alist-get 'opened ,file))

(when (null (require 'icons-in-terminal nil t))
  (defun icons-in-terminal (&rest _)
    "")
  (defun sidebar-insert-fileicon-rescue (file _1 _2 _3 face)
    "FILE FACE."
    (when (sidebar--dir-p file)
      (insert (propertize (if (sidebar--open-p file) "-" "+") 'face face))))
  (setq sidebar-insert-fileicon-function 'sidebar-insert-fileicon-rescue))

(eval-after-load 'dash '(dash-enable-font-lock))

(defconst sidebar-version "0.5.0"
  "Sidebar's version.")

(defgroup sidebar nil
  "Customizable file explorer with git integration."
  :group 'tools
  :group 'convenience
  :link '(custom-manual "(sidebar) Top")
  :link '(info-link "(sidebar) Customizing"))

(defmacro sidebar--getpath (file)
  "Return the path from FILE."
  `(alist-get 'path ,file))

(defmacro sidebar--getline (file)
  "Return the line where is FILE."
  `(alist-get 'line ,file))

(defmacro sidebar-writable (&rest body)
  "Set temporarily the buffer to writable while executing BODY."
  `(-let [buffer-read-only nil]
     ,@body))

(defcustom sidebar-adjust-auto-window-width nil
  "If activated, the sidebar's window will automatically be resize if the..
filename on the current line is longer than the window.
This can be done manually by calling the function `sidebar-adjust-window-width'
or typing `<right>'."
  :type 'boolean
  :group 'sidebar)

(defcustom sidebar-message-current nil
  "If activated, print the full path of the current file in the echo area."
  :type 'boolean
  :group 'sidebar)

(defcustom sidebar-show-hidden-files t
  "If non-nil, hidden files are listed."
  :type 'boolean
  :group 'sidebar)

(defcustom sidebar-width 40
  "Default window width of `sidebar'."
  :type 'integer
  :group 'sidebar)

(defcustom sidebar-icon-dir-closed 'myicons_0013
  "Icon to use before a closed directory.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-icon-dir-opened 'myicons_0014
  "Icon to use before an opened directory.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-icon-git-not-updated 'oct_flame
  "Icon to use with a non updated file (with git).
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-icon-git-updated 'oct_git_commit
  "Icon to use with an updated file (with git).
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-icon-git-changed 'oct_beaker
  "Icon to use with file that has changed since index (with git).
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-icon-git-added 'oct_pulse
  "Icon to use with an added file (with git).
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-icon-git-renamed 'oct_git_renamed
  "Icon to use with a renamed file (with git).
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-icon-git-match 'oct_git_commit
  "Icon to use with a file that matches the index (with git).
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-icon-powerline '(powerline_left_hard_divider 0 -0.05 1.0)
  "Icon to insert at the end of the current line.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type `(radio
	      (const :tag ,(format "Triangle up     %s" (icons-in-terminal 'myicons_0002))
		         (myicons_0002 0 -0.01 1.11))
	      (const :tag ,(format "Triangle bottom %s" (icons-in-terminal 'myicons_0003))
		         (myicons_0003 0 -0.05 1.2))
	      (const :tag ,(format "Wave-Bottom     %s" (icons-in-terminal 'myicons_0009))
		         (myicons_0009 0 -0.05 1.0))
	      (const :tag ,(format "Wave-Up         %s" (icons-in-terminal 'myicons_0008))
		         (myicons_0008 0 -0.05 1.0))
	      (const :tag ,(format "Powerline       %s" (icons-in-terminal 'powerline_left_hard_divider))
		         (powerline_left_hard_divider 0 -0.05 1.0))
	      (const :tag ,(format "Circle          %s" (icons-in-terminal 'powerline_extra_right_half_circle_thick))
		         (powerline_extra_right_half_circle_thick 0 -0.05 1.0))
	      (const :tag ,(format "Flame           %s" (icons-in-terminal 'powerline_extra_flame_thick))
		         (powerline_extra_flame_thick 1 -0.05 1.0))
	      (const :tag ,(format "Squares small   %s" (icons-in-terminal 'powerline_extra_pixelated_squares_small))
		         (powerline_extra_pixelated_squares_small 1 -0.05 1.0))
	      (const :tag ,(format "Squares big     %s" (icons-in-terminal 'powerline_extra_pixelated_squares_big))
		         (powerline_extra_pixelated_squares_big 1 -0.05 1.0))
	      (const :tag ,(format "Ice             %s" (icons-in-terminal 'powerline_extra_ice_waveform))
		         (powerline_extra_ice_waveform 1 -0.05 1.0))
	      (const :tag ,(format "Lego            %s" (icons-in-terminal 'myicons_0010))
		         (myicons_0010 0 0.0 1.0))
	      )
  :group 'sidebar)

(defcustom sidebar-icons-modeline '(myicons_0009 myicons_0007 0)
  "Icons to use in the modeline with branch and remote branch."
  :group 'sidebar
  :type `(radio
          (const :tag ,(format "Triangle up left / bottom right    Left %s  %s Right"
                               (icons-in-terminal 'myicons_0002)
                               (icons-in-terminal 'myicons_0004))
		         (myicons_0002 myicons_0004 0))
          (const :tag ,(format "Triangle bottom left / up right    Left %s  %s Right"
                               (icons-in-terminal 'myicons_0003)
                               (icons-in-terminal 'myicons_0005))
		         (myicons_0003 myicons_0005 0))
          (const :tag ,(format "Triangle bottom                    Left %s  %s Right"
                               (icons-in-terminal 'myicons_0003)
                               (icons-in-terminal 'myicons_0004))
		         (myicons_0003 myicons_0004 0))
          (const :tag ,(format "Triangle up                        Left %s  %s Right"
                               (icons-in-terminal 'myicons_0002)
                               (icons-in-terminal 'myicons_0005))
		         (myicons_0002 myicons_0005 0))
          (const :tag ,(format "Wave-Bottom                        Left %s  %s Right"
                               (icons-in-terminal 'myicons_0009)
                               (icons-in-terminal 'myicons_0007))
		         (myicons_0009 myicons_0007 0))
          (const :tag ,(format "Wave-Up                            Left %s  %s Right"
                               (icons-in-terminal 'myicons_0008)
                               (icons-in-terminal 'myicons_0006))
		         (myicons_0008 myicons_0006 0))
          (const :tag ,(format "Wave up left / bottom right        Left %s  %s Right"
                               (icons-in-terminal 'myicons_0008)
                               (icons-in-terminal 'myicons_0007))
		         (myicons_0008 myicons_0007 0))
          (const :tag ,(format "Wave bottom left / up right        Left %s  %s Right"
                               (icons-in-terminal 'myicons_0009)
                               (icons-in-terminal 'myicons_0006))
		         (myicons_0009 myicons_0006 0))
          (const :tag ,(format "Powerline                          Left %s  %s Right"
                               (icons-in-terminal 'powerline_left_hard_divider)
                               (icons-in-terminal 'powerline_right_hard_divider))
		         (powerline_left_hard_divider powerline_right_hard_divider 0))
          (const :tag ,(format "Circle                             Left %s  %s Right"
                               (icons-in-terminal 'powerline_extra_right_half_circle_thick)
                               (icons-in-terminal 'powerline_extra_left_half_circle_thick))
		         (powerline_extra_right_half_circle_thick powerline_extra_left_half_circle_thick 2))
          (const :tag ,(format "Flame                              Left %s %s Right"
                               (icons-in-terminal 'powerline_extra_flame_thick)
                               (icons-in-terminal 'powerline_extra_flame_thick_mirrored))
		         (powerline_extra_flame_thick powerline_extra_flame_thick_mirrored 2))
          (const :tag ,(format "Squares small                      Left %s %s Right"
                               (icons-in-terminal 'powerline_extra_pixelated_squares_small)
                               (icons-in-terminal 'powerline_extra_pixelated_squares_small_mirrored))
		         (powerline_extra_pixelated_squares_small powerline_extra_pixelated_squares_small_mirrored 2))
          (const :tag ,(format "Squares big                        Left %s %s Right"
                               (icons-in-terminal 'powerline_extra_pixelated_squares_big)
                               (icons-in-terminal 'powerline_extra_pixelated_squares_big_mirrored))
		         (powerline_extra_pixelated_squares_big powerline_extra_pixelated_squares_big_mirrored 2))
          (const :tag ,(format "Ice                                Left %s %s Right"
                               (icons-in-terminal 'powerline_extra_ice_waveform)
                               (icons-in-terminal 'powerline_extra_ice_waveform_mirrored))
		         (powerline_extra_ice_waveform powerline_extra_ice_waveform_mirrored 2))
	      ))

(defcustom sidebar-icon-header-end '(myicons_0008 0)
  "Icon to insert at the end of the header line.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type `(radio
	      (const :tag ,(format "Triangle up     %s" (icons-in-terminal 'myicons_0002))
		         (myicons_0002 1))
	      (const :tag ,(format "Triangle bottom %s" (icons-in-terminal 'myicons_0003))
		         (myicons_0003 1))
	      (const :tag ,(format "Wave-Bottom     %s" (icons-in-terminal 'myicons_0009))
		         (myicons_0009 0))
	      (const :tag ,(format "Wave-Up         %s" (icons-in-terminal 'myicons_0008))
		         (myicons_0008 0))
	      (const :tag ,(format "Powerline       %s" (icons-in-terminal 'powerline_left_hard_divider))
		         (powerline_left_hard_divider 1))
	      (const :tag ,(format "Circle          %s" (icons-in-terminal 'powerline_extra_right_half_circle_thick))
		         (powerline_extra_right_half_circle_thick 1))
	      (const :tag ,(format "Flame           %s" (icons-in-terminal 'powerline_extra_flame_thick))
		         (powerline_extra_flame_thick 2))
	      (const :tag ,(format "Squares small   %s" (icons-in-terminal 'powerline_extra_pixelated_squares_small))
		         (powerline_extra_pixelated_squares_small 2))
	      (const :tag ,(format "Squares big     %s" (icons-in-terminal 'powerline_extra_pixelated_squares_big))
		         (powerline_extra_pixelated_squares_big 2))
	      (const :tag ,(format "Ice             %s" (icons-in-terminal 'powerline_extra_ice_waveform))
		         (powerline_extra_ice_waveform 2))
	      )
  :group 'sidebar)

(defcustom sidebar-icon-header-project 'oct_repo
  "Icon to insert before a project name.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-icon-header-directory 'oct_file_directory
  "Icon to insert before a directory name.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-icon-branch 'oct_git_branch
  "Icon to insert before the name of the current branch.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-icon-remotebranch 'oct_git_branch
  "Icon to insert after the name of the remote branch.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-mode-line-height 1.5
  "(GUI) Height of the mode line."
  :type 'float
  :group 'sidebar)

(defcustom sidebar-header-line-height 1.5
  "(GUI) Height of the header line."
  :type 'float
  :group 'sidebar)

(defcustom sidebar-status-on-file t
  "Insert icon after the filename according to its git status.

Default: t."
  :type 'boolean
  :group 'sidebar)

(defcustom sidebar-status-on-directory 'on-closed
  "Control when to place icon of git status on directories.

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

(defcustom sidebar-filename-colored nil
  "The filename will be colored according to its git status.
Untracked and ignored files will always be colored.
You can change their colors if you don't want them to be
colored.

Default: nil."
  :type 'boolean
  :group 'sidebar)

(defcustom sidebar-check-update t
  "If non nil, sidebar checks if we're using the last version.
Default: non-nil."
  :type 'boolean
  :group 'sidebar)

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defvar-local sidebar-files-number 0
  "Number of files listed in the sidebar.")

(defvar-local sidebar-directories-number 0
  "Number of directories listed in the sidebar.")

(defvar sidebar-button-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'sidebar-open-line)
    (define-key map [mouse-2] 'sidebar-open-line)
    map)
  "Keymap for file button.")

(defvar sidebar--streched-spaces-p (intern-soft ":re-align")
  "Non-nil if streched spaces are supported.
Apply this patch to support it:
https://gist.github.com/sebastiencs/2f066f8d12b71f40fda9bdb979fe971d")

(defsubst sidebar-get-root-project ()
  "Return the project directory, nil if there is no project."
  (ignore-errors (projectile-project-root)))

(defsubst sidebar-get-lsp-root ()
  "Return the lsp root directory, or nil."
  (and (bound-and-true-p lsp--cur-workspace)
       (fboundp 'lsp--workspace-root)
       (lsp--workspace-root lsp--cur-workspace)))

(defsubst sidebar-project-root ()
  "Return the project root using projectile.
If it's not a project, return the file's directory.
If it's not a file, return the home directory."
  (or (sidebar-get-lsp-root)
      (sidebar-get-root-project)
      (when buffer-file-name (file-name-directory buffer-file-name))
      (file-name-as-directory (expand-file-name "~"))))

(defsubst sidebar-get-curl-buffer ()
  "Return the buffer associated to the curl buffer for the current frame."
  (get-buffer-create (sidebar-cons-buffer-name "-CURL")))

(defsubst sidebar-get-git-buffer ()
  "Return the existing/created sidebar git buffer for the current frame."
  (get-buffer-create (sidebar-cons-buffer-name "-GIT")))

(defsubst sidebar-exists-p ()
  "Check if a sidebar for the frame exists."
  (get-buffer (sidebar-cons-buffer-name)))

(defun sidebar-count-char (char s)
  "Count the occurence of CHAR in string S."
  (let ((n 0))
    (dotimes (it (length s) n)
      (when (eq (elt s it) char)
	    (setq n (1+ n))))))

(defun sidebar-calc-depth (path status)
  "Calcul the depth of FILE from the current directory point of view.
This is used to count the number of space to insert before the filename.
STATUS is the status of the FILE."
  (let* ((path-from-current (s-chop-prefix default-directory path))
         (depth (sidebar-count-char ?\/ path-from-current))) ; TODO: Support Windows (replace '/')
    (if (> depth 0)
        (1+ (* depth 2))
      (1+ depth))))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     ;;(message "%.06f" (float-time (time-since time)))
     ))

(defvar-local sidebar--root-project nil)
(defvar-local sidebar--git-hashtable nil)
(defvar-local sidebar--git-directories nil)

(defun sidebar-status-from-parent (file-path)
  "If FILE-PATH is in a untracked or ignored directory, return its status."
  (when sidebar--git-directories
    (let ((list sidebar--git-directories)
          found)
      (while (and list (not found))
        (when (string-prefix-p (caar list) file-path)
          (setq found (cdar list)))
        (setq list (cdr list)))
      found)))

(defsubst sidebar-color-from-status (status &optional default)
  "Return the face associated to the git STATUS.
If there is no status, return DEFAULT."
  (pcase status
    ('not-updated 'sidebar-not-updated)
    ('updated 'sidebar-updated)
    ('changed 'sidebar-changed)
    ('added 'sidebar-added)
    ('renamed 'sidebar-renamed)
    ('match 'sidebar-match)
    (_ default)))

(defun sidebar-insert-status-subfiles-helper (status number file path)
  "Insert the icon and numbers after a directory.
STATUS is use to know which icon to insert
NUMBER is the number to insert
FILE PATH"
  (concat
   (sidebar-insert-status file path status t)
   ;; https://gist.github.com/sebastiencs/2f066f8d12b71f40fda9bdb979fe971d
   (propertize " " 'display '(space :re-align t))
   (number-to-string number)))

(defun sidebar-status-subfiles (path)
  "Return the numbers of git status in the subfiles of directory PATH."
  (let ((not-updated 0) (updated 0) (untracked 0) (changed 0) (added 0) (renamed 0) (match 0))
    (-when-let (hashtable (sidebar-get git-hashtable))
      (maphash (lambda (key value)
		         (when (s-starts-with? path key)
		           (pcase value
                     ('not-updated (setq not-updated (1+ not-updated)))
			         ('updated (setq updated (1+ updated)))
			         ('changed (setq changed (1+ changed)))
			         ('added (setq added (1+ added)))
			         ('match (setq match (1+ match))))))
	           hashtable))
    `((not-updated . ,not-updated) (updated . ,updated) (untracked . ,untracked)
      (changed . ,changed) (added . ,added) (renamed . ,renamed) (match . ,match))))

(defun sidebar-insert-status-subfiles-p (file)
  "Return non-nil if we have to insert the icons for the subfiles of FILE."
  (when (sidebar--dir-p file)
    (cond ((equal sidebar-status-on-directory 'always) t)
	      ((and (equal sidebar-status-on-directory 'on-closed) (not (sidebar--open-p file))) t)
	      ((and (equal sidebar-status-on-directory 'on-opened) (sidebar--open-p file)) t))))

(defun sidebar-insert-status-subfiles (file path)
  "Insert the subfiles's status of the directory FILE.
PATH is the path of FILE."
  (when (sidebar-insert-status-subfiles-p file)
    (-let (((&alist 'not-updated not-updated
		            'updated updated
		            'changed changed
		            'added added
		            'match match)
	        (sidebar-status-subfiles path))
	       (func 'sidebar-insert-status-subfiles-helper))
      (concat
       (when (> not-updated 0)
	     (funcall func 'not-updated not-updated file path))
       (when (> updated 0)
	     (funcall func 'updated updated file path))
       (when (> changed 0)
	     (funcall func 'changed changed file path))
       (when (> added 0)
	     (funcall func 'added added file path))
       (when (> match 0)
	     (funcall func 'match match file path))))))

(defsubst sidebar-insert-filename (str face)
  "Small function to insert STR with FACE if non-nil."
  (propertize str 'face face 'mouse-face face 'keymap sidebar-button-keymap))

(defun sidebar-insert-icon (icon face)
  "Insert ICON with FACE if non-nil."
  (icons-in-terminal icon :face face :height 1.12))

(defun sidebar-insert-fileicon (file filename status path face)
  "Insert the icon associated to FILE.
It should insert the directories icons for directories and icons
associated to their extensions for files.
FILENAME is the filename of FILE.
STATUS is its git status.
PATH its path.
FACE is the face to use for the icons."
  (if (sidebar--dir-p file)
      (let ((icon (if (sidebar--open-p file) sidebar-icon-dir-opened sidebar-icon-dir-closed)))
	    (sidebar-insert-icon icon face))
    (-let* (((icon . color) (sidebar-filemapping-lookup filename))
	        (partial (-partial 'icons-in-terminal icon :height 1.12)))
      (cond (face (funcall partial :face face))
	        (color (funcall partial :foreground color))
	        (t (funcall partial))))))

(defsubst sidebar-window-width ()
  "Return the sidebar's window width."
  (window-width (sidebar-get-window)))

(defun sidebar-insert-icon-filename (file filename status path)
  "Insert the icon associated to FILE followed by its FILENAME.
STATUS is its git status.
PATH is the file path."
  (let* ((status (or (sidebar-status-from-parent path) status))
         (dir-p (sidebar--dir-p file))
         (icon-color (or (and (eq 'ignored status)
                              (if dir-p 'sidebar-ignored-dir 'sidebar-ignored-file))
                         (and dir-p 'sidebar-dir)))
         (file-color (or (and (eq 'ignored status)
                              (if dir-p 'sidebar-ignored-dir 'sidebar-ignored-file))
                         (and (eq 'untracked status) 'sidebar-untracked)
                         (and dir-p 'sidebar-dir)
                         (and sidebar-filename-colored
                              (sidebar-color-from-status status 'sidebar-file))
                         'sidebar-file)))
    (concat
     (funcall sidebar-insert-fileicon-function file filename status path icon-color)
     ;; Use a streched space, so filenames are always aligned no matter the icon's width
     ;; Apply this patch to emacs source:
     ;; https://gist.github.com/sebastiencs/2f066f8d12b71f40fda9bdb979fe971d
     (when sidebar--streched-spaces-p
       (propertize " " 'display '(space :re-align t)))
     (propertize " " 'display `(space :width 0.6))
     (sidebar-insert-filename filename file-color))))

(defun sidebar-insert-status (file path status &optional dir)
  "Insert the icons associated to the git status of FILE.
PATH is its path.
STATUS is the git status of that file.
DIR is non-nil if it's a directory."
  (when (or sidebar-status-on-file dir)
    (let* ((face (sidebar-color-from-status status nil))
	       (func (lambda (name)
		           (concat
                    ;; https://gist.github.com/sebastiencs/2f066f8d12b71f40fda9bdb979fe971d
                    (when sidebar--streched-spaces-p
                      (propertize " " 'display '(space :re-align t)))
                    (propertize " " 'display '(space :width 0.5))
 		            (funcall 'sidebar-insert-icon name face)))))
      (pcase status
	    ('not-updated (funcall func sidebar-icon-git-not-updated))
	    ('updated (funcall func sidebar-icon-git-updated))
	    ('changed (funcall func sidebar-icon-git-changed))
	    ('added (funcall func sidebar-icon-git-added))
	    ('renamed (funcall func sidebar-icon-git-renamed))
	    ('match (funcall func sidebar-icon-git-match))))))

(sidebar-print-function file (file &optional no-newline)
  "Insert FILE's filename on the current LINE.

First, it inserts ' ' x times, depending on the file depth (relative to
 the current directory).
This is followed by the icon of that file.  The icon is selected according to
its extension (or if it's a directory).
Then it inserts the filename.
If FILE is a directory and closed (not expanded), it inserts the icons of
 all the files it contains just after its name, still on the same line.

FILE is an alist created from `sidebar-file-struct'."
  (let* ((path (sidebar--getpath file))
         (filename (and path (file-name-nondirectory path)))
	     (path-in-project (s-chop-prefix sidebar--root-project path))
	     (path-fixed-dirname (if (sidebar--dir-p file) (file-name-as-directory path-in-project) path-in-project))
	     (status (and sidebar--git-hashtable
                      (gethash path-fixed-dirname sidebar--git-hashtable)))
	     (depth (sidebar-calc-depth path status)))
    (concat
     (make-string depth ?\s)
     (sidebar-insert-icon-filename file filename status path-fixed-dirname)
     (sidebar-insert-status file path-fixed-dirname status)
     (sidebar-insert-status-subfiles file path-fixed-dirname)
     (unless no-newline "\n"))))

(defun sidebar-print-listfiles (list)
  "Insert all files in LIST from the current line.
It updates the associated value `line' for each file.  This is used to
keep track of which file is on which line."
  (sidebar-writable
   (let ((fn (sidebar-get print-item))
         (sidebar--root-project (sidebar-get root-project))
         (sidebar--git-hashtable (sidebar-get git-hashtable))
         (default-directory (sidebar-get current-path)))
     (mapc fn list))))

(defun sidebar-toggle-hidden-files ()
  "Toggle listing of hidden files."
  (interactive)
  (setq sidebar-show-hidden-files (not sidebar-show-hidden-files))
  (sidebar-refresh))

(defun sidebar-dots-file (file)
  "Return t if FILE is '.' or '..'."
  (let ((file (file-name-nondirectory file)))
    (or (string= "." file)
        (string= ".." file)
        (and (not sidebar-show-hidden-files)
             (string= "." (substring file 0 1))))))

(sidebar-content-provider files (path)
  "Return a list of files/directories in PATH.
It removes '.' and '..'
Sort the directories first
The list returned is a list of association list for each file created
with `\\[sidebar-file-struct]'"
  (let* ((files-and-dirs (--> path (directory-files it t) (-remove 'sidebar-dots-file it)))
	     (dirs-sorted (-filter 'file-directory-p files-and-dirs))
	     (files-sorted (--filter (not (file-directory-p it)) files-and-dirs)))
    (-concat dirs-sorted files-sorted)))

(defsubst sidebar-load-content (path)
  "Function that call 'load-content-function'.
It varies for files, buffers or mu4e.
PATH is used only for files, it represents the path where to load new files."
  (funcall (sidebar-get load-content-function) path))

(defun sidebar-goto-buffername (buffer-name)
  "This function jump the cursor to BUFFER-NAME (string) in the sidebar.
If there is no filename equal to the BUFFER-NAME, it put the cursor
on the frame parameter `sidebar-line-to-start'.
This is use when the sidebar is created."
  (let* ((file (and buffer-name (sidebar--find-file buffer-name)))
	     (line (or (sidebar-get save-line) (sidebar--getline file) (sidebar-get line-to-start))))
    (sidebar-goto-line line)
    (sidebar-show-current)
    (when (and (sidebar-get restore-function)
	           (sidebar-get window-start))
      (set-window-start (sidebar-get-window) (sidebar-get window-start)))
    (sidebar-set save-line nil)))

(defun sidebar-expand-path (project-path-root file-path)
  "Return a list of files and dirs to open from PROJECT-PATH-ROOT to FILE-PATH."
  (-when-let* ((base project-path-root)
	           (dirs-to-open (-some->> file-path
				                       (s-chop-prefix (-> base directory-file-name))
				                       (s-chop-suffix (-> file-path file-name-nondirectory))
				                       directory-file-name
				                       sidebar-split-path)))
    (-map 'sidebar-file-struct
	      (--map (setq base (concat (file-name-as-directory base) it))
		         dirs-to-open))))

(defun sidebar-split-path (path)
  "Return a list of directory names from PATH.
Example:
  path = \"/var/dir/other\"
return = (\"var\" \"dir\" \"other\")"
  (sidebar-split-path-helper path ()))

(defun sidebar-split-path-helper (path accum)
  "Help function for `sidebar-split-path'.
PATH ACCUM."
  (let ((dir (-> path file-name-directory directory-file-name))
	    (name (file-name-nondirectory path)))
    (if (equal dir path)
	    accum
      (sidebar-split-path-helper dir (cons name accum)))))

(defun sidebar-check-setup ()
  "Check if the font ‘icons-in-terminal’ is installed.
TODO: Check with terminals too (now it checks only with GUI), `font-info'
returns an error on terminals."
  (when (and (sidebar-gui-p) (not (font-info "icons-in-terminal")))
    ;; (ignore-errors (kill-buffer (sidebar-cons-buffer-name)))
    (message "The font icons-in-terminal is not installed: see https://github.com/sebastiencs/sidebar.el")))

(defvar sidebar-mode-association
  '(:sidebar-mode
    ((sidebar-mode-to-use                  . sidebar-mode)
     (sidebar-load-content-function        . sidebar-content-files)
     (sidebar-make-header-function         . sidebar-make-header)
     (sidebar-make-modeline-left-function  . sidebar-make-modeline-left)
     (sidebar-make-modeline-right-function . sidebar-make-modeline-right)
     (sidebar-item-builder-function        . sidebar-file-struct)
     (sidebar-restore-function             . sidebar-restore-state)
     (sidebar-print-item                   . sidebar-print-file)
     (sidebar-line-to-start                . 1))
    :sidebar-mu4e-mode
    ((sidebar-load-content-function        . sidebar-content-mu4e)
     (sidebar-mode-to-use                  . sidebar-mu4e-mode)
     (sidebar-make-header-function         . sidebar-mu4e-make-header)
     (sidebar-make-modeline-left-function  . sidebar-mu4e-make-modeline-left)
     (sidebar-make-modeline-right-function . sidebar-mu4e-make-modeline-right)
     (sidebar-item-builder-function        . sidebar-mu4e-item-builder)
     (sidebar-restore-function             . nil)
     (sidebar-print-item                   . sidebar-print-mu4e)
     (sidebar-line-to-start                . 1))
    :sidebar-buffers-mode
    ((sidebar-load-content-function        . sidebar-content-buffers)
     (sidebar-mode-to-use                  . sidebar-buffers-mode)
     (sidebar-make-header-function         . sidebar-buffers-make-header)
     (sidebar-make-modeline-left-function  . sidebar-buffers-make-modeline-left)
     (sidebar-make-modeline-right-function . sidebar-buffers-make-modeline-right)
     (sidebar-item-builder-function        . sidebar-buffers-item-builder)
     (sidebar-restore-function             . nil)
     (sidebar-print-item                   . sidebar-print-buffers)
     (sidebar-line-to-start                . 1))
    )
  "Variable storing the functions to call according to the mode used.")

(defun sidebar-init-vars (project-path-root)
  "Initialize the variables on sidebar startup.
PROJECT-PATH-ROOT."
  (let ((mode (cond ((sidebar-mu4e?)    :sidebar-mu4e-mode)
		            ((sidebar-buffers?) :sidebar-buffers-mode)
		            (t                  :sidebar-mode))))
    (--each (plist-get sidebar-mode-association mode)
      (sidebar-set1 (car it) (cdr it))))
  (if (and (sidebar-get restore-function) (sidebar-get saved-state-files))
      (funcall (sidebar-get restore-function))
    (sidebar-set root-project (sidebar-project-root))
    (sidebar-set history (list project-path-root))
    (sidebar-set current-path project-path-root)
    (sidebar-set window-start nil)
    (setq default-directory project-path-root)
    (sidebar-set default-width sidebar-width)
    ;; (sidebar-set files (sidebar-load-content project-path-root))
    (unless (sidebar-get overlay)
      (sidebar-set overlay (ov (point-min) (point-min) 'face 'sidebar-powerline)))))

(defun sidebar-open ()
  "Open or create a sidebar for the current frame."
  (interactive)
  (sidebar-set window-origin (get-buffer-window))
  (when (or (sidebar-get mu4e-force) (sidebar-mu4e?))
    (sidebar-kill))
  (if (and (eq (sidebar-get mode-to-use) 'sidebar-buffers-mode)
           (not (eq this-command 'sidebar-buffers-open)))
      (sidebar-buffers-switch-to-files t)
    (let ((sidebar-exists (sidebar-exists-p))
	      (sidebar-buffer (sidebar-get-buffer))
	      (sidebar-window (sidebar-get-window))
	      (project-path-root (sidebar-project-root))
	      (buffer-name-current (buffer-file-name)))
      (set-window-buffer sidebar-window sidebar-buffer)
      (set-buffer sidebar-buffer)
      (select-window sidebar-window)
      (internal-show-cursor (sidebar-get-window) nil)
      (unless sidebar-exists
        (sidebar-check-setup)
        (sidebar-init-vars project-path-root)
        (funcall (sidebar-get mode-to-use))
        (sidebar-refresh (sidebar-expand-path project-path-root buffer-name-current))
        (sidebar-goto-buffername (and (eq (sidebar-get mode-to-use) 'sidebar-mode)
                                      buffer-name-current))
        (sidebar-curl-run)
        (unless (sidebar-get saved-state-files)
	      (sidebar-git-run nil t))))))

(defun sidebar-close ()
  "Close the sidebar for the current frame.
If you want to kill the sidebar and its buffer, use `sidebar-kill'"
  (interactive)
  (-when-let (sidebar-window (sidebar-get-window t))
    (delete-window sidebar-window)))

;;(kill-buffer (sidebar-cons-buffer-name))

(defun sidebar-adjust-window-width (&optional n-chars-on-line sidebar-window)
  "Resize the sidebar window if the filename on the current line is longer...
than the window's width.
N-CHARS-ON-LINE is the number of characters on the current line.
SIDEBAR-WINDOW is sidebar's window."
  (interactive)
  (let ((window (or sidebar-window (sidebar-get-window t)))
	    (chars (or n-chars-on-line (- (line-end-position) (line-beginning-position)))))
    (when (> chars (window-total-width window))
      (sidebar-set-window (+ 3 chars)))))

(defun sidebar-reset-window-width ()
  "Reset the sidebar window to the default value `sidebar-width'."
  (interactive)
  (sidebar-set-window))

(defun sidebar-make-powerline ()
  "Return a string to make a poweline effect.
The effect is made by inserting space with a colored background
and an icon at the end.
See `sidebar-move-overlay'."
  (let* ((icon sidebar-icon-powerline)
	     (face 'sidebar-powerline))
    (concat
     (propertize " " 'face face 'display `(space :align-to (- right-fringe ,(if (sidebar-gui-p) 2 3))))
     (icons-in-terminal (car icon)
		                :raise (car (cddr icon))
		                :height (cadr (cddr icon))
		                :foreground (face-background face nil t)))))

(defun sidebar-move-overlay (beg end window)
  "Move the overlay that make the powerline effect.
The overlay is just a colored background (more precisely a face).
The overlay is only inserted from the beginning of the line to the end
of the filename.  The remaining space and the icon at the end is a string
that we insert with the 'after-string overlay property.
This string is generated for each line, to make it fit for the filename
width and the window.
BEG is the position at the beginning of the line.
END is the position at the end of the line.
WINDOW is the sidebar's window."
  (let* ((ov (sidebar-get overlay)))
    (ov-move ov beg end)
    (unless (ov-val ov 'after-string)
      (ov-set ov 'after-string (sidebar-make-powerline)))))

(defun sidebar-update-path-on-header ()
  "Function that update the suffix on the header.
The suffix represents the path of the file pointed by the
cursor (current line).
The header is asked to be update only when the suffix change."
  (-when-let* ((path-file (-some-> (sidebar--getpath (sidebar-find-file-from-line)) file-name-directory))
	           (current-path (sidebar-get current-path))
	           (suffix-path (when (>= (length path-file) (length current-path))
                              (substring path-file (1- (length current-path))))))
    (-let [suffix-header (sidebar-get suffix-header)]
      (when (or (null suffix-header)
		        (not (s-equals? suffix-path suffix-header)))
	    (sidebar-set suffix-header suffix-path)
	    (force-mode-line-update)))))

(defun sidebar-show-current ()
  "Show the current line with a background color and powerline effect.
Resize the window if necessary (customizable)."
  (save-excursion
    (-when-let* ((sidebar-window (sidebar-get-window t))
		         (line-begin (line-beginning-position))
		         (line-end (line-end-position)))
      (sidebar-move-overlay line-begin line-end sidebar-window)
      (set-window-fringes sidebar-window nil nil nil)
      (set-window-margins sidebar-window 0 0)
      (sidebar-update-path-on-header)
      (when sidebar-adjust-auto-window-width
	    (sidebar-adjust-window-width (- line-end line-begin) sidebar-window))
      (-when-let* ((_ sidebar-message-current)
		           (path (sidebar--getpath (sidebar-find-file-from-line))))
	    (message (abbreviate-file-name path))))))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-create-directory ()
  "Create a directory and its parents if non existing."
  (interactive)
  (let* ((new-dir (-some->> (sidebar-find-file-from-line)
			                sidebar--getpath
			                file-name-directory
			                (read-string "[sidebar] Create directory: "))))
    (if (file-exists-p new-dir)
	    (error "[sidebar] Cannot create directory %s, it already exists" new-dir)
      (condition-case err
	      (progn
	        (make-directory new-dir t)
	        (message "[sidebar] directory created")
	        (sidebar-git-run t))
	    (error "[sidebar] Error: %s" (error-message-string err))))))

(defun sidebar-create-file ()
  "Create a new file.
The file should be in an existing directory.
To create directories, see `sidebar-create-directory'"
  (interactive)
  (let* ((new-file (-some--> (sidebar-find-file-from-line)
			                 sidebar--getpath
			                 (or it (sidebar-get current-path))
			                 file-name-directory
			                 (read-string "[sidebar] Create file: " it))))
    (cond
     ((file-exists-p new-file)
      (error "[sidebar] Cannot create file %s, it already exists" new-file))
     ((-> new-file file-writable-p not)
      (error "[sidebar] Cannot create file %s, directory not writable" new-file))
     (t (-when-let (sidebar-window (sidebar-get window-origin))
	      (select-window sidebar-window)
	      (find-file new-file)
	      (save-buffer)
	      (message "[sidebar] File %s created" new-file)
	      (sidebar-refresh))))))

(defun sidebar-delete-selected ()
  "Delete the file/directory on the current line.
If it's a directory, it removes recursively its subfiles."
  (interactive)
  (let* ((file-at-line (sidebar-find-file-from-line))
	     (path (sidebar--getpath file-at-line))
	     (str-prompt (format "[sidebar] Delete the %s %s ? ('yes' or anything else to cancel): "
			                 (if (sidebar--dir-p file-at-line) "directory" "file")
			                 path))
	     (confirm (read-string str-prompt)))
    (when (s-equals? "yes" confirm)
      (if (sidebar--dir-p file-at-line) (delete-directory path t) (delete-file path))
      (sidebar-refresh))))

(defun sidebar-rename-buffer-name (buffers name new-name)
  "Rename a buffer.
BUFFERS is a list of buffers.
NAME is the old name.
NEW-NAME is the new name."
  (while buffers
    (when (equal (buffer-file-name (car buffers)) name)
      (when (y-or-n-p (format "Rename the buffer %s ? " (buffer-name (car buffers))))
	    (with-current-buffer (car buffers)
	      (set-visited-file-name new-name t t)
	      (save-buffer))))
    (setq buffers (cdr buffers))))

(defun sidebar-rename-selected ()
  "Rename the file/directory on the current line.
If there are buffers visiting this file, you'll be ask to rename them too."
  (interactive)
  (let* ((file-at-line (sidebar-find-file-from-line))
	     (path (sidebar--getpath file-at-line))
	     (directory (file-name-directory path))
	     (new-name (read-string (format "[sidebar] Rename %s to: " (file-name-nondirectory path)))))
    (if (file-exists-p new-name)
	    (error "[sidebar] Cannot rename to %s, it already exists" new-name)
      (rename-file path (concat directory new-name))
      (sidebar-rename-buffer-name (buffer-list) path new-name)
      (sidebar-refresh))))

(defvar-local sidebar-file-to-copy
  '(:file nil :method 'copy)
  "Variable that holds the file to copy.
Methods are copy or cut.")

(defun sidebar-copy-selected ()
  "Copy the file/directory on the current line.
To paste it, use `sidebar-paste'."
  (interactive)
  (let ((file-at-line (sidebar-find-file-from-line)))
    (plist-put sidebar-file-to-copy :file file-at-line)
    (plist-put sidebar-file-to-copy :method 'copy)
    (message "%s to copy: '%s'" (if (sidebar--dir-p file-at-line) "Directory" "File")
	         (file-name-nondirectory (sidebar--getpath file-at-line)))))

(defun sidebar-cut-selected ()
  "Cut the file/directory on the current line.
to paste it, use `sidebar-paste'."
  (interactive)
  (let ((file-at-line (sidebar-find-file-from-line)))
    (if (not (file-writable-p (sidebar--getpath file-at-line)))
	    (error "[sidebar] Cannot cut file, it's non writable")
      (plist-put sidebar-file-to-copy :file file-at-line)
      (plist-put sidebar-file-to-copy :method 'cut)
      (message "%s to cut: '%s'" (if (sidebar--dir-p file-at-line) "Directory" "File")
	           (file-name-nondirectory (sidebar--getpath file-at-line))))))

(defun sidebar-paste ()
  "Paste the file/directory previously copied/cut.
The file will be paste to the path of the file on the current line.
If the file on the current line is a directory, it pastes the file outside it.
To paste the file inside the directory, it has to be open (expand).

If the file is cut, you'll be ask to rename the buffers visiting it."
  (interactive)
  (-let* ((file-at-line (sidebar-find-file-from-line))
	      (directory (if (sidebar--open-p file-at-line) (file-name-as-directory (sidebar--getpath file-at-line))
		               (file-name-directory (sidebar--getpath file-at-line))))
	      ((&plist :method method) sidebar-file-to-copy)
	      ((&plist :file file) sidebar-file-to-copy)
	      (path (sidebar--getpath file))
	      (new-file (->> path file-name-nondirectory (concat directory)))
	      (str-prompt (format "%s %s to %s ? "
			                  (if (equal method 'copy) "Copy" "Cut")
			                  (file-name-nondirectory path)
			                  directory)))
    (when (y-or-n-p str-prompt)
      (cond
       ((file-exists-p new-file)
	    (error "[sidebar] Cannot paste file.  Already exists"))
       ((not (file-writable-p directory))
	    (error "[sidebar] Cannot create file, directory not writable"))
       (t (condition-case err
	          (progn
		        (if (sidebar--dir-p file)
		            (copy-directory path directory t t nil)
		          (copy-file path directory nil t t t))
		        (when (equal method 'cut)
		          (if (sidebar--dir-p file)
		              (delete-directory path t)
		            (delete-file path)
		            (sidebar-rename-buffer-name (buffer-list) path new-file)))
		        (sidebar-refresh))
	        (error "Error while copying file: " (error-message-string err))))))))

(defun sidebar-update-to-opened (list path-old)
  "Return LIST with PATH-OLD's 'opened value to t."
  (let ((file (--first (string= (sidebar--getpath it) path-old) list)))
    (setf (sidebar--open-p file) t))
  list)

(defun sidebar-init-buffer ()
  "Intialize the buffer before inserting anything in it."
  (-let [sidebar-window (sidebar-get-window t)]
    (set-window-margins sidebar-window 0 0)
    (set-window-fringes sidebar-window nil nil nil))
  (delete-overlay (sidebar-get overlay))
  (sidebar-writable
   (erase-buffer)
   (remove-overlays)
   (unless (eq major-mode 'sidebar-buffers-mode)
     (overlay-put (make-overlay (point) (point))
                  'after-string "\n"))))

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
  (let* ((new-directory (file-name-directory (directory-file-name (sidebar-get current-path))))
	     (new-files (sidebar-load-content new-directory))
	     (old-files (sidebar--list-all))
	     (history (sidebar-get history))
	     (old-dir (directory-file-name (sidebar-get current-path))))
    (if (string= old-dir new-directory)
	    (message "Sidebar: You're at the top")
      (sidebar-init-buffer)
      ;; (sidebar-set history (add-to-list 'history new-directory nil 's-equals?))
      ;;(sidebar-set history (add-to-list 'history new-directory nil 's-equals?))
      (sidebar-set current-path (file-name-as-directory new-directory))
      (setq default-directory (sidebar-get current-path))
      (projectile-reset-cached-project-root)
      (sidebar-set root-project (sidebar-project-root))
      (sidebar--maybe-invalidate-git)
      (sidebar-print-listfiles (sidebar-update-to-opened new-files old-dir))
      (let* ((line-to-put-old-files (sidebar--getline (sidebar--find-file old-dir))))
	    (sidebar-goto-line (+ line-to-put-old-files 1) t)
	    (sidebar-print-listfiles old-files)
	    (sidebar-goto-line line-to-put-old-files)
        ))
    (recenter)
    (sidebar-update-files-number)
    ;;(sidebar-git-run)
    ))

(defun sidebar-open-directory (file)
  "Set the current directory to FILE.

If FILE is opened (expanded), we filter the list of files to get only the ones
in the new directory.
If FILE it not opened, we load the dir with `sidebar-content-files'
."
  (let ((history (sidebar-get history)))
    (projectile-reset-cached-project-root)
    (sidebar-set current-path (file-name-as-directory (sidebar--getpath file)))
    (setq default-directory (sidebar-get current-path))
    ;;(sidebar-set history (add-to-list 'history default-directory nil 's-equals?))
    (sidebar-set root-project (sidebar-project-root))
    (sidebar-set line-on-refresh 1)
    (sidebar-set window-start 1)
    (sidebar-git-run)))

(defsubst sidebar-open-file-in-window (window buffer-file)
  "Callback function called when a window has been selected by the user.
WINDOW is the window in which BUFFER-FILE will be display."
  (set-window-buffer window buffer-file))

(defun sidebar-list-windows-others-frame (frames)
  "Return a list of windows from FRAMES.
We exclude the non-visible frames and frame used by the daemon."
  (let ((windows nil))
    (--each frames
      (when (and (not (equal it (window-frame)))
		         (not (s-equals? "initial_terminal" (terminal-name it)))
		         (frame-visible-p it))
	    (setq windows (append windows (-remove 'window-dedicated-p (window-list it))))))
    windows))

(defun sidebar-open-in-window ()
  "Open a file in a selected window.
A list of windows will be shown to the user to select the one in which to
open the buffer.
Only the windows non dedicated are shown."
  (interactive)
  (let* ((file (sidebar-find-file-from-line))
	     (windows-in-frame (-remove 'window-dedicated-p (window-list)))
	     (list-frames (frame-list))
	     (windows-in-others-frame (sidebar-list-windows-others-frame list-frames)))
    (if (sidebar--dir-p file)
	    (sidebar-open-directory file)
      (sidebar-select-make-buffer (list windows-in-frame windows-in-others-frame)
				                  " Select a window "
				                  " Others frames "
				                  (lambda (x) (s-chop-suffix ">" (s-replace "#<window " "#" (format "%s" x))))
				                  sidebar-select-icon-before-window
				                  'sidebar-open-file-in-window
				                  (find-file-noselect (sidebar--getpath file))))))

(defun sidebar-find-file-from-line (&optional line)
  "Return the file on the LINE.
'sidebar-files' is always sorted, so we just need to access it in the list by
its index."
  (get-text-property (point) 'sidebar-item))

(defun sidebar-open-file (file)
  "Open FILE in the buffer where `sidebar-open' has been called.
If the window doesn't exists anymore, the function calls
`sidebar-open-in-window'.
If you want to select the window, you should call `sidebar-open-in-window'"
  (let ((buffer-file (find-file-noselect (sidebar--getpath file)))
	    (window (sidebar-get window-origin)))
    (if (window-live-p window)
	    (set-window-buffer window buffer-file)
      (sidebar-set window-origin (sidebar-open-in-window)))))

(defun sidebar-open-line ()
  "Open file or directory of the current line.
If it's a directory, open it in the sidebar.
If it's a file, open it on the window where `sidebar-open' has been called.
If you want to select the window, you should call `sidebar-open-in-window'"
  (interactive)
  (-when-let (file (sidebar-find-file-from-line))
    (if (sidebar--dir-p file)
	    (sidebar-open-directory file)
      (sidebar-open-file file))))

(defun sidebar-search-closed-dir (file)
  "Search FILE in the list of previously closed directory.
If found, it is extracted and remove from the list
Return the found element."
  (-when-let (found (--filter (string= (car it) (-> file sidebar--getpath file-name-as-directory))
			                  (sidebar-get closed-directories)))
    (sidebar-set closed-directories
      (--remove (string= (car it) (-> file sidebar--getpath file-name-as-directory))
		        (sidebar-get closed-directories)))
    (cdar found)))

(defun sidebar--update-dirline (dir)
  "DIR."
  (save-excursion
    (beginning-of-line)
    (sidebar-writable
     (delete-region (line-beginning-position) (line-end-position))
     (let ((sidebar--git-hashtable (sidebar-get git-hashtable))
           (sidebar--root-project (sidebar-get root-project)))
       (sidebar-print-file dir t)))
    (sidebar-show-current)))

(defun sidebar-expand-dir (file line)
  "Expand the directory FILE on the LINE.
If the directory has already been expanded, it get the list of files
from the saved list `sidebar-closed-directories'.
Otherwise it load the dir with `sidebar-content-files'."
  (setf (sidebar--open-p file) t)
  (save-excursion
    (forward-line)
    (sidebar-print-listfiles (or (sidebar-search-closed-dir file)
			                     (sidebar-load-content (sidebar--getpath file)))))
  (sidebar--update-dirline file))

(defun sidebar-delete-line ()
  "Delete the whole line (including \n)."
  (sidebar-writable
   (delete-region (line-beginning-position) (line-end-position))
   (delete-char 1)))

(defun sidebar-update-closed-dirs (dir list)
  "Insert DIR at the begining of LIST."
  (--> (-concat (list dir) list)
       (-insert-at 0 it (sidebar-get closed-directories))
       (sidebar-set closed-directories it)))

(defun sidebar-close-dir (file line)
  "Close the opened (expanded) directory FILE on LINE.
All the files in the closed dir are saved in the
list 'sidebar-closed-directories' to reuse them later if
the directory is re-opened"
  (setf (sidebar--open-p file) nil)
  (save-excursion
    (let* ((dir-to-close (-> (sidebar--getpath (sidebar--get-file))
	                         (file-name-as-directory)))
	       (files-to-remove (--filter (s-starts-with? dir-to-close (sidebar--getpath it))
                                      (sidebar--list-all))))
      (sidebar-update-closed-dirs dir-to-close files-to-remove)
      (forward-line)
      (--dotimes (length files-to-remove)
	    (sidebar-delete-line))))
  (sidebar--update-dirline file))

(defun sidebar-expand-or-close-dir ()
  "Expand or close the directory on the current line."
  (interactive)
  (let ((line (line-number-at-pos))
	    (file (sidebar-find-file-from-line)))
    (when (sidebar--dir-p file)
      (if (sidebar--open-p file)
	      (sidebar-close-dir file line)
	    (sidebar-expand-dir file line)))))

(defun sidebar-goto-line (line &optional force)
  "Go to LINE.
The function checks to not go at the last line (there is no
filename on this line) or at the first line.
if FORCE is non-nil, there is no check."
  (when (integerp line)
	(forward-line (- line (line-number-at-pos)))))

(defun sidebar-update-from-opened-dirs (list opened)
  "Set the associated value `opened' to t for all files of LIST present in the list OPENED."
  (--each opened
    (-when-let* ((path (sidebar--getpath it))
		         (found (--first (string= (sidebar--getpath it) path) list)))
      (setf (sidebar--open-p found) t)))
  list)

(defun sidebar--find-file-point (path)
  "PATH."
  (save-excursion
    (goto-char 1)
    (let ((item (get-text-property (point) 'sidebar-item))
          point)
      (while (and item (not point))
        (if (string= path (sidebar--getpath item))
            (setq point (point))
          (forward-line)
          (setq item (get-text-property (point) 'sidebar-item))))
      point)))

(defun sidebar--find-file (path)
  "PATH."
  (-when-let* ((point (sidebar--find-file-point path))
               (file (get-text-property point 'sidebar-item)))
    (setf (sidebar--getline file) (line-number-at-pos point))
    file))

(defun sidebar--get-file ()
  "PATH."
  (get-text-property (point) 'sidebar-item))

(defun sidebar-list-opened ()
  (save-excursion
    (goto-char 1)
    (let ((prop (get-text-property (point) 'sidebar-item))
          list)
      (while prop
        (when (sidebar--open-p prop)
          (push prop list))
        (forward-line 1)
        (setq prop (get-text-property (point) 'sidebar-item)))
      (nreverse list))))

(defun sidebar--list-all ()
  (save-excursion
    (goto-char 1)
    (let ((prop (get-text-property (point) 'sidebar-item))
          list)
      (while prop
        (push prop list)
        (forward-line 1)
        (setq prop (get-text-property (point) 'sidebar-item)))
      (nreverse list))))

(defun sidebar--maybe-invalidate-git ()
  (unless (equal (sidebar-get root-project) (sidebar-get git-dir))
    (sidebar-set git-branches nil)
    (sidebar-set git-dir nil)
    (sidebar-set git-hashtable nil)))

(defun sidebar-update-files-number ()
  "Update the variables `sidebar-files-number' and `sidebar-directories-number'."
  (let* ((list (sidebar--list-all))
         (total (length list))
	     (ndirs (--count (sidebar--dir-p it) list)))
    (setq sidebar-files-number (- total ndirs)
	      sidebar-directories-number ndirs)))

(defun sidebar-refresh (&optional to-expand silent)
  "Update the content of the sidebar.

The function saves all the directories opened (expanded) in the current sidebar.
Then it load the files of the current directory with `sidebar-load-content'
Print them.
For each directory in the list previously saved, it reload the dir
with `sidebar-load-content' and print them on the sidebar at the right place.

TO-EXPAND is used on the sidebar creation with files, it's a list of
files/directories to expand.

SILENT."
  (interactive)
  (with-current-buffer (sidebar-get-buffer)
    (let* ((current-line (or (sidebar-get line-on-refresh) (line-number-at-pos)))
	       (opened-dirs (or (sidebar-get opened) to-expand (sidebar-list-opened)))
           (files (-> (sidebar-load-content (sidebar-get current-path))
			          (sidebar-update-from-opened-dirs opened-dirs))))
      (sidebar-set line-on-refresh current-line)
      (sidebar-set opened nil)
      (sidebar--maybe-invalidate-git)
      (sidebar-init-buffer)
      (sidebar-print-listfiles files)
      (--each opened-dirs
	    (-when-let* ((path (sidebar--getpath it))
	                 (found (sidebar--find-file path)))
	      (sidebar-goto-line (+ (sidebar--getline found) 1) t)
	      (sidebar-print-listfiles (-> (sidebar-load-content (sidebar--getpath found))
	    	                           (sidebar-update-from-opened-dirs opened-dirs)))))
      (sidebar--restore-point current-line)
      (sidebar-set line-on-refresh nil)
      (sidebar-show-current))
    (sidebar-update-files-number)
    (unless silent
      ;; (message "Sidebar refreshed")
      )))

(defun sidebar--restore-point (line)
  "Restore the current LINE."
  (-if-let (win (sidebar-get-window t))
      (progn
        (with-selected-window win
          (sidebar-goto-line line))
        (-when-let (start (sidebar-get window-start))
          (set-window-start win start)
          (sidebar-set window-start nil)))
    (sidebar-goto-line line)))

(defun sidebar-refresh-on-save-after-timer ()
  "Function called when a buffer is saved, it refreshes the sidebar."
  (-when-let (sidebar-window (sidebar-get-window t))
    (with-current-buffer (sidebar-get-buffer)
      (message "run from sidebar-refresh-on-save-after-timer")
      (sidebar-git-run t))))

(defmacro sidebar-protect-repetition (name time &rest body)
  "NAME TIME BODY."
  (declare (indent 2))
  (let ((n (intern (format "sidebar-repet-%s" name))))
    `(progn
       (unless (sidebar-get ,n)
	     (sidebar-set ,n t)
	     (run-at-time ,time nil (lambda () (sidebar-set ,n nil)))
	     ,@body))))

(defun sidebar-refresh-on-save ()
  "Function called when a buffer is saved, it refreshes the sidebar.
I'm using a timer because, with my config, flycheck write a temporary
file in the current directory (I don't know why) and it appears in the sidebar.
So I'm just waiting for it to be delete :/
It doesn't refresh the sidebar when using the sidebar over tramp."
  (unless (sidebar-tramp-p)
    ;; (sidebar-protect-repetition on-save 2
    (run-with-timer 2 nil 'sidebar-refresh-on-save-after-timer)))

(defun sidebar-delete-buffer-on-kill (frame)
  "When the FRAME is deleted, this function kill the Sidebar buffer associated to it."
  (ignore-errors (kill-buffer (sidebar-get buffer-name frame))))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-curl-sentinel (process change)
  "Sentinel for the PROCESS running curl.
CHANGE is unused"
  (when (eq (process-status process) 'exit)
    (if (= (process-exit-status process) 0)
	    (with-current-buffer (sidebar-get-curl-buffer)
	      (let ((data (buffer-substring-no-properties (point-min) (point-max))))
	        (when (and (= (length data) 1)
		               (s-equals? data "0"))
	          (message "A new version of sidebar.el is available !")))))
    (ignore-errors (kill-buffer (sidebar-get-curl-buffer)))))

(defun sidebar-curl-run ()
  "Run curl to determine if we're using the last version."
  (when (and sidebar-check-update
	         (executable-find "curl"))
    (let ((process (get-buffer-process (sidebar-get-curl-buffer))))
      (when (and process (process-live-p process))
	    (kill-process process)))
    (with-current-buffer (sidebar-get-curl-buffer)
      (erase-buffer)
      (let* ((url (concat "http://sidebar.chapu.is/islast?version=" sidebar-version))
	         (process (start-process "curl-process" (sidebar-get-curl-buffer) "curl" url)))
	    (set-process-query-on-exit-flag process nil)
	    (set-process-sentinel process 'sidebar-curl-sentinel)))))

(defun sidebar-git-parse-branch (line)
  "Parse the first LINE of git status with the option `-b'.
The format is `## branchname tracking info'"
  (let* ((str (substring line 3 nil))
	     (str (s-split "\\.\\.\\." str t)))
    (with-current-buffer (sidebar-get-buffer)
      (sidebar-set git-branches (cons (car str) (cadr str))))))

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
      (--each str-table
	    (let ((status (sidebar-git-match-status (substring it 0 2)))
	          (filepath (substring it 3 nil)))
	      (puthash filepath status table)))
      table)))

(defun sidebar-git-sentinel (process change)
  "Sentinel for the PROCESS running git.  Handle exit.
Once the output is parsed, it refreshes the sidebar.
CHANGE is unused"
  (when (eq (process-status process) 'exit)

    (sidebar--update-git-status (and (= (process-exit-status process) 0)
                                     'sidebar-git-parse-buffer)
                                t)
    (ignore-errors (kill-buffer (sidebar-get-git-buffer)))))

(defun sidebar-tramp-p ()
  "Return non-nil if we're browsing a remote directory."
  (let ((path (sidebar-get current-path)))
    (and (stringp path) (file-remote-p path))))

(defun sidebar--update-git-status (fn &optional refresh win-start)
  "FN REFRESH WIN-START."
  (let ((hashtable (and fn (funcall fn)))
        (branches (and (fboundp 'sidebar-git-branches)
                       (sidebar-git-branches))))
    (setq sidebar--git-directories nil)
    (when hashtable
      (maphash (lambda (key val)
                 (when (directory-name-p key)
                   (push (cons key val) sidebar--git-directories)))
               hashtable))
    (sidebar-set git-branches (and (fboundp 'sidebar-git-branches) branches))
    (sidebar-set git-dir (and (or hashtable branches) (sidebar-get root-project)))
    (sidebar-set git-hashtable hashtable)
    (when win-start
      (sidebar-set window-start win-start))
    (when (or hashtable refresh)
      (sidebar-refresh))))

(defun sidebar-git-run (&optional force first-run)
  "Run git status in the current directory.
The output is parsed to print information of each file in the sidebar.
The process is run only once per project.
Once done, it refresh the sidebar.
if FORCE is non-nil, force to run the process.
FIRST-RUN."
  (-when-let (win (sidebar-get-window t))
    (unless (or (sidebar-get window-start)
                first-run)
      (sidebar-set window-start (window-start win))))
  (cond
   ((and (equal (sidebar-get root-project) (sidebar-get git-dir))
         (not force))
    (sidebar-refresh))
   ((and (fboundp 'make-thread) (fboundp 'sidebar-git-status))
    (let ((win-start (sidebar-get window-start)))
      (make-thread (lambda nil (sidebar--update-git-status 'sidebar-git-status nil win-start))))
    (sidebar-refresh))
   (t (let ((process (get-buffer-process (sidebar-get-git-buffer))))
        (if (process-live-p process)
            ;; (kill-process process)
            (message "PROCESS ALIVE !")
          (with-current-buffer (sidebar-get-git-buffer)
            (erase-buffer))
          (let ((process (start-process "sidebar-git" (sidebar-get-git-buffer)
  			                            "git" "status" "--porcelain" "--ignored" "-z" "-b")))
            (set-process-query-on-exit-flag process nil)
            (set-process-sentinel process 'sidebar-git-sentinel)))))))

(defun sidebar-refresh-cmd ()
  "Refresh the sidebar content.
See `sidebar-git-run' and `sidebar-refresh'"
  (interactive)
  ;;;(sidebar-protect-repetition on-refresh 1
  ;;(message "LAAA")
  (sidebar-git-run t))

(defun sidebar-make-header ()
  "Return the string to insert in the header."
  (let* ((project (sidebar-get root-project))
	     (current-path (sidebar-get current-path))
	     (project-name (or project (abbreviate-file-name current-path)))
	     (suffix (sidebar-get suffix-header)))
    (when project
      (setq project-name (--> (file-name-directory (directory-file-name project-name))
			                  (s-chop-prefix it current-path)
			                  (s-chop-suffix "/" it))))
    (concat
     " "
     (if project
	     (icons-in-terminal sidebar-icon-header-project
			                :face 'sidebar-icon-header-project
			                :background (face-background 'sidebar-header-line nil t)
			                :raise -0.07
			                :height 1.3)
       (icons-in-terminal sidebar-icon-header-directory
			              :face 'sidebar-icon-header-directory
			              :background (face-background 'sidebar-header-line nil t)
			              :raise -0.0
			              :height 1.3))
;;;     (when (not (sidebar-gui-p)) " ")
     " "
     (propertize project-name 'display '(raise 0.12))
     (when (and suffix (> (length suffix) 1))
       (propertize (if project suffix (substring suffix 1))
		           'face 'sidebar-suffix-path-header
		           'display '(raise 0.12)
		           'font-lock-ignore t)))))

(defun sidebar-truncate (string len)
  "Truncate STRING if it's longer than LEN."
  (if (> (length string) len)
      (concat (substring string 0 (- len 2)) "..")
    string))

(defvar sidebar-header-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'sidebar-up-directory)
    (define-key map [header-line mouse-2] 'sidebar-up-directory)
    (define-key map [header-line mouse-3] 'sidebar-up-directory)
    map)
  "Keymap for the header line.")

(defun sidebar-set-header ()
  "Format the header with the string from `sidebar-make-header-function'."
  (let* ((window-width (sidebar-window-width))
	     (string (-some-> (funcall (sidebar-get make-header-function))
			              (sidebar-truncate (1- window-width))))
         (len-icon (if (sidebar-gui-p) (cadr sidebar-icon-header-end) 1)))
    (add-face-text-property 0 (length string) 'sidebar-header-line t string)
    (add-text-properties 0 (length string) '(mouse-face sidebar-header-line) string)
    (add-text-properties 0 (length string) `(keymap ,sidebar-header-keymap) string)
    (concat
     string
     (propertize " " 'face 'sidebar-header-line 'display `(space :align-to (- right-fringe 1 ,len-icon)))
     (icons-in-terminal (car sidebar-icon-header-end)
			            :foreground (face-background 'sidebar-header-line nil t)
			            :height sidebar-header-line-height))))

(defun sidebar-make-modeline-left ()
  "Return the string to insert in the modeline (left side).
It returns the git branch or the number of files in the sidebar if we're
not in a git project."
  (if (and (sidebar-get root-project) (sidebar-get git-branches))
      (concat
       " "
       (icons-in-terminal sidebar-icon-branch
			              :face 'sidebar-icon-branch
			              :background (face-background 'sidebar-branch nil t)
			              :raise -0.1
			              :height 1.3)
       (when (not (sidebar-gui-p)) " ")
       (propertize (or (car (sidebar-get git-branches)) "")
		           'face `(:inherit sidebar-branch :background ,(face-background 'sidebar-branch nil t))
		           'display '(raise 0.1)))
    (concat " "
	        (number-to-string sidebar-files-number)
	        (if (> sidebar-files-number 1) " files" " file"))))

(defun sidebar-make-modeline-right ()
  "Return the string to insert in the modeline (right side).
It returns the git remote branch or the number of directories in the sidebar
if we're not in a git project."
  (let* ((branches (sidebar-get git-branches))
         (distant (cdr branches)))
    (if (and (sidebar-get root-project) branches)
        (let ((str-branch-distant (and distant (s-split " \\[\\|\\]" distant))))
	      (concat
	       (icons-in-terminal sidebar-icon-remotebranch
	  		                  :face 'sidebar-icon-remote-branch
	  		                  :background (face-background 'sidebar-remote-branch nil t)
	  		                  :raise -0.1
	  		                  :height 1.3)
	       (when (not (sidebar-gui-p)) " ")
	       (propertize (or (car str-branch-distant) "") 'display '(raise 0.1))
	       "  "))
      (concat (number-to-string sidebar-directories-number)
	          (if (> sidebar-directories-number 1) " directories" " directory")))))

(defun sidebar-set-modeline ()
  "Construct the mode-line."
  (when (not (sidebar-get select-active))
    (let* ((left (concat (funcall (sidebar-get make-modeline-left-function)) " "))
	       (right (concat " " (funcall (sidebar-get make-modeline-right-function))))
           (align-to (+ (length right) (if (sidebar-gui-p) (car (cddr sidebar-icons-modeline)) 0))))
      (when (> (length left) 1)
	    (add-face-text-property 0 (length left) 'sidebar-branch nil left)
	    (setq left (concat left (icons-in-terminal (car sidebar-icons-modeline)
						                           :foreground (face-background 'sidebar-branch nil t)
						                           :raise -0.1
						                           :height sidebar-mode-line-height))))
      (when (> (length right) 1)
	    (add-face-text-property 0 (length right) 'sidebar-remote-branch nil right)
	    (setq right (concat (icons-in-terminal (cadr sidebar-icons-modeline)
					                           :foreground (face-background 'sidebar-remote-branch nil t)
					                           :raise -0.1
					                           :height sidebar-mode-line-height)
			                right)))
      (concat left
              (propertize " " 'display `(space :align-to (- right-margin ,align-to 2)))
	          right
              (propertize " " 'face 'sidebar-remote-branch 'display `(space :align-to (right-margin)))))))

(defadvice sidebar-post-command (around intercept activate)
  (condition-case err
      ad-do-it
    ((debug error) (signal (car err) (cdr err)))))

(defun sidebar-post-command()
  "Function called after every command.
This ensure that the overlay is a the cursor place.
It also make the cursor not being at the first or last point in the buffer
because there are nothing at those points."
  ;; (message "last command: %s" this-command)
  (when (eobp)
    (ignore-errors (forward-line -1)))
  (sidebar-show-current))

(defun sidebar-before-make-frame-hook ()
  "This hook run when another frame is created.
When another frame is created, if the current window selected is the
sidebar, it opens a sidebar in the new frame.  I don't know why.
This function just select another window before the frame is created."
  (let ((selected-window (frame-selected-window))
	    (windows-in-frame (window-list))
	    (sidebar-window (sidebar-get-window t)))
    (when (equal selected-window sidebar-window)
      (let ((other-window (--first (not (equal sidebar-window it)) windows-in-frame)))
	    (set-frame-selected-window nil other-window)))))

(defun sidebar-debug-vars (&optional all)
  "Print all sidebar's variables in a buffer.
Only 'closed-directories', 'files' and 'save-files' are not printed.
If ALL is non-nil, it print everything."
  (interactive)
  (let (variables)
    (--each (frame-parameters)
      (let ((var (symbol-name (car it))))
	    (when (s-starts-with? "sidebar-" var)
	      (push `(,(substring var 8) ,(cdr it)) variables))))
    (unless all
      (setq variables (--remove (or (s-equals? "closed-directories" (car it))
				                    ;; (s-equals? "files" (car it))
				                    ;; (s-equals? "save-files" (car it))
                                    )
				                variables)))
    (-let [buffer (get-buffer-create "sidebar-debug")]
      (with-current-buffer buffer
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    (insert (format "%s\n" (propertize (format-time-string "= %A %H:%M:%S =")
					                       'face '(:foreground "red"))))
	    (--each (--sort (string< (car it) (car other)) variables)
	      (insert (format "%s: %s\n"
			              (propertize (car it) 'face '(:foreground "yellow"))
			              (cadr it))))
	    (setq buffer-read-only t)
	    (goto-char 1))
      (display-buffer buffer '(display-buffer-in-side-window . ((side . right) (window-width . 0.40)))))))

(defun sidebar-history-open (dir)
  "Callback called when the user has selected a DIR to open in the history."
  (select-window (sidebar-get-window))
  (sidebar-open-directory (sidebar-file-struct dir)))

(defun sidebar-history ()
  "Show the last opened directories with the possibility to open one of them."
  (interactive)
  (sidebar-select-make-buffer (list (sidebar-get history) nil)
			                  " Last directories "
			                  ""
			                  (lambda (x) (abbreviate-file-name x))
			                  sidebar-select-icon-before-directory
			                  'sidebar-history-open
			                  nil))

(defun sidebar-help ()
  "Function to display an help for sidebar."
  (interactive)
  (describe-mode))

(defun sidebar-config-change-hook ()
  "If some other window change sidebar's width, this function resize it."
  (when (and (not (equal this-command 'sidebar-adjust-window-width))
	         (window-live-p (sidebar-get-window t))
	         (sidebar-get default-width)
	         (/= (window-total-width (sidebar-get-window t)) (sidebar-get default-width)))
    (save-excursion
      (sidebar-set-window (sidebar-get default-width)))))

(defun sidebar-config-change-hook-local ()
  (-some-> (sidebar-get-window t)
           (set-window-margins 0 0)))

(defun sidebar-save-state ()
  "Save the sidebar state."
  (sidebar-set window-start (window-start (sidebar-get-window)))
  (with-current-buffer (sidebar-get-buffer)
    (sidebar-set save-line-files (line-number-at-pos)))
  (sidebar-set files-opened (sidebar-list-opened))
  (sidebar-set save-default-width (sidebar-get default-width))
  (sidebar-set save-root-project (sidebar-get root-project))
  (sidebar-set save-cwd default-directory)
  (sidebar-set save-history (sidebar-get history))
  (sidebar-set save-current-path (sidebar-get current-path)))

(defun sidebar-restore-state ()
  "Restore the sidebar state."
  (sidebar-set save-line (sidebar-get save-line-files))
  (sidebar-set default-width (sidebar-get save-default-width))
  (setq default-directory (sidebar-get save-cwd))
  (sidebar-set root-project (sidebar-get save-root-project))
  (sidebar-set history (sidebar-get save-history))
  (sidebar-set opened (sidebar-get files-opened))
  (sidebar-set current-path (sidebar-get save-current-path)))

(defun sidebar-switch-to-buffers ()
  "Switch to the list of buffers."
  (interactive)
  (sidebar-save-state)
  (ignore-errors (kill-buffer (sidebar-cons-buffer-name)))
  (sidebar-set buffers-force t)
  (sidebar-set saved-state-files t)
  (sidebar-set save-line nil)
  (sidebar-open)
  (sidebar-goto-line (sidebar-get buffers-line)))

(defun sidebar-kill ()
  "Close the sidebar and its buffer."
  (interactive)
  (sidebar-set saved-state-files nil)
  (sidebar-set window-start nil)
  (sidebar-set save-line-files nil)
  (sidebar-set save-files nil)
  (sidebar-set save-default-width nil)
  (sidebar-set save-root-project nil)
  (sidebar-set save-history nil)
  (sidebar-set save-current-path nil)
  (sidebar-set default-width nil)
  (sidebar-set root-project nil)
  (sidebar-set history nil)
  (sidebar-set git-hashtable nil)
  (sidebar-set root-project nil)
  (sidebar-set git-dir nil)
  (sidebar-set opened nil)
  (sidebar-set current-path nil)
  (ignore-errors (kill-buffer (sidebar-cons-buffer-name))))

(defface sidebar-empty-face
  '(())
  "Face used to initialize the header and modeline faces."
  :group nil)

(defun sidebar-init-mode ()
  "Initialize the sidebar modes."

  (set (make-local-variable 'face-remapping-alist)
       '((header-line sidebar-empty-face)
	     (header-line-inactive sidebar-empty-face)
	     (mode-line sidebar-empty-face)
	     (mode-line-inactive sidebar-empty-face)))

  (setq cursor-type nil
        tab-width 1
        truncate-lines t
	    buffer-read-only t
	    mode-line-format '(:eval (sidebar-set-modeline))
	    header-line-format '(:eval (sidebar-set-header)))

  (remove-hook 'post-command-hook 'global-hl-line-highlight))

(defvar sidebar-mode-map nil
  "Keymap uses with ‘sidebar-mode’.")
(unless sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q") 'sidebar-close)
    (define-key map (kbd "C-q") 'sidebar-kill)
    (define-key map (kbd "SPC") 'sidebar-expand-or-close-dir)
    (define-key map (kbd "DEL") 'sidebar-up-directory)
    (define-key map (kbd "RET") 'sidebar-open-line)
    (define-key map (kbd "M-RET") 'sidebar-open-in-window)
    (define-key map (kbd "g") 'sidebar-refresh-cmd)
    (define-key map (kbd "C-t") 'sidebar-history)
    (define-key map (kbd "n") 'sidebar-create-file)
    (define-key map (kbd "i") 'sidebar-create-directory)
    (define-key map (kbd "C-d") 'sidebar-delete-selected)
    (define-key map (kbd "C") 'sidebar-copy-selected)
    (define-key map (kbd "M-C") 'sidebar-cut-selected)
    (define-key map (kbd "P") 'sidebar-paste)
    (define-key map (kbd "h") 'sidebar-toggle-hidden-files)
    (define-key map (kbd "R") 'sidebar-rename-selected)
    (define-key map (kbd "<tab>") 'sidebar-switch-to-buffers)
    (define-key map (kbd "TAB") 'sidebar-switch-to-buffers)
    (define-key map (kbd "<right>") 'sidebar-adjust-window-width)
    (define-key map (kbd "<left>") 'sidebar-reset-window-width)
    (define-key map (kbd "?") 'sidebar-help)
    (setq sidebar-mode-map map)))

(define-derived-mode sidebar-mode special-mode "Sidebar"
  "Major mode for Sidebar.

\\{sidebar-mode-map}"
  ::group sidebar

  (make-local-variable 'post-command-hook)

  (sidebar-init-mode)

  (add-hook 'post-command-hook 'sidebar-post-command)
  (add-hook 'after-save-hook 'sidebar-refresh-on-save t)
  (add-hook 'delete-frame-functions 'sidebar-delete-buffer-on-kill)
  (add-hook 'before-make-frame-hook 'sidebar-before-make-frame-hook)
  (add-hook 'window-configuration-change-hook 'sidebar-config-change-hook)
  (add-hook 'window-configuration-change-hook 'sidebar-config-change-hook-local nil t)

  )

(provide 'sidebar)

;;; sidebar.el ends here
