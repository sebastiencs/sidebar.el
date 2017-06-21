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

(when (null (require 'icons-in-terminal nil t))
  (defun icons-in-terminal (&rest _)
    "")
  (defun sidebar-insert-fileicon-rescue (file _1 _2 _3 face)
    "FILE FACE."
    (when (--dir? file)
      (insert (propertize (if (--opened? file) "-" "+") 'face face))))
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

(defmacro sidebar-writable (&rest body)
  "Set temporarily the buffer to writable while executing BODY."
  `(-let [buffer-read-only nil]
     ,@body))

(defcustom sidebar-adjust-auto-window-width nil
  "If activated, the sidebar's window will automatically be resize if the..
filename on the current line is longer than the window.
This can be done manually by calling the function `sidebar-adjust-window-width'
or by binding a key to it."
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

;;;"Insert the remaining spaces and a '' to make a powerline effect."

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

(defcustom sidebar-icons-branches-modeline '(myicons_0009 myicons_0007 0)
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

(defcustom sidebar-icon-branch-end 'myicons_0008
  "Icon to insert at the end of the current branch.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type `(radio
	  (const :tag ,(format "Triangle up     %s" (icons-in-terminal 'myicons_0002))
		 myicons_0002)
	  (const :tag ,(format "Triangle bottom %s" (icons-in-terminal 'myicons_0003))
		 myicons_0003)
	  (const :tag ,(format "Wave-Bottom     %s" (icons-in-terminal 'myicons_0009))
		 myicons_0009)
	  (const :tag ,(format "Wave-Up         %s" (icons-in-terminal 'myicons_0008))
		 myicons_0008)
	  (const :tag ,(format "Powerline       %s" (icons-in-terminal 'powerline_left_hard_divider))
		 powerline_left_hard_divider)
	  (const :tag ,(format "Circle          %s" (icons-in-terminal 'powerline_extra_right_half_circle_thick))
		 powerline_extra_right_half_circle_thick)
	  (const :tag ,(format "Flame           %s" (icons-in-terminal 'powerline_extra_flame_thick))
		 powerline_extra_flame_thick)
	  (const :tag ,(format "Squares small   %s" (icons-in-terminal 'powerline_extra_pixelated_squares_small))
		 powerline_extra_pixelated_squares_small)
	  (const :tag ,(format "Squares big     %s" (icons-in-terminal 'powerline_extra_pixelated_squares_big))
		 powerline_extra_pixelated_squares_big)
	  (const :tag ,(format "Ice             %s" (icons-in-terminal 'powerline_extra_ice_waveform))
		 powerline_extra_ice_waveform)
	  (const :tag ,(format "Trapezoid       %s" (icons-in-terminal 'powerline_extra_trapezoid_top_bottom))
		 powerline_extra_trapezoid_top_bottom)
	  )
  :group 'sidebar)

(defcustom sidebar-icon-branch 'oct_git_branch
  "Icon to insert before the name of the current branch.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type 'symbol
  :group 'sidebar)

(defcustom sidebar-icon-remotebranch-start 'myicons_0007
  "Icon to insert before the remote branch.
To get a list of the icons names, you can run:
 `~/.local/share/icons-in-terminal/print_icons.sh --names'
More info at URL `https://github.com/sebastiencs/icons-in-terminal'."
  :type `(radio
	  (const :tag ,(format "Triangle up     %s" (icons-in-terminal 'myicons_0005))
		 myicons_0005)
	  (const :tag ,(format "Triangle bottom %s" (icons-in-terminal 'myicons_0004))
		 myicons_0004)
	  (const :tag ,(format "Wave-Bottom     %s" (icons-in-terminal 'myicons_0007))
		 myicons_0007)
	  (const :tag ,(format "Wave-Up         %s" (icons-in-terminal 'myicons_0006))
		 myicons_0006)
	  (const :tag ,(format "Powerline       %s" (icons-in-terminal 'powerline_right_hard_divider))
		 powerline_right_hard_divider)
	  (const :tag ,(format "Circle          %s" (icons-in-terminal 'powerline_extra_left_half_circle_thick))
		 powerline_extra_left_half_circle_thick)
	  (const :tag ,(format "Flame           %s" (icons-in-terminal 'powerline_extra_flame_thick_mirrored))
		 powerline_extra_flame_thick_mirrored)
	  (const :tag ,(format "Squares small   %s" (icons-in-terminal 'powerline_extra_pixelated_squares_small_mirrored))
		 powerline_extra_pixelated_squares_small_mirrored)
	  (const :tag ,(format "Squares big     %s" (icons-in-terminal 'powerline_extra_pixelated_squares_big_mirrored))
		 powerline_extra_pixelated_squares_big_mirrored)
	  (const :tag ,(format "Ice             %s" (icons-in-terminal 'powerline_extra_ice_waveform_mirrored))
		 powerline_extra_ice_waveform_mirrored)
	  (const :tag ,(format "Trapezoid       %s" (icons-in-terminal 'powerline_extra_trapezoid_top_bottom_mirrored))
		 powerline_extra_trapezoid_top_bottom_mirrored)
	  )
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

Default: nil."
  :type 'boolean
  :group 'sidebar)

(defcustom sidebar-check-update t
  "If non nil, sidebar checks if we're using the last version.
Default: non-nil."
  :type 'boolean
  :group 'sidebar)

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defvar-local sidebar-files-number 0)
(defvar-local sidebar-directories-number 0)

(defvar sidebar-buffer-name nil
  "Name of the sidebar buffer for the current frame.")

(defvar sidebar-button-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'sidebar-open-line)
    (define-key map [mouse-2] 'sidebar-open-line)
    map)
  "Keymap for file button.")

(defsubst sidebar-get-root-project ()
  "Return the project directory, nil if there is no project."
  (ignore-errors (projectile-project-root)))

(defsubst sidebar-project-root ()
  "Return the project root using projectile.
If it's not a project, return the file's directory.
If it's not a file, return the home directory."
  (or (sidebar-get-root-project)
      (when buffer-file-name (file-name-directory buffer-file-name))
      (expand-file-name "~")))

(defsubst sidebar-cons-git-buffer-name ()
  "Construct the git buffer name from 'SIDEBAR' and the frame name.
See `\\[sidebar-cons-buffer-name]' for more info."
  (concat " *SIDEBAR-" (or (frame-parameter nil 'window-id)
			   (frame-parameter nil 'name))"-GIT*"))
;;;(concat "*" SIDEBAR "-" (frame-parameter nil 'name) "-GIT*"))

(defsubst sidebar-cons-curl-buffer-name ()
  "Construct the curl buffer name from 'SIDEBAR' and the frame name.
See `\\[sidebar-cons-buffer-name]' for more info."
  (concat " *SIDEBAR-" (or (frame-parameter nil 'window-id)
			   (frame-parameter nil 'name))"-CURL*"))

(defsubst sidebar-get-curl-buffer ()
  "Return the buffer associated to the curl buffer."
  (get-buffer-create (sidebar-cons-curl-buffer-name)))

(defsubst sidebar-get-git-buffer ()
  "Return the existing/created sidebar git buffer for the current frame."
  (get-buffer-create (sidebar-cons-git-buffer-name)))

(defsubst sidebar-exists? ()
  "Check if a sidebar for the frame exists."
  (get-buffer (sidebar-cons-buffer-name)))

(defsubst sidebar-insert (str &optional face)
  "Small function to insert STR with FACE if non-nil."
  (if face
      (insert (propertize str 'face face))
    (insert str)))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-count-char (char s)
  "Count the occurence of CHAR in string S."
  (let ((n 0)
	(c (elt char 0)))
    (--dotimes (length s)
      (when (eq (elt s it) c)
	(setq n (1+ n))))
    n))

(defun sidebar-calc-depth (file status)
  "Calcul the depth of FILE from the current directory point of view.
This is uses to count the number of space to insert before the filename.
STATUS is the status of the FILE."
  (let* ((path-from-current (s-chop-prefix (sidebar-get current-path) (--getpath file)))
	 (depth (sidebar-count-char "/" path-from-current))) ; TODO: Support Windows (replace '/')
    (when (> depth 0)
      (setq depth (* depth 2)))
    (setq depth (1+ depth))
    depth))

(defun sidebar-child-of-status? (file-path status)
  "Return non-nil if FILE-PATH is a child of a STATUS directory."
  (when (sidebar-get git-hashtable)
    (catch 'stop-map
      (maphash (lambda (key value)
		 (when (equal value status)
		   (when (and (s-starts-with? key file-path) (not (string= key file-path)))
		     (throw 'stop-map t))))
	       (sidebar-get git-hashtable)))))

(defun sidebar-color-from-status (status &optional default)
  "STATUS DEFAULT."
  (pcase status
    ('not-updated 'sidebar-not-updated-face)
    ('updated 'sidebar-updated-face)
    ('changed 'sidebar-changed-face)
    ('added 'sidebar-added-face)
    ('renamed 'sidebar-renamed-face)
    ('match 'sidebar-match-face)
    (_ default)))

(defun sidebar-get-color (file path status &optional icon no-color)
  "Return the face to use for FILE.
PATH is the path of the file relative to the project root directory
STATUS is the status from git
non-nil ICON means we're getting the color for an icon.
NO-COLOR non-nil means don't use a color."
  (cond ((or (equal 'ignored status) (sidebar-child-of-status? path 'ignored))
	 (if (--dir? file) 'sidebar-ignored-dir-face 'sidebar-ignored-file-face))
	((and (or (equal 'untracked status) (sidebar-child-of-status? path 'untracked)) (not icon))
	 'sidebar-untracked-face)
	((--dir? file) 'sidebar-dir-face)
	(no-color 'sidebar-file-face)
	((not icon) (sidebar-color-from-status status 'sidebar-file-face))))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-gui-insert-status-subfiles (status number file path)
  "Insert the icon and numbers after a directory.
STATUS is use to know which icon to insert
NUMBER is the number to insert
FILE PATH"
  (sidebar-gui-insert-status file path status t)
  (sidebar-insert " ")
  (sidebar-insert (number-to-string number)))

(defun sidebar-status-subfiles (path)
  "PATH."
  (let ((not-updated 0) (updated 0) (untracked 0) (changed 0) (added 0) (renamed 0) (match 0))
    (-when-let (hashtable (sidebar-get git-hashtable))
      (maphash (lambda (key value)
		 (when (s-starts-with? path key)
		   (cond ((equal 'not-updated value) (setq not-updated (1+ not-updated)))
			 ((equal 'updated value) (setq updated (1+ updated)))
			 ((equal 'changed value) (setq changed (1+ changed)))
			 ((equal 'added value) (setq added (1+ added)))
			 ((equal 'match value) (setq match (1+ match)))
			 (t nil))))
	       hashtable))
    `((not-updated . ,not-updated) (updated . ,updated) (untracked . ,untracked)
      (changed . ,changed) (added . ,added) (renamed . ,renamed) (match . ,match))))

(defun sidebar-insert-status-subfiles? (file)
  "FILE."
  (when (--dir? file)
    (cond ((equal sidebar-status-on-directory 'always) t)
	  ((and (equal sidebar-status-on-directory 'on-closed) (not (--opened? file))) t)
	  ((and (equal sidebar-status-on-directory 'on-opened) (--opened? file)) t))))

(defun sidebar-insert-status-subfiles (file path)
  "FILE PATH."
  (when (sidebar-insert-status-subfiles? file)
    (-let (((&alist 'not-updated not-updated
		    'updated updated
		    'changed changed
		    'added added
		    'match match)
	    (sidebar-status-subfiles path))
	   (func 'sidebar-gui-insert-status-subfiles))
      (when (> not-updated 0)
	(funcall func 'not-updated not-updated file path))
      (when (> updated 0)
	(funcall func 'updated updated file path))
      (when (> changed 0)
	(funcall func 'changed changed file path))
      (when (> added 0)
	(funcall func 'added added file path))
      (when (> match 0)
	(funcall func 'match match file path)))))

(defsubst sidebar-insert-filename (str face)
  "Small function to insert STR with FACE if non-nil."
  (insert (propertize str 'face face 'mouse-face face 'keymap sidebar-button-keymap)))

(defun sidebar-insert-icon (icon face)
  "Insert ICON with FACE if non-nil.
LINE."
  (if face
      (insert (icons-in-terminal icon :face face :height 1.15))
    (insert (icons-in-terminal icon :height 1.15))))

(defun sidebar-insert-fileicon (file filename status path face)
  "FILE FILENAME STATUS PATH FACE."
  (if (--dir? file)
      (let ((icon (if (--opened? file) sidebar-icon-dir-opened sidebar-icon-dir-closed)))
	(sidebar-insert-icon icon (sidebar-get-color file path status t)))
    (-let* (((&plist :icon icon :color color) (sidebar-filemapping-lookup filename))
	    (partial (-partial 'icons-in-terminal icon :height 1.1)))
      (cond (face (insert (funcall partial :face face)))
	    (color (insert (funcall partial :foreground color)))
	    (t (insert (funcall partial)))))))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defsubst sidebar-window-width ()
  "Return the sidebar's window width."
  (window-width (sidebar-get-window)))

(defun sidebar-insert-powerline (icons-on-line)
  "Insert the remaining spaces and a '' to make a powerline effect.
ICONS-ON-LINE."
  (let* ((space-to-add (-> (sidebar-window-width) (- (+ (current-column) 2))))
	 (space-to-add (-> space-to-add (- (cond ((sidebar-gui?) (+ icons-on-line
								    (cadr sidebar-icon-powerline)))
	 					 (t 1)))))
	 (icon sidebar-icon-powerline)
	 (face 'sidebar-powerline-face))
    (insert (propertize (s-repeat space-to-add " ") 'face face))
    (insert (icons-in-terminal (car icon)
			       :raise (car (cddr icon))
			       :height (cadr (cddr icon))
			       :foreground (face-background face)))
    (unless (or (sidebar-gui?)
		(= (cadr icon) 0))
      (insert " "))))

(defun sidebar-gui-insert-icon-filename (file filename status path)
  "FILE FILENAME STATUS PATH."
  (funcall sidebar-insert-fileicon-function file filename status path (sidebar-get-color file path status t))
  (sidebar-insert " ")
  (sidebar-insert-filename filename (sidebar-get-color file path status nil (not sidebar-filename-colored))))

(defun sidebar-gui-insert-status (file path status &optional dir)
  "FILE PATH STATUS DIR."
  (when (or sidebar-status-on-file dir)
    (let* ((face (sidebar-color-from-status status nil))
	   (func (lambda (name)
		   (sidebar-insert " ")
		   (funcall 'sidebar-insert-icon name face))))
      (pcase status
	('not-updated (funcall func sidebar-icon-git-not-updated))
	('updated (funcall func sidebar-icon-git-updated))
	('changed (funcall func sidebar-icon-git-changed))
	('added (funcall func sidebar-icon-git-added))
	('renamed (funcall func sidebar-icon-git-renamed))
	('match (funcall func sidebar-icon-git-match))))))

(defun sidebar-print-file (file &optional line)
  "Insert FILE on the current LINE.
The function inserts the filename without parents directories.

First, it inserts ' ' x times, depending on the file depth (relative to
 the current directory).
If the file has a git status and is not a directory, it inserts the icon
 associated to the status.
Then it inserts the filename.
If FILE is a directory and closed (not expanded), it inserts the icons of
 all the files it contains just after its name, still on the same line.

FILE is a associated list created from `\\[sidebar-file-struct]'."
  (let* ((filename (-some-> file --getpath file-name-nondirectory))
	 (path-in-project (s-chop-prefix (sidebar-get root-project) (--getpath file)))
	 (path-fixed-dirname (if (--dir? file) (file-name-as-directory path-in-project) path-in-project))
	 (git-hashtable (sidebar-get git-hashtable))
	 (status (-some->> git-hashtable (gethash path-fixed-dirname)))
	 (depth (sidebar-calc-depth file status))
	 (line-number (or line (line-number-at-pos))))
    (sidebar-insert (s-repeat depth " "))
    (sidebar-gui-insert-icon-filename file filename status path-fixed-dirname)
    (sidebar-gui-insert-status file path-fixed-dirname status)
    (sidebar-insert-status-subfiles file path-fixed-dirname)))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-print-listfiles (list)
  "Insert all files in LIST from the current line.
It updates the associated value 'line for each file.  This is used to
keep track of which file is on which line."
  (let ((line-number (line-number-at-pos)))
    (sidebar-writable
     (--each list
       (setf (--getline it) line-number)
       (funcall (sidebar-get print-item) it line-number)
       (insert "\n")
       ;; (newline)
       (setq line-number (1+ line-number))))))

(defun sidebar-sort-files-by-line ()
  "Sort `sidebar-files' by the associated value 'line."
  (sidebar-set files (--sort (< (--getline it) (--getline other))
			     (sidebar-get files))))

(defun sidebar-print ()
  "Prints Sidebar."
  (sidebar-print-listfiles (sidebar-get files))
  (sidebar-sort-files-by-line))

(defun sidebar-dots-file (file)
  "Return t if FILE is '.' or '..'."
  (let ((file (file-name-nondirectory file)))
    (or (string= "." file) (string= ".." file))))

;;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(sidebar-content-provider files (path)
  "Return a list of files/directories in PATH.
It removes '.' and '..'
Sort the directories first
The list returned is a list of association list for each file created
with `\\[sidebar-file-struct]'"
  (let* ((files-and-dirs (--> path (directory-files it t) (-remove 'sidebar-dots-file it)))
	 (dirs-sorted (--sort (string< it other) (-filter 'file-directory-p files-and-dirs)))
	 (files-sorted (--sort (string< it other) (--filter (-> it file-directory-p not) files-and-dirs))))
    (-concat dirs-sorted files-sorted)))

(defsubst sidebar-load-content (path)
  "PATH."
  (funcall (sidebar-get load-content-function) path))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-goto-buffername (buffer-name)
  "This function jump the cursor to BUFFER-NAME (string) in the sidebar.
If there is no filename equal to the BUFFER-NAME, it put the cursor
on the frame parameter `sidebar-line-to-start'.
This is use when the sidebar is created."
  (let* ((file (when buffer-name (--first (string= (--getpath it) buffer-name) (sidebar-get files))))
	 (line (or (sidebar-get save-line) (--getline file) (sidebar-get line-to-start))))
    (sidebar-goto-line line)
    (sidebar-show-current)
    (when (and (sidebar-get restore-function)
	       (sidebar-get window-start))
      (set-window-start (sidebar-get-window) (sidebar-get window-start)))
    (sidebar-set save-line nil)))

(defun sidebar-expand-path (project-path-root file-path)
  "PROJECT-PATH-ROOT FILE-PATH."
  (-when-let* ((base project-path-root)
	       (dirs-to-open (-some->> file-path
				       (s-chop-prefix (-> base directory-file-name))
				       (s-chop-suffix (-> file-path file-name-nondirectory))
				       directory-file-name
				       split-path)))
    (-map 'sidebar-file-struct
	  (--map (setq base (concat (file-name-as-directory base) it))
		 dirs-to-open))))

;; (file-name-nondirectory "/home/seb/seb/")
;; (directory-file-name "/home/seb/seb/")

;; (sidebar-expand-path "/home/sebastien/" "/home/sebastien/travaux/machin/truc/file.c")
;; (sidebar-expand-path "/home/sebastien/" "/home/sebastien/file.c")

;; (split-path "travaux/machin")

(defun split-path (path)
  "PATH."
  (split-path-helper path ()))

(defun split-path-helper (path accum)
  "PATH ACCUM."
  (let ((dir (-> path file-name-directory directory-file-name))
	(name (file-name-nondirectory path)))
    (if (equal dir path)
	accum
      (split-path-helper dir (cons name accum)))))

(defun sidebar-check-setup ()
  "Check if the font icons-in-terminal is installed.
TODO: Check with terminals too (now it checks only with GUI), `font-info'
returns an error on terminals."
  (when (and (sidebar-gui?) (not (font-info "icons-in-terminal")))
    (ignore-errors (kill-buffer (sidebar-cons-buffer-name)))
    (error "The font icons-in-terminal is not installed: see https://github.com/sebastiencs/sidebar.el")))

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
     (sidebar-print-item                   . sidebar-mu4e-print-item)
     (sidebar-line-to-start                . 2))
    :sidebar-buffers-mode
    ((sidebar-load-content-function        . sidebar-content-buffers)
     (sidebar-mode-to-use                  . sidebar-buffers-mode)
     (sidebar-make-header-function         . sidebar-buffers-make-header)
     (sidebar-make-modeline-left-function  . sidebar-buffers-make-modeline-left)
     (sidebar-make-modeline-right-function . sidebar-buffers-make-modeline-right)
     (sidebar-item-builder-function        . sidebar-buffers-item-builder)
     (sidebar-restore-function             . nil)
     (sidebar-print-item                   . sidebar-buffers-print-item)
     (sidebar-line-to-start                . 4))
    ))

(defun sidebar-init-vars (project-path-root)
  "PROJECT-PATH-ROOT."
  (let ((mode (cond ((sidebar-mu4e?)    :sidebar-mu4e-mode)
		    ((sidebar-buffers?) :sidebar-buffers-mode)
		    (t                  :sidebar-mode))))
    (--each (plist-get sidebar-mode-association mode)
      (sidebar-set1 (car it) (cdr it))))
  (if (and (sidebar-get restore-function) (sidebar-get saved-state-files))
      (funcall (sidebar-get restore-function))
    (sidebar-set root-project (sidebar-get-root-project))
    (sidebar-set history (list project-path-root))
    (sidebar-set current-path project-path-root)
    (sidebar-set default-width sidebar-width)
    (sidebar-set files (sidebar-load-content project-path-root))
    (unless (sidebar-get overlay)
      (sidebar-set overlay (ov (point-min) (point-min) 'face 'sidebar-powerline-face)))))

(defun sidebar-open ()
  "Open or create a sidebar for the current frame."
  (interactive)
  (sidebar-set window-origin (get-buffer-window))
  (when (or (sidebar-get mu4e-force) (sidebar-mu4e?))
    (sidebar-kill))
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
      (sidebar-check-setup)
      (sidebar-init-vars project-path-root)
      (funcall (sidebar-get mode-to-use))
      (sidebar-refresh (sidebar-expand-path project-path-root buffer-name-current))
      (sidebar-goto-buffername buffer-name-current)
      (sidebar-curl-run)
      (unless (sidebar-get saved-state-files)
	(sidebar-git-run)))))

(defun sidebar-close ()
  "Close the sidebar for the current frame, you still can reopen it."
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

(defun sidebar-count-icons (string)
  "Count the numbers of icons in STRING.
The icons are known to be characters between 0xe000 and 0xf8ff."
  (let ((n 0))
    (--dotimes (length string)
      (let ((c (elt string it)))
	(when (and (>= c #xe000) (<= c #xf8ff))
	  (setq n (1+ n)))))
    n))

(defun sidebar-make-powerline (window-width n-icons n-characters)
  "WINDOW-WIDTH N-ICONS N-CHARACTERS."
  (let* ((space-to-add (- window-width (1+ n-characters)
			  (if (sidebar-gui?) (+ n-icons (cadr sidebar-icon-powerline)) 1)))
	 (icon sidebar-icon-powerline)
	 (face 'sidebar-powerline-face))
    (concat
     (propertize (s-repeat space-to-add " ") 'face face)
     (icons-in-terminal (car icon)
			:raise (car (cddr icon))
			:height (cadr (cddr icon))
			:foreground (face-background face))
     (unless (or (sidebar-gui?) (= (cadr icon) 0)) " "))))

(defun sidebar-move-overlay (beg end window)
  "BEG END WINDOW."
  (let* ((ov (sidebar-get overlay))
	 (window-width (window-width window))
	 (n-icons (sidebar-count-icons (buffer-substring beg end)))
	 (n-characters (- end beg))
	 (powerline (sidebar-make-powerline window-width n-icons n-characters)))
    (ov-move ov beg end)
    (ov-set ov 'after-string powerline)))

(defun sidebar-update-path-on-header ()
  "."
  (-when-let* ((path-file (-some-> (--getpath (sidebar-find-file-from-line)) file-name-directory))
	       (current-path (sidebar-get current-path))
	       (suffix-path (substring path-file (1- (length current-path)))))
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
      (sidebar-update-path-on-header)
      (when sidebar-adjust-auto-window-width
	(sidebar-adjust-window-width (- line-end line-begin) sidebar-window))
      (-when-let* ((_ sidebar-message-current)
		   (path (--getpath (sidebar-find-file-from-line))))
	(message (abbreviate-file-name path))))))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-create-directory ()
  "Create a directory and its parents if non existing."
  (interactive)
  (let* ((new-dir (-some->> (sidebar-find-file-from-line)
			    --getpath
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
			     --getpath
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
	 (path (--getpath file-at-line))
	 (str-prompt (format "[sidebar] Delete the %s %s ? ('yes' or anything else to cancel): "
			     (if (--dir? file-at-line) "directory" "file")
			     path))
	 (confirm (read-string str-prompt)))
    (when (s-equals? "yes" confirm)
      (if (--dir? file-at-line) (delete-directory path t) (delete-file path))
      (sidebar-refresh))))

(defun sidebar-rename-buffer-name (buffers name new-name)
  "BUFFERS NAME NEW-NAME."
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
	 (path (--getpath file-at-line))
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
    (message "%s to copy: '%s'" (if (--dir? file-at-line) "Directory" "File")
	     (file-name-nondirectory (--getpath file-at-line)))))

(defun sidebar-cut-selected ()
  "Cut the file/directory on the current line.
to paste it, use `sidebar-paste'."
  (interactive)
  (let ((file-at-line (sidebar-find-file-from-line)))
    (if (not (file-writable-p (--getpath file-at-line)))
	(error "[sidebar] Cannot cut file, it's non writable")
      (plist-put sidebar-file-to-copy :file file-at-line)
      (plist-put sidebar-file-to-copy :method 'cut)
      (message "%s to cut: '%s'" (if (--dir? file-at-line) "Directory" "File")
	       (file-name-nondirectory (--getpath file-at-line))))))

(defun sidebar-paste ()
  "Paste the file/directory previously copied/cut.
The file will be paste to the path of the file on the current line.
If the file on the current line is a directory, it pastes the file outside it.
To paste the file inside the directory, it has to be open (expand).

If the file is cut, you'll be ask to rename the buffers visiting it."
  (interactive)
  (-let* ((file-at-line (sidebar-find-file-from-line))
	  (directory (if (--opened? file-at-line) (file-name-as-directory (--getpath file-at-line))
		       (file-name-directory (--getpath file-at-line))))
	  ((&plist :method method) sidebar-file-to-copy)
	  ((&plist :file file) sidebar-file-to-copy)
	  (path (--getpath file))
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
		(if (--dir? file)
		    (copy-directory path directory t t nil)
		  (copy-file path directory nil t t t))
		(when (equal method 'cut)
		  (if (--dir? file)
		      (delete-directory path t)
		    (delete-file path)
		    (sidebar-rename-buffer-name (buffer-list) path new-file)))
		(sidebar-refresh))
	    (error "Error while copying file: " (error-message-string err))))))))

(defun sidebar-update-to-opened (list path-old)
  "Return LIST with PATH-OLD's 'opened value to t."
  (let ((file (--first (string= (--getpath it) path-old) list)))
    (setf (--opened? file) t))
  list)

(defun sidebar-init-buffer ()
  "."
  (set-window-margins (sidebar-get-window t) 0 0)
  (delete-overlay (sidebar-get overlay))
  (sidebar-writable
   (erase-buffer)
   (insert "\n")))

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
	 (old-files (sidebar-get files))
	 (history (sidebar-get history))
	 (old-dir (directory-file-name (sidebar-get current-path))))
    (if (string= old-dir new-directory)
	(message "Sidebar: You're at the top")
      (sidebar-init-buffer)
      (sidebar-set history (add-to-list 'history new-directory nil 's-equals?))
      (sidebar-set files (sidebar-update-to-opened new-files old-dir))
      (sidebar-set current-path (file-name-as-directory new-directory))
      (setq default-directory (sidebar-get current-path))
      (sidebar-set root-project (sidebar-get-root-project))
      (sidebar-print-listfiles (sidebar-get files))
      (let* ((old-dir- (--first (string= (--getpath it) old-dir) (sidebar-get files)))
	     (line-to-put-old-files (--getline old-dir-)))
	(sidebar-goto-line (+ line-to-put-old-files 1) t)
	(sidebar-update-line-number (length old-files) line-to-put-old-files)
	(sidebar-print-listfiles old-files)
	(sidebar-set files (-concat (sidebar-get files) old-files))
	(sidebar-sort-files-by-line)
	(sidebar-goto-line line-to-put-old-files)))
    (sidebar-git-run)))

(defun sidebar-open-directory (file)
  "Set the current directory to FILE.

If FILE is opened (expanded), we filter the list of files to get only the ones
in the new directory.
If FILE it not opened, we load the dir with `\\[sidebar-load-content]'
."
  (let ((files nil)
	(history (sidebar-get history)))
    (if (--opened? file)
	(let ((dirname (file-name-as-directory (--getpath file))))
	  (setq files (--filter (s-starts-with? dirname (--getpath it)) (sidebar-get files))))
      (setq files (sidebar-load-content (--getpath file))))
    (delete-overlay (sidebar-get overlay))
    (sidebar-set files files)
    (sidebar-set current-path (file-name-as-directory (--getpath file)))
    (setq default-directory (sidebar-get current-path))
    (sidebar-set history (add-to-list 'history default-directory nil 's-equals?))
    (sidebar-set root-project (sidebar-get-root-project))
    (sidebar-goto-line 1))
  (sidebar-git-run))

(defsubst sidebar-open-file-in-window (window buffer-file)
  "WINDOW BUFFER-FILE."
  (set-window-buffer window buffer-file))

(defun sidebar-list-windows-others-frame (frames)
  "FRAMES."
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
    (if (--dir? file)
	(sidebar-open-directory file)
      (sidebar-select-make-buffer (list windows-in-frame windows-in-others-frame)
				  " Select a window "
				  " Others frames "
				  (lambda (x) (s-chop-suffix ">" (s-replace "#<window " "#" (format "%s" x))))
				  sidebar-select-icon-before-window
				  'sidebar-open-file-in-window
				  (find-file-noselect (--getpath file))))))

(defun sidebar-find-file-from-line (&optional line)
  "Return the file on the LINE.
Because sidebar-files is always sorted, it's easy to get it"
  (if line
      (nth (- line 2) (sidebar-get files))
    (nth (- (line-number-at-pos) 2) (sidebar-get files))))

(defun sidebar-open-file (file)
  "Open FILE in the buffer where `\\[sidebar-open]' has been called.
If the window doesn't exists anymore, the function calls `sidebar-open-in-window'."
  (let ((buffer-file (find-file-noselect (--getpath file)))
	(window (sidebar-get window-origin)))
    (if (window-live-p window)
	(set-window-buffer window buffer-file)
      (sidebar-set window-origin (sidebar-open-in-window)))))

(defun sidebar-open-line ()
  "Open file or directory of the current line.
If it's a directory, open it in the sidebar.
If it's a file, open it on the window where `\\[sidebar-open]' has been called"
  (interactive)
  (-when-let (file (sidebar-find-file-from-line))
    (if (--dir? file)
	(sidebar-open-directory file)
      (sidebar-open-file file))))

(defun sidebar-update-line-number (num line)
  "Add NUM to every file where 'line is > than LINE."
  (let ((list (sidebar-get files)))
    (--each list
      (let ((line-for-this-file (--getline it)))
	(when (> line-for-this-file line)
	  (setf (--getline it) (+ line-for-this-file num)))))))

(defun sidebar-search-closed-dir (file)
  "Search FILE in the list of previously closed directory.
If found, it is extracted and remove from the list
Return the found element."
  (-when-let (found (--filter (string= (car it) (-> file --getpath file-name-as-directory))
			      (sidebar-get closed-directories)))
    (sidebar-set closed-directories
		 (--remove (string= (car it) (-> file --getpath file-name-as-directory))
			   (sidebar-get closed-directories)))
    (cdar found)))

;;(kill-buffer (sidebar-cons-buffer-name))

(defun sidebar-expand-dir (file line)
  "Expand the directory FILE on the LINE.
If the directory has already been expanded, it get the list of files
from the saved list `\\[sidebar-closed-directories]'.
Otherwise it load the dir with `\\[sidebar-load-content]'."
  (setf (--opened? file) t)
  (save-excursion
    (let ((new-files (or (sidebar-search-closed-dir file)
			 (sidebar-load-content (--getpath file)))))
      (forward-line)
      (sidebar-update-line-number (length new-files) line)
      (sidebar-print-listfiles new-files)
      (sidebar-set files (-concat (sidebar-get files) new-files))
      (sidebar-sort-files-by-line)))
  (save-excursion
    (beginning-of-line)
    (sidebar-writable
     (delete-region (line-beginning-position) (line-end-position))
     (sidebar-print-file file))
    (sidebar-show-current)))

(defun sidebar-delete-line ()
  "Delete the whole line (including \n)."
  (sidebar-writable
   (delete-region (line-beginning-position) (line-end-position))
   (delete-char 1)))

(defun sidebar-update-closed-dirs (dir list)
  "Insert DIR at the begining of LIST."
  (sidebar-set closed-directories (-insert-at 0 (-concat (list dir) list) (sidebar-get closed-directories))))

(defun sidebar-close-dir (file line)
  "Close the opened (expanded) directory FILE on LINE.
All the files in the closed dir are saved in the
list `\\[sidebar-closed-directories]' to reuse them later if
the directory is re-opened"
  (setf (--opened? file) nil)
  (save-excursion
    (let* ((dir-to-close (--getpath (nth (- (line-number-at-pos) 2) (sidebar-get files))))
	   (dir-to-close (file-name-as-directory dir-to-close))
	   (files-to-remove (--filter (s-starts-with? dir-to-close (--getpath it)) (sidebar-get files)))
	   (new-sidebar-files (--remove (s-starts-with? dir-to-close (--getpath it)) (sidebar-get files))))
      (sidebar-update-closed-dirs dir-to-close files-to-remove)
      (sidebar-set files new-sidebar-files)
      (forward-line)
      (sidebar-update-line-number (- (length files-to-remove)) line)
      (--dotimes (length files-to-remove)
	(sidebar-delete-line))))
  (save-excursion
    (beginning-of-line)
    (sidebar-writable
     (delete-region (line-beginning-position) (line-end-position))
     (sidebar-print-file file))
    (sidebar-show-current)))

(defun sidebar-expand-or-close-dir ()
  "Expand or close the directory on the current line."
  (interactive)
  (let ((line (line-number-at-pos))
	(file (sidebar-find-file-from-line)))
    (when (--dir? file)
      (if (--opened? file)
	  (sidebar-close-dir file line)
	(sidebar-expand-dir file line))))
  (sidebar-update-files-number))

(defun sidebar-goto-line (line &optional force)
  "Go to LINE.
The function checks to not go at the last line (there is no
filename on this line)
if FORCE is non-nil, there is no check."
  (when (integerp line)
    (if force
	(forward-line (- line (line-number-at-pos)))
      (let ((max (or (--getline (car (last (sidebar-get files)))) 2)))
	(if (> line max) (setq line max)
	  (when (< line 2) (setq line 2)))
	(forward-line (- line (line-number-at-pos)))))))

(defun sidebar-update-from-opened-dirs (list opened)
  "Set the associated value `opened' to t for all files of LIST present in the list OPENED."
  (--each opened
    (-when-let* ((path (--getpath it))
		 (found (--first (string= (--getpath it) path) list)))
      (setf (--opened? found) t)))
  list)

(defun sidebar-update-files-number ()
  "."
  (let ((total (length (sidebar-get files)))
	(ndirs (setq sidebar-files-number (--reduce-from (+ acc (if (--dir? it) 1 0)) 0 (sidebar-get files)))))
    (setq sidebar-files-number (- total ndirs)
	  sidebar-directories-number ndirs)))

(defun sidebar-refresh (&optional to-expand)
  "Update the list of files in the Sidebar TO-EXPAND.

The function saves all the directories opened (expanded) in the current sidebar.
Then it load the files of the current directory with `\\[sidebar-load-content]'
Print them.
For each directory in the list previously saved, it reload the dir
with `\\[sidebar-load-content]' and print them on the sidebar at the right place."
  (interactive)
  (with-current-buffer (sidebar-get-buffer)
    (let ((opened-dirs (or to-expand (--filter (--opened? it) (sidebar-get files))))
	  (current-line (line-number-at-pos)))
      (sidebar-set files (sidebar-update-from-opened-dirs (sidebar-load-content (sidebar-get current-path)) opened-dirs))
      (sidebar-init-buffer)
      (sidebar-print-listfiles (sidebar-get files))
      (sidebar-sort-files-by-line)
      (--each opened-dirs
	(-when-let* ((path (--getpath it))
		     (found (--first (string= (--getpath it) path) (sidebar-get files))))
	  (setf (--opened? found) t)
	  (sidebar-goto-line (+ (--getline found) 1) t)
	  (let ((new-files (-> (sidebar-load-content (--getpath found))
			       (sidebar-update-from-opened-dirs opened-dirs))))
	    (sidebar-print-listfiles new-files)
	    (sidebar-update-line-number (length new-files) (--getline found))
	    (sidebar-set files (-concat (sidebar-get files) new-files))
	    (sidebar-sort-files-by-line))))
      (sidebar-goto-line current-line)
      (sidebar-show-current))
    (sidebar-update-files-number)
    (message "Sidebar refreshed")))

(defun sidebar-refresh-on-save-after-timer ()
  "Function called when a buffer is saved, it refreshes the sidebar."
  (-when-let (sidebar-window (sidebar-get-window t))
    (with-current-buffer (sidebar-get-buffer)
      (sidebar-git-run t))))

(defun sidebar-refresh-on-save ()
  "Function called when a buffer is saved, it refreshes the sidebar.
I'm using a timer because, with my config, flycheck write a temporary
file in the current directory (I don't know why) and it appears in the sidebar.
So I'm just waiting for it to be delete :/"
  (run-with-idle-timer 2 nil 'sidebar-refresh-on-save-after-timer))

(defun sidebar-delete-buffer-on-kill (frame)
  "When the FRAME is deleted, this function kill the Sidebar buffer associated to it."
  (ignore-errors (kill-buffer (sidebar-cons-buffer-name))))

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
      (sidebar-set git-branches str))))

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

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-git-sentinel (process change)
  "Sentinel for the PROCESS running git.  Handle exit.
Once the output is parsed, it refreshes the sidebar.
CHANGE is unused"
  (when (eq (process-status process) 'exit)
    (let ((start (window-start (sidebar-get-window))))
      (if (/= (process-exit-status process) 0)
	  (sidebar-refresh)
	(let ((table (sidebar-git-parse-buffer))
	      (sidebar-window (sidebar-get-window t)))
	  (with-current-buffer (sidebar-get-buffer)
	    (sidebar-set git-hashtable table)
	    (sidebar-refresh))))
      (set-window-start (sidebar-get-window) start)
      (ignore-errors (kill-buffer (sidebar-get-git-buffer))))))

(defun sidebar-git-run (&optional force)
  "Run git status in the current directory.
The output is parsed to print information of each file in the sidebar.
The process is run only once per project.
Once done, it refresh the sidebar.
if FORCE is non-nil, force to run the process."
  (with-current-buffer (sidebar-get-buffer)
    (sidebar-set saved-line-number (line-number-at-pos)))
  (if (or force
	  (and (sidebar-get root-project)
	       (not (s-equals? (sidebar-get root-project) (sidebar-get git-dir)))))
      (progn
	(sidebar-set git-dir (sidebar-get root-project))
	(let ((process (get-buffer-process (sidebar-get-git-buffer))))
	  (when (and process (process-live-p process))
	    (kill-process process)))
	(with-current-buffer (sidebar-get-git-buffer)
	  (erase-buffer)
	  (let ((process (start-process "sidebar-git" (sidebar-get-git-buffer)
					"git" "status" "--porcelain" "--ignored" "-z" "-b" ".")))
	    (set-process-query-on-exit-flag process nil)
	    (set-process-sentinel process 'sidebar-git-sentinel))))
    (sidebar-refresh)))

(defun sidebar-refresh-cmd ()
  "Refresh the sidebar content.
See `sidebar-git-run' and `sidebar-refresh'"
  (interactive)
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
			    :face 'sidebar-icon-header-project-face
			    :background (face-background 'sidebar-header-line-face)
			    :raise -0.07
			    :height 1.3)
       (icons-in-terminal sidebar-icon-header-directory
			  :face 'sidebar-icon-header-directory-face
			  :background (face-background 'sidebar-header-line-face)
			  :raise -0.0
			  :height 1.3))
;;;     (when (not (sidebar-gui?)) " ")
     " "
     (propertize project-name 'display '(raise 0.12))
     (when (and suffix (> (length suffix) 1))
       (propertize (if project suffix (substring suffix 1))
		   'face 'sidebar-suffix-path-header-face
		   'display '(raise 0.12)
		   'font-lock-ignore t)))))

(defun sidebar-set-header ()
  "Format the header with the string from `sidebar-make-header-function'."
  (let* ((string (funcall (sidebar-get make-header-function)))
	 (length 0))
    (add-face-text-property 0 (length string) 'sidebar-header-line-face t string)
    (setq length (- (sidebar-window-width) (1+ (length string))))
    (when (sidebar-gui?)
      (setq length (- length (cadr sidebar-icon-header-end))))
    (concat
     string
     (propertize (concat (s-repeat length " "))
		 'face 'sidebar-header-line-face 'display '(raise 0.12))
     (icons-in-terminal (car sidebar-icon-header-end)
			:foreground (face-background 'sidebar-header-line-face)
			:height sidebar-header-line-height))))

(defun sidebar-make-modeline-left ()
  "."
  (if (and (sidebar-get root-project) (sidebar-get git-branches))
      (concat
       " "
       (icons-in-terminal sidebar-icon-branch
			  :face 'sidebar-icon-branch-face
			  :background (face-background 'sidebar-branch-face)
			  :raise -0.1
			  :height 1.3)
       (when (not (sidebar-gui?)) " ")
       (propertize (car (sidebar-get git-branches))
		   'face `(:inherit sidebar-branch-face :background ,(face-background 'sidebar-branch-face))
		   'display '(raise 0.1)))
    (concat " "
	    (number-to-string sidebar-files-number)
	    (if (> sidebar-files-number 1) " files" " file"))))

(defun sidebar-make-modeline-right ()
  "."
  (if (and (sidebar-get root-project) (sidebar-get git-branches))
      (let ((str-branch-distant (s-split " \\[\\|\\]" (cadr (sidebar-get git-branches)))))
	(concat
	 (icons-in-terminal sidebar-icon-remotebranch
			    :face 'sidebar-icon-remotebranch-face
			    :background (face-background 'sidebar-remotebranch-face)
			    :raise -0.1
			    :height 1.3)
	 (when (not (sidebar-gui?)) " ")
	 (propertize (car str-branch-distant) 'display '(raise 0.1))
	 "  "))
    (concat (number-to-string sidebar-directories-number)
	    (if (> sidebar-directories-number 1) " directories" " directory"))
    ))

(defun sidebar-set-modeline ()
  "."
  (when (not (sidebar-get select-active))
    (let ((left (concat (funcall (sidebar-get make-modeline-left-function)) " "))
	  (right (concat " " (funcall (sidebar-get make-modeline-right-function))))
	  (sidebar-width (sidebar-window-width))
	  (space-to-add 0))
      (when (> (length left) 1)
	(add-face-text-property 0 (length left) 'sidebar-branch-face nil left)
	(setq left (concat left (icons-in-terminal (car sidebar-icons-branches-modeline)
						   :foreground (face-background 'sidebar-branch-face)
						   :raise -0.1
						   :height sidebar-mode-line-height))))
      (when (> (length right) 1)
	(add-face-text-property 0 (length right) 'sidebar-remotebranch-face nil right)
	(setq right (concat (icons-in-terminal (cadr sidebar-icons-branches-modeline)
					       :foreground (face-background 'sidebar-remotebranch-face)
					       :raise -0.1
					       :height sidebar-mode-line-height)
			    right)))
      (setq space-to-add (- sidebar-width (+ (length left) (length right))))
      (when (sidebar-gui?)
	(setq space-to-add (- (+ (- space-to-add 3)
				 (* sidebar-mode-line-height 2))
			      (car (cddr sidebar-icons-branches-modeline)))))
      (concat left
	      (s-repeat space-to-add " ")
	      right
	      (propertize "    " 'face 'sidebar-remotebranch-face)))))

(defun sidebar-post-command()
  ;; (message "last command: %s" this-command)
  (-let [line (or (sidebar-get saved-line-number) (line-number-at-pos))]
    (sidebar-goto-line line))
  (sidebar-set saved-line-number nil)
  (sidebar-show-current))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

;;  (message (concat "line changed to: " (number-to-string (line-number-at-pos))))))))

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

(defun sidebar-history-open (dir)
  "DIR."
  (select-window (sidebar-get-window))
  (sidebar-open-directory (sidebar-file-struct dir)))

(defun sidebar-history ()
  "Show the last opened directories with the ability to open one of them."
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

(defun sidebar-save-state ()
  "."
  (sidebar-set window-start (window-start (sidebar-get-window)))
  (sidebar-set save-line-files (line-number-at-pos))
  (sidebar-set save-files (sidebar-get files))
  (sidebar-set save-default-width (sidebar-get default-width))
  (sidebar-set save-root-project (sidebar-get root-project))
  (sidebar-set save-history (sidebar-get history))
  (sidebar-set save-current-path (sidebar-get current-path)))

(defun sidebar-restore-state ()
  "."
  (sidebar-set save-line (sidebar-get save-line-files))
  (sidebar-set files (sidebar-get save-files))
  (sidebar-set default-width (sidebar-get save-default-width))
  (sidebar-set root-project (sidebar-get save-root-project))
  (sidebar-set history (sidebar-get save-history))
  (sidebar-set current-path (sidebar-get save-current-path)))

(defun sidebar-switch-to-buffers ()
  "."
  (interactive)
  (sidebar-save-state)
  (ignore-errors (kill-buffer (sidebar-cons-buffer-name)))
  (sidebar-set buffers-force t)
  (sidebar-set saved-state-files t)
  (sidebar-set save-line nil)
  (sidebar-open))

(defun sidebar-kill ()
  "Kill the sidebar's buffer."
  (interactive)
  (sidebar-set saved-state-files nil)
  (sidebar-set window-start nil)
  (sidebar-set save-line-files nil)
  (sidebar-set save-files nil)
  (sidebar-set save-default-width nil)
  (sidebar-set save-root-project nil)
  (sidebar-set save-history nil)
  (sidebar-set save-current-path nil)
  (sidebar-set files nil)
  (sidebar-set default-width nil)
  (sidebar-set root-project nil)
  (sidebar-set history nil)
  (sidebar-set current-path nil)
  (ignore-errors (kill-buffer (sidebar-cons-buffer-name))))

(defun sidebar-init-mode ()
  "."
  (face-remap-add-relative 'header-line '((:inherit sidebar-header-face :background "" :foreground "")))
  (face-remap-add-relative 'mode-line '((:inherit sidebar-header-face :foreground "" :background "" :box nil)))
  (face-remap-add-relative 'mode-line-inactive '((:inherit sidebar-header-face :foreground "" :background "" :box nil)))

  (setq cursor-type nil
	buffer-read-only t
	mode-line-format (list '(:eval (sidebar-set-modeline)))
	header-line-format (list '(:eval (sidebar-set-header))))

  (if (sidebar-gui?)
      (progn
	;; (setq sidebar-status-on-directory sidebar-gui-status-on-directory)
	;; (setq sidebar-filename-colored sidebar-gui-filename-colored)
	;; (setq sidebar-status-on-file sidebar-gui-status-on-file)
	(copy-face 'sidebar-powerline-gui-face 'sidebar-powerline-face)
	(copy-face 'sidebar-file-gui-face 'sidebar-file-face)
	(copy-face 'sidebar-dir-gui-face 'sidebar-dir-face)
	(copy-face 'sidebar-untracked-gui-face 'sidebar-untracked-face)
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
	(copy-face 'sidebar-suffix-path-header-gui-face 'sidebar-suffix-path-header-face)
	(copy-face 'sidebar-match-gui-face 'sidebar-match-face))
    ;; (setq sidebar-status-on-directory sidebar-terminal-status-on-directory)
    ;; (setq sidebar-filename-colored sidebar-terminal-filename-colored)
    ;; (setq sidebar-status-on-file sidebar-terminal-status-on-file)
    (copy-face 'sidebar-powerline-terminal-face 'sidebar-powerline-face)
    (copy-face 'sidebar-file-terminal-face 'sidebar-file-face)
    (copy-face 'sidebar-dir-terminal-face 'sidebar-dir-face)
    (copy-face 'sidebar-untracked-terminal-face 'sidebar-untracked-face)
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
    (copy-face 'sidebar-suffix-path-header-terminal-face 'sidebar-suffix-path-header-face)
    (copy-face 'sidebar-match-terminal-face 'sidebar-match-face))

  (remove-hook 'post-command-hook 'global-hl-line-highlight)

  )

(defvar sidebar-mode-map nil
  "Keymap uses with sidebar-mode.")
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
    (define-key map (kbd "C-h") 'sidebar-history)
    (define-key map (kbd "n") 'sidebar-create-file)
    (define-key map (kbd "C-n") 'sidebar-create-directory)
    (define-key map (kbd "C-d") 'sidebar-delete-selected)
    (define-key map (kbd "M-w") 'sidebar-copy-selected)
    (define-key map (kbd "C-w") 'sidebar-cut-selected)
    (define-key map (kbd "C-y") 'sidebar-paste)
    (define-key map (kbd "R") 'sidebar-rename-selected)
    (define-key map (kbd "<tab>") 'sidebar-switch-to-buffers)
    (define-key map (kbd "<right>") 'sidebar-adjust-window-width)
    (define-key map (kbd "<left>") 'sidebar-reset-window-width)
    (define-key map (kbd "?") 'sidebar-help)
    (setq sidebar-mode-map map)))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defface sidebar-header-face
  '(())
  ;;  '((t :background "yellow"))
  "Face used with files."
  :group nil)

(define-derived-mode sidebar-mode nil "Sidebar"
  "Major mode for Sidebar.

\\{sidebar-mode-map}"
  ::group sidebar

  (sidebar-init-mode)
  (add-to-list 'display-buffer-alist '(" SIDEBAR-SELECT" display-buffer-in-side-window (side . left) (slot . 1)))

  (make-local-variable 'post-command-hook)
  (add-hook 'post-command-hook 'sidebar-post-command)
  (add-hook 'after-save-hook 'sidebar-refresh-on-save t)
  (add-hook 'delete-frame-functions 'sidebar-delete-buffer-on-kill)
  (add-hook 'before-make-frame-hook 'sidebar-before-make-frame-hook)
  (add-hook 'window-configuration-change-hook 'sidebar-config-change-hook)

  )

;; (eval-buffer)
;;(sidebar-open)

(provide 'sidebar)

;;; sidebar.el ends here
