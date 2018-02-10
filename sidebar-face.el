;;; sidebar-face.el --- sidebar-face  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/sidebar.el
;; Version: 0.0.1

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
;; Faces used by sidebar

;;; Code:

(defgroup sidebar-faces nil
  "Faces uses in Sidebar."
  :prefix "sidebar-"
  :group 'sidebar
  :group 'faces)

(defface sidebar-primary-color
  '((((min-colors 16777216))
     :background "#1A237E" :foreground "white")
    (((min-colors 256))
     :background "#005fff" :foreground "black")
    (t
     :background "white" :foreground "black"))
  "Primary color of the sidebar.
This face is inherited by `sidebar-branch',
`sidebar-remotebranch', `sidebar-header-line'
and `sidebar-powerline'"
  :group 'sidebar-faces)

(defface sidebar-file
  '((((min-colors 256))
     :foreground "grey")
    (t
     :foreground "white"))
  "Face used with files."
  :group 'sidebar-faces)

(defface sidebar-dir
  '((t :inherit dired-directory))
  "Face used with directories."
  :group 'sidebar-faces)

(defface sidebar-untracked
  '((((min-colors 16777216))
     :foreground "gray31")
    (((min-colors 256))
     :foreground "#ff8c00")
    (t
     :foreground "white"))
  "Face used with untracked files/directories."
  :group 'sidebar-faces)

(defface sidebar-ignored-dir
  '((((min-colors 256))
     :foreground "#3f3f3f")
    (t
     :foreground "white"))
  "Face used with ignored directories."
  :group 'sidebar-faces)

(defface sidebar-ignored-file
  '((((min-colors 256))
     :foreground "#3f3f3f")
    (t
     :foreground "white"))
  "Face used with ignored files."
  :group 'sidebar-faces)

(defface sidebar-not-updated
  '((((min-colors 16777216))
     :foreground "red")
    (((min-colors 256))
     :foreground "brown")
    (t
     :foreground "white"))
  "Face used for files not updated."
  :group 'sidebar-faces)

(defface sidebar-updated
  '((((min-colors 16777216))
     :foreground "forest green")
    (((min-colors 256))
     :foreground "green")
    (t
     :foreground "white"))
  "Face used for updated files."
  :group 'sidebar-faces)

(defface sidebar-changed
  '((((min-colors 256))
     :foreground "orange")
    (t
     :foreground "white"))
  "Face used for changed files."
  :group 'sidebar-faces)

(defface sidebar-added
  '((((min-colors 16777216))
     :foreground "forest green")
    (((min-colors 256))
     :foreground "green")
    (t
     :foreground "white"))
  "Face used for added files."
  :group 'sidebar-faces)

(defface sidebar-renamed
  '((((min-colors 256))
     :foreground "orange")
    (t
     :foreground "white"))
  "Face used for renamed files."
  :group 'sidebar-faces)

(defface sidebar-match
  '((((min-colors 16777216))
     :foreground "forest green")
    (((min-colors 256))
     :foreground "green")
    (t
     :foreground "white"))
  "Face used for matched files."
  :group 'sidebar-faces)

(defface sidebar-powerline
  '((t :inherit sidebar-primary-color))
  "Face used with the current line."
  :group 'sidebar-faces)

(defface sidebar-branch
  '((t :inherit sidebar-primary-color))
  "Face used on the current branch in the mode line."
  :group 'sidebar-faces)

(defface sidebar-icon-branch
  '((t :inherit sidebar-branch))
  "Face used on the icon before the current branch in the mode line."
  :group 'sidebar-faces)

(defface sidebar-remote-branch
  '((t :inherit sidebar-primary-color))
  "Face used on the remote branch in the mode line."
  :group 'sidebar-faces)

(defface sidebar-icon-remote-branch
  '((t :inherit sidebar-remote-branch))
  "Face used on the icon before the remote branch in the mode line."
  :group 'sidebar-faces)

(defface sidebar-header-line
  '((t :inherit sidebar-primary-color))
  "Face used on the header line."
  :group 'sidebar-faces)

(defface sidebar-suffix-path-header
  '((t :foreground "grey58"))
  "Face used with the suffix on the header line.
The suffix is the additionnal path (if any) of the file on the current line."
  :group 'sidebar-faces)

(defface sidebar-icon-header-project
  '((t :inherit sidebar-header-line))
  "Face used on the header line."
  :group 'sidebar-faces)

(defface sidebar-icon-header-directory
  '((t :inherit sidebar-header-line))
  "Face used on the header line."
  :group 'sidebar-faces)

(provide 'sidebar-face)

;;; sidebar-face.el ends here
