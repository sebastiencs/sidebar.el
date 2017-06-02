;;; sidebar-face.el --- sidebar-face

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/sidebar.el

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

;;; Code:

(defgroup sidebar-terminal-face nil
  "Faces uses in sidebar on terminals."
  :prefix "sidebar-"
  :link '(info-link "(sidebar) Frames and Faces")
  :group 'sidebar
  :group 'faces)

(defgroup sidebar-gui-face nil
  "Faces uses in sidebar with gui."
  :prefix "sidebar-"
  :link '(info-link "(sidebar) Frames and Faces")
  :group 'sidebar
  :group 'faces)

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

;; (defface sidebar-mode-line-terminal-face
;;   '((t :foreground "white"
;;        :background "#222222"))
;;   "Face used with the mode line."
;;   :group 'sidebar-terminal-faces)

(defface sidebar-branch-terminal-face
  '((t :foreground "white"
       :background "#222222"))
  "Face used on the current branch."
  :group 'sidebar-terminal-faces)

(defface sidebar-icon-branch-terminal-face
  '((t :foreground "white"
       :background "#222222"))
  "Face used on the icon before the current branch."
  :group 'sidebar-terminal-faces)

(defface sidebar-remotebranch-terminal-face
  '((t :foreground "white"
       :background "#222222"))
  "Face used on the remote branch."
  :group 'sidebar-terminal-faces)

(defface sidebar-icon-remotebranch-terminal-face
  '((t :foreground "white"
       :background "#222222"))
  "Face used on the icon after the remote branch."
  :group 'sidebar-terminal-faces)

(defface sidebar-header-line-terminal-face
  '((t :foreground "white"
       :background "#222222"))
  "Face used with the header line."
  :group 'sidebar-terminal-faces)

(defface sidebar-icon-header-project-terminal-face
  '((t :foreground "white"
       :background "#222222"))
  "Face used with the icon before a project name."
  :group 'sidebar-terminal-faces)

(defface sidebar-icon-header-directory-terminal-face
  '((t :foreground "white"
       :background "#222222"))
  "Face used with the icon before a directory name (On the header line)."
  :group 'sidebar-terminal-faces)

(defface sidebar-powerline-gui-face
;;;  '((t :background "#005fff"
  '((t :background "#1A237E"
       :foreground "white"))
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

(defface sidebar-branch-gui-face
  '((t :foreground "white"
       :background "#1A237E"))
  "Face used on the current branch."
  :group 'sidebar-gui-faces)

(defface sidebar-icon-branch-gui-face
  '((t :foreground "white"
       :background "#1A237E"))
  "Face used on the icon before the current branch."
  :group 'sidebar-gui-faces)

(defface sidebar-remotebranch-gui-face
  '((t :foreground "white"
       :background "#1A237E"))
  "Face used on the remote branch."
  :group 'sidebar-gui-faces)

(defface sidebar-icon-remotebranch-gui-face
  '((t :foreground "white"
       :background "#1A237E"))
  "Face used on the icon after the remote branch."
  :group 'sidebar-gui-faces)

(defface sidebar-header-line-gui-face
  '((t :foreground "white"
       :background "#1A237E"))
  "Face used with the header line."
  :group 'sidebar-terminal-faces)

(defface sidebar-icon-header-project-gui-face
  '((t :foreground "white"
       :background "#1A237E"))
  "Face used with the icon before a project name."
  :group 'sidebar-terminal-faces)

(defface sidebar-icon-header-directory-gui-face
  '((t :foreground "white"
       :background "#1A237E"))
  "Face used with the icon before a directory name (On the header line)."
  :group 'sidebar-terminal-faces)

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
(defface sidebar-header-line-face nil "" :group nil)
(defface sidebar-branch-face nil "" :group nil)
(defface sidebar-remotebranch-face nil "" :group nil)
(defface sidebar-icon-header-project-face nil "" :group nil)
(defface sidebar-icon-header-directory-face nil "" :group nil)

(provide 'sidebar-face)

;;; sidebar-face.el ends here
