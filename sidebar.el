;;; sidebar.el --- Sidebar major mode

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/sidebar.el
;; Keywords: project, sidebar, projectile
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
;; my project
;;
;; Features that are required by this library:
;;
;;  `projectile'
;;

;;; Code:

(require 'projectile)
(require 'loop)
(require 's)
					;(require 'ov)

(eval-after-load 'dash '(dash-enable-font-lock))

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

(defcustom sidebar-width 40
  "Width of `sidebar'."
  :type 'integer
  :group 'sidebar)

(defcustom sidebar-character-dir-closed "+" ;"▸"
  "Character to use before a closed directory."
  :type 'string
  :group 'sidebar)

(defcustom sidebar-character-dir-opened "↳" ;"-" ;"▾"
  "Character to use before an opened directory."
  :type 'string
  :group 'sidebar)

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defface sidebar-dir-face
  '((t :foreground "color-27"))
  "Face used with directories."
  :group 'sidebar-face)

(defface sidebar-powerline-face
  '((t :background "color-27" ;"#0087af"
       :foreground "black"))
  "Face used for the powerline."
  :group 'sidebar-face)

(defvar sidebar-files '())
(defvar sidebar-current-path nil)
;;;(defvar sidebar-window-origin nil)
(defvar sidebar-closed-directories nil)
(defvar sidebar-root-project nil)
(defvar sidebar-git-hashtable nil)
(defvar sidebar-header-text "Sidebar")

(defun sidebar-get-root-project ()
  "."
  (ignore-errors (projectile-project-root)))

(defun sidebar-project-root ()
  "Return the project root using projectile.
If it's not a project, return the file directory.
If it's not a file, return the home directory."
  (interactive)
  (or (sidebar-get-root-project)
      (when buffer-file-name (file-name-directory buffer-file-name))
      "~"))

(defun sidebar-cons-buffer-name ()
  "Construct the buffer name from 'sidebar-buffer-name' and the frame name."
  (concat "*" sidebar-buffer-name "-" (frame-parameter nil 'name) "*"))

(defun sidebar-get-buffer ()
  "Return the existing/created sidebar buffer for the current frame."
  (get-buffer-create (sidebar-cons-buffer-name)))

(defun sidebar-cons-git-buffer-name ()
  "Construct the buffer name from 'sidebar-buffer-name' and the frame name."
  (concat "*" sidebar-buffer-name "-" (frame-parameter nil 'name) "-GIT*"))

(defun sidebar-get-git-buffer ()
  "Return the existing/created sidebar buffer for the current frame."
  (get-buffer-create (sidebar-cons-git-buffer-name)))

(defun sidebar-exists-p ()
  "Check if a sidebar for the frame exists."
  (get-buffer (sidebar-cons-buffer-name)))

(defun sidebar-file-struct (file)
  "FILE."
  (list (cons 'path file)
	(cons 'dir (file-directory-p file))
	(cons 'line 0)
	(cons 'opened nil)))

(defun sidebar-get-current-line ()
  "Return the current line in buffer."
  (string-to-number (format-mode-line "%l")))

(defun sidebar-print-selected (str)
  "Print STR with powerline."
  (insert (propertize (s-pad-right (- (window-width (sidebar-get-window)) 2) " " str) 'font-lock-face '(:background "blue")))
  (insert (propertize "" 'font-lock-face '(:background nil :foreground "blue"))))

(defun sidebar-dir-arrow (dirname opened)
  "DIRNAME OPENED."
  (if opened
      (concat sidebar-character-dir-opened " " dirname)
    (concat sidebar-character-dir-closed " " dirname)))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-calc-depth (file)
  "FILE."
  (let* ((path-from-current (s-chop-prefix sidebar-current-path (--getpath file)))
	 (depth (s-count-matches "/" path-from-current)))
    (when (> depth 0)
      (setq depth (* depth 2))
      (unless (--dir? file)
	(setq depth (+ depth 1))))
    (+ depth 1)))

(defun sidebar-get-filename-dir-indicator (file)
  "FILE."
  (or (and (--dir? file)
	   (sidebar-dir-arrow (file-name-nondirectory (--getpath file)) (--opened? file)))
      (file-name-nondirectory (--getpath file))))

(defun sidebar-print-with-git (file)
  "FILENAME FILE."
  (let* ((filename (sidebar-get-filename-dir-indicator file))
	 (depth (sidebar-calc-depth file))
	 (path-in-project (s-chop-prefix sidebar-root-project (--getpath file)))
	 (path-with-correct-dirname (or (and (--dir? file) (file-name-as-directory path-in-project))
					path-in-project))
	 (status (gethash path-with-correct-dirname sidebar-git-hashtable)))

    (when (and (> depth 2) status)
      (setq depth (- depth 2)))
    (insert (s-repeat depth " "))
    (when status
      (insert (propertize "✓ " 'font-lock-face '(:foreground "green"))))
    (insert (propertize filename 'font-lock-face (or (and (--dir? file) 'sidebar-dir-face)
						     '(:foreground "grey"))))
    ;; (insert (propertize filename 'font-lock-face (or (and (--dir? file) '(:foreground "color-27"))
    ;; 						     '(:foreground "grey"))))
    ))

;;('sidebar-dir-face)

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))


(defun sidebar-print-normal (file)
  "FILENAME FILE."
  (let ((filename (sidebar-get-filename-dir-indicator file)))
    (insert (s-repeat (sidebar-calc-depth file) " ") filename)))

(defun sidebar-print-listfiles (list)
  "LIST DEPTH OPENED-DIRS."
  (let ((func-insert (or (and sidebar-git-hashtable 'sidebar-print-with-git)
			 'sidebar-print-normal)))
    (loop-for-each file list
      (setf (--getline file) (line-number-at-pos))
      (funcall func-insert file)
      (newline))))

;; (defun sidebar-print-listfiles (list)
;;   "LIST DEPTH OPENED-DIRS."
;;   (let ((func-insert (or (and sidebar-git-hashtable 'sidebar-print-with-git)
;; 			 'sidebar-print-normal)))
;;     (loop-for-each file list
;;       (let* ((filename (file-name-nondirectory (--getpath file))))
;; 	(when (--dir? file)
;; 	  (setq filename (sidebar-dir-arrow filename (--opened? file))))
;; 	(setf (--getline file) (line-number-at-pos))
;; 	(funcall func-insert filename file)
;; 	(newline)))))



;; (when (--dir? file)
;;   (let ((dirname (file-name-nondirectory (--getpath file)))
;; 	  (opened (--opened? file)))
;;     (setf (--getline file) (line-number-at-pos))
;;     (insert (s-repeat (sidebar-calc-depth file) " ") (sidebar-dir-arrow dirname opened))
;;     (newline))))
;; (loop-for-each file list
;;   (unless (--dir? file)
;;     (let ((filename (file-name-nondirectory (--getpath file))))
;; 	(setf (--getline file) (line-number-at-pos))
;; 	(insert (s-repeat (sidebar-calc-depth file) " ") filename)
;; 	(newline)))))

;; (defun sidebar-print-listfiles (list)
;;   "LIST DEPTH OPENED-DIRS."
;;   (loop-for-each file list
;;     (when (--dir? file)
;;       (let ((dirname (file-name-nondirectory (--getpath file)))
;; 	    (opened (--opened? file)))
;; 	(setf (--getline file) (line-number-at-pos))
;; 	(insert (s-repeat (sidebar-calc-depth file) " ") (sidebar-dir-arrow dirname opened))
;; 	(newline))))
;;   (loop-for-each file list
;;     (unless (--dir? file)
;;       (let ((filename (file-name-nondirectory (--getpath file))))
;; 	(setf (--getline file) (line-number-at-pos))
;; 	(insert (s-repeat (sidebar-calc-depth file) " ") filename)
;; 	(newline)))))

(defun sidebar-sort-files-by-line ()
  "Sort `sidebar-files' by line."
  (setq sidebar-files (-sort (lambda (first second)
			       (< (--getline first) (--getline second)))
			     sidebar-files)))

;; (insert "toto")
;; (defvar ov1 nil)
;; (setq ov1 (ov-insert "coucou toi"))coucou toi
;; (ov-set ov1 'invisible t)Coucou toi
;; (ov-reset ov1)

;; (setq ov1 (ov-line))
;; (ov-set ov1  'invisible t)
;; (ov-reset ov1)

					;(ov-set (ov-insert "Coucou toi") '(face (:background "#00ff00" :height 1.5)))
					;(ov-set (ov-insert "Coucou toi") 'face 'menu 'intangible t)

					;(move-to-column (- (window-width) 1))
					;(end-of-line)

;; (defun sidebar-print-listfiles (list)
;;   "LIST."
;;   (loop-for-each file list
;;     (let ((filename (file-name-nondirectory (--getpath file))))
;;       (insert filename)
;;       (newline))))

					;(insert (propertize "foo" 'font-lock-face '(:background "green")))
					;(column-at-pos)

(defun sidebar-print ()
  "Prints Sidebar."
  (setq sidebar-header-text (abbreviate-file-name sidebar-current-path))
  ;;  (setq header-line-format '((abbreviate-file-name path)))
  (sidebar-print-listfiles sidebar-files)
  (sidebar-sort-files-by-line))

(defun sidebar-dots-file (file)
  "Return t if FILE is '.' or '..'."
  (let ((file (file-name-nondirectory file)))
    (or (string= "." file) (string= ".." file))))

(defvar it)
(defvar other)
;;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-load-dir (path)
  "PATH."
  (let* ((files-and-dirs (-remove 'sidebar-dots-file (directory-files path t)))
	 (dirs-sorted (--sort (string< it other) (-filter 'file-directory-p files-and-dirs)))
	 (files-sorted (--sort (string< it other) (--filter (not (file-directory-p it)) files-and-dirs))))
    (-map 'sidebar-file-struct (-concat dirs-sorted files-sorted))))

;; (defun sidebar-load-dir (path)
;;   "PATH."
;;   (let* ((files-and-dirs (-remove 'sidebar-dots-file (directory-files path t)))
;; 	 (dirs (-filter 'file-directory-p files-and-dirs))
;; 	 (files (--filter (not (file-directory-p it)) files-and-dirs))
;; 	 (dirs-sorted (--sort (string< it other) dirs))
;; 	 (files-sorted (--sort (string< it other) files)))
;;     (-map 'sidebar-file-struct (-concat dirs-sorted files-sorted))))

;; (defun sidebar-load-dir (path)
;;   "PATH."
;;   (let* ((files-dirs (directory-files path t))
;; 	 (files-dirs (-map 'sidebar-file-struct (-remove 'sidebar-dots-file files-dirs)))
;; 	 (dirs (-filter (lambda (file) (--dir? file)) files-dirs))
;; 	 (files (-filter (lambda (file) (not (--dir? file))) files-dirs))
;; 	 (dirs-sorted (-sort (lambda (first second) (string< (--getpath first) (--getpath second))) dirs))
;; 	 (files-sorted (-sort (lambda (first second) (string< (--getpath first) (--getpath second))) files)))
;;     (-concat dirs-sorted files-sorted)))


;; (defun sidebar-load-dir (path)
;;   "PATH."
;;   (let ((files (directory-files path t)))
;;     (-map 'sidebar-file-struct (-remove 'sidebar-dots-file files))))

(defun sidebar-get-window ()
  "."
  (let ((sidebar-window (get-buffer-window (sidebar-cons-buffer-name))))
    (unless sidebar-window
      (setq sidebar-window (split-window (selected-window) (- (frame-width) sidebar-width) 'left t)))
    sidebar-window))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

;;(mapcar (function window-name) (window-list))
(buffer-list)
;;(get-buffer-window "*SIDEBAR-F2*")
;;(get-buffer-window)
;;(buffer-file-name)
(defun sidebar-goto-buffername (buffer-name)
  "BUFFER-NAME."
  (let ((file (--first (string= (--getpath it) buffer-name) sidebar-files)))
    (if file
	(progn
	  (sidebar-goto-line (--getline file))
	  (sidebar-show-current nil))
      (sidebar-goto-line 1)
      (sidebar-show-current nil))))

(defun sidebar-open ()
  "Open or create a sidebar for the current frame."
  (interactive)
  (setq default-directory "/home/sebastien/travaux/lightdm-electron-sample/")
  (set-frame-parameter nil 'sidebar-window-origin (get-buffer-window))
;;;  (setq sidebar-window-origin (get-buffer-window))
  (setq sidebar-root-project (sidebar-get-root-project))
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
      (setq sidebar-current-path project-path-root)
      (setq sidebar-files (sidebar-load-dir project-path-root))
      (sidebar-print)
      (sidebar-goto-buffername buffer-name-current)
      (sidebar-mode))))

;;get-current-buffer

;;;    (internal-show-cursor (sidebar-get-window) nil)

;; (defun sidebar-open ()
;;   "Open or create a sidebar for the current frame."
;;   (interactive)
;;   (setq sidebar-window-origin (get-buffer-window))
;;   (let ((sidebar-exists (sidebar-exists-p))
;; 	(sidebar-buffer (sidebar-get-buffer))
;; 	(sidebar-window (sidebar-get-window))
;; 	(project-path-root (sidebar-project-root))
;; 	(buffer-name-current (buffer-file-name)))
;;     (set-window-buffer sidebar-window sidebar-buffer)
;;     (set-buffer sidebar-buffer)
;;     (unless sidebar-exists
;;       (setq sidebar-current-path project-path-root)
;;       (setq sidebar-files (sidebar-load-dir project-path-root))
;;       (sidebar-print)
;;       (sidebar-goto-buffername buffer-name-current)
;;       (sidebar-mode))
;; ;;;    (internal-show-cursor (sidebar-get-window) nil)
;;     (select-window sidebar-window)))

(defun sidebar-close ()
  "Close the sidebar for the current frame."
  (interactive)
  (let ((sidebar-window (get-buffer-window (sidebar-cons-buffer-name))))
    (when sidebar-window (delete-window sidebar-window))))

					;(kill-buffer (sidebar-cons-buffer-name))

(frame-parameter nil 'name)
(frame-parameters)
(window-list)
(buffer-list)
(sidebar-cons-buffer-name)
(sidebar-project-root)
(abbreviate-file-name "/home/sebastien/travaux/sidebar/")
(setq header-line-format '())
(setq my-var (sidebar-project-root))
(setq header-line-format '("coucou" " cou " my-var))

(let (my-var (sidebar-project-root))
  (setq header-line-format '("coucou" " cou " my-var)))

(let (my-var (sidebar-project-root))
  (setq header-line-format '("" my-var)))

(let (my-dir (sidebar-project-root))
  (setq header-line-format '("saca " my-dir)))

(defun sidebar-test ()
  "."
  (interactive)
  (message "coucou"))

;;(kill-buffer (sidebar-cons-buffer-name))

(defun sidebar-disable-current (line)
  "Print current LINE with background colored."
  (save-excursion
    (let ((file (nth (- (line-number-at-pos) 1) sidebar-files)))
      (when file
	(delete-region (line-beginning-position) (line-end-position))
	(if sidebar-git-hashtable
	    (sidebar-print-with-git file)
	  (sidebar-print-normal file)
	  )))))

;; (let* ((str (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
;; 	 (str (s-trim-right (s-chop-suffixes '("" "") str))))
;;   (save-excursion
;;     (delete-region (line-beginning-position) (line-end-position))
;;     (insert str))))

;; (defun sidebar-disable-current (line)
;;   "Print current LINE with background colored."
;;   (let* ((str (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
;; 	 (str (s-trim-right (s-chop-suffixes '("" "") str))))
;;     (save-excursion
;;       (delete-region (line-beginning-position) (line-end-position))
;;       (insert str))))

(defun sidebar-show-current (line)
  "Print current LINE with background colored."
  (let* ((str (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	 (str (s-pad-right (- (window-width (sidebar-get-window)) 2) " " str)))
    (save-excursion
      (delete-region (line-beginning-position) (line-end-position))
      (insert (propertize str 'font-lock-face 'sidebar-powerline-face))
      (insert (propertize "" 'face `(:foreground ,(face-background 'sidebar-powerline-face)))))))

(defun sidebar-previous-line ()
  "Go the the previous line."
  (interactive)
  (sidebar-disable-current nil)
  (if (= (line-number-at-pos) 1)
      (forward-line (- (count-lines (point-min) (point-max)) 1))
    (forward-line -1))
  (sidebar-show-current nil)
  (message (--getpath (nth (- (line-number-at-pos) 1) sidebar-files))))

(defun sidebar-next-line ()
  "Go the the next line."
  (interactive)
  (sidebar-disable-current nil)
  (forward-line)
  (let ((numbers-of-line (count-lines (point-min) (point-max))))
    (if (> (line-number-at-pos) numbers-of-line)
	(forward-line (- numbers-of-line))))
  (sidebar-show-current nil)
  (message (--getpath (nth (- (line-number-at-pos) 1) sidebar-files))))


;; (loop-for-each file list
;;   (when (--dir? file)
;;     (let ((dirname (file-name-nondirectory (--getpath file)))
;; 	  (opened (--opened? file)))
;;       (setf (--getline file) (line-number-at-pos))
;;       (insert (s-repeat depth " ") (sidebar-dir-arrow dirname  opened))
;;       (newline))))

;;(filename (file-name-nondirectory (--getpath file)))

;;(ignore-errors (kill-buffer (sidebar-cons-buffer-name)))

(defun sidebar-old-as-opened (list path-old)
  "LIST PATH-OLD."
  (let ((file (--first (string= (--getpath it) path-old) list)))
    (setf (--opened? file) t)
    list))

(defun sidebar-up-directory ()
  "."
  (interactive)
  (let* ((new-directory (file-name-directory (directory-file-name sidebar-current-path)))
	 (new-files (sidebar-load-dir new-directory))
	 (old-files sidebar-files)
	 (old-dir (directory-file-name sidebar-current-path)))
    (if (string= old-dir new-directory)
	(message "Sidebar: You're at the top")
      (erase-buffer)
      (setq sidebar-files (sidebar-old-as-opened new-files old-dir))
      (setq sidebar-current-path (file-name-as-directory new-directory))
      (setq default-directory sidebar-current-path)
      (setq sidebar-header-text (abbreviate-file-name new-directory))
      (sidebar-print-listfiles sidebar-files)
      (let* ((old-dir- (--first (string= (--getpath it) old-dir) sidebar-files))
	     (line-to-put-old-files (--getline old-dir-)))
	(sidebar-goto-line (+ line-to-put-old-files 1))
	(sidebar-update-line-number (length old-files) line-to-put-old-files)
	(sidebar-print-listfiles old-files)
	(setq sidebar-files (-concat sidebar-files old-files))
	(sidebar-sort-files-by-line)
	(sidebar-goto-line line-to-put-old-files)
	(sidebar-show-current nil)))))

(defun sidebar-open-directory (file)
  "FILE."
  (let ((files nil))
    (if (--opened? file)
	(let ((dirname (file-name-as-directory (--getpath file))))
	  (setq files (--filter (s-starts-with? dirname (--getpath it)) sidebar-files)))
      (setq files (sidebar-load-dir (--getpath file))))
    (erase-buffer)
    (setq sidebar-files files)
    (setq sidebar-current-path (file-name-as-directory (--getpath file)))
    (setq default-directory sidebar-current-path)
    (setq sidebar-header-text (abbreviate-file-name (--getpath file)))
    (sidebar-print-listfiles files)
    (sidebar-goto-line 1)
    (sidebar-show-current nil)))

(defun sidebar-find-file-from-line (line)
  "LINE."
  (let ((found))
    (loop-for-each file sidebar-files
      (when (= (--getline file) line)
	(setq found file)
	(loop-break)))
    found))

(defun sidebar-open-file (file)
  "Open FILE in the buffer where the sidebar has been called."
  (let ((buffer-file (find-file-noselect (--getpath file))))
    (set-window-buffer (frame-parameter nil 'sidebar-window-origin) buffer-file)))
;;;(set-window-buffer sidebar-window-origin buffer-file)))

;;set-frame-parameter
;;make-variable-frame-local

(defun sidebar-open-line ()
  "."
  (interactive)
  (let* ((line (line-number-at-pos))
	 (file (sidebar-find-file-from-line line)))
    (if (--dir? file)
	(sidebar-open-directory file) ;TODO
      (sidebar-open-file file))))

;;(kill-buffer (sidebar-cons-buffer-name))

;;	(setf (--getline file) (line-number-at-pos))

(defun sidebar-update-line-number (num line)
  "Add NUM to every file who's line number is > than LINE."
  (let ((list sidebar-files))
    (loop-for-each file list
      (let ((line-for-this-file (--getline file)))
	(when (> line-for-this-file line)
	  (setf (--getline file) (+ line-for-this-file num)))))))

;; (defun sidebar-count-depth (str)
;;   "Count how many space characters there is at the beginning of STR."
;;   (let ((depth 0) (str (append str nil)))
;;     (loop-while str
;;       (if (= (car str) 32)
;; 	  (setq depth (+ depth 1)
;; 		str (cdr str))
;; 	(setq str nil)))
;;     depth))

(defun sidebar-search-closed-dir (file)
  "FILE."
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
  "FILE LINE."
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
    (search-forward sidebar-character-dir-closed (line-end-position))
    (replace-match (propertize sidebar-character-dir-opened 'font-lock-face '(:background "#0087af" :foreground "black")))))

;;(file-name-as-directory "/home/sebastien/travaux/sidebar.el/src")
;;(message (--getpath (nth (- (line-number-at-pos) 1) sidebar-files))))

(defun sidebar-delete-line ()
  "."
  (delete-region (line-beginning-position) (line-end-position))
  (delete-char 1))

(defun sidebar-update-closed-dirs (dir list)
  "DIR LIST."
  (setq sidebar-closed-directories (-insert-at 0 (-concat (list dir) list) sidebar-closed-directories)))

(defun sidebar-close-dir (file line)
  "FILE LINE."
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
    (search-forward sidebar-character-dir-opened (line-end-position))
    (replace-match (propertize sidebar-character-dir-closed 'font-lock-face '(:background "#0087af" :foreground "black")))))

(defun sidebar-expand-or-close-dir ()
  "."
  (interactive)
  (let* ((line (line-number-at-pos))
	 (file (sidebar-find-file-from-line line)))
    (when (--dir? file)
      (if (--opened? file)
	  (sidebar-close-dir file line)
	(sidebar-expand-dir file line)))))

;; (defun sidebar-goto-line (line)
;;   "Go to LINE."
;;   (forward-line (- line (line-number-at-pos))))

(defun sidebar-goto-line (line)
  "Go to LINE."
  (let ((max (count-lines (point-min) (point-max))))
    (when (> line max)
      (setq line max))
    (forward-line (- line (line-number-at-pos)))))

(defun sidebar-update-from-opened-dirs (list opened)
  "LIST OPENED."
  (loop-for-each file opened
    (let ((found (--first (string= (--getpath it) (--getpath file)) list)))
      (when found
	(setf (--opened? found) t))))
  list)

(defun sidebar-refresh ()
  "."
  (interactive)
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
	  (sidebar-goto-line (+ (--getline found) 1))
	  (let* ((new-files (sidebar-load-dir (--getpath found)))
		 (new-files (sidebar-update-from-opened-dirs new-files opened-dirs)))
	    (sidebar-print-listfiles new-files)
	    (sidebar-update-line-number (length new-files) (--getline found))
	    (setq sidebar-files (-concat sidebar-files new-files))
	    (sidebar-sort-files-by-line)))))
    (sidebar-goto-line current-line)
    (sidebar-show-current nil))
  (message "Sidebar refreshed"))

(defun sidebar-refresh-on-save-after-timer ()
  "Function called when a buffer is saved, it refreshes the sidebar."
  (save-excursion
    (let ((sidebar-window (get-buffer-window (sidebar-cons-buffer-name))))
      (when sidebar-window
	(set-buffer (sidebar-get-buffer))
	(sidebar-refresh)))))

(defun sidebar-refresh-on-save ()
  "Function called when a buffer is saved, it refreshes the sidebar.
I'm using a timer because, with my config, flycheck write a file in the
current directory (I don't know why) and it appears in the Sidebar.
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
	((s-matches? "^M[ MD]$" status) 'updated)
	((s-matches? "^A[ MD]$" status) 'added)
	((s-matches? "^D[ M]$" status) 'deleted)
	((s-matches? "^D[ M]$" status) 'renamed)
	((s-matches? "^[MARC] $" status) 'match)
	((s-matches? "^[ MARC]M$" status) 'changed)
	((s-matches? "^[ MARC]D$" status) 'deleted)
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

(defun sidebar-git-parse-buffer (process)
  "PROCESS."
  (with-current-buffer (sidebar-get-git-buffer)
    (let* ((data (buffer-substring-no-properties (point-min) (point-max)))
	   (str-table (s-split "\0" data t))
	   (table (make-hash-table :test 'equal :size (- (length str-table) 1))))
      (sidebar-git-parse-branch (car str-table))
      (setq str-table (cdr str-table))
      (loop-for-each line str-table
	(let ((status (sidebar-git-match-status (substring line 0 2)))
	      (filepath (substring line 3 nil)))
	  (puthash filepath status table)
	  ))
      table)))

;; (defun sidebar-git-update-sidebar table
;;   "Update the sidebar with git information for each file."
;;   ;; (setq sidebar-git-hashtable table))
;;   )

(defun sidebar-git-handle-exit (process change)
  "Function call on PROCESS exits.
CHANGE is unused"
  (when (eq (process-status process) 'exit)
    (if (/= (process-exit-status process) 0)
	(message "Git failed")
      (message "Git succed")
      (let ((table (sidebar-git-parse-buffer process)))
	(setq sidebar-git-hashtable table)
	(sidebar-refresh)))
    (ignore-errors (kill-buffer (sidebar-get-git-buffer)))))
;;;  (sidebar-git-update-sidebar table)))))

(defun sidebar-run-git ()
  "."
  (interactive)
  (when sidebar-root-project
    (let ((process (get-buffer-process (sidebar-get-git-buffer))))
      (when (and process (process-live-p process))
	(kill-process process)))
    (with-current-buffer (sidebar-get-git-buffer)
      (erase-buffer))
    (let ((process (start-process "sidebar-git" (sidebar-get-git-buffer) "git" "status" "--porcelain" "--ignored" "-z" "-b" ".")))
      (set-process-query-on-exit-flag process nil)
      (set-process-sentinel process 'sidebar-git-handle-exit))))
;;   (lambda (process _event)
;;     (when (eq (process-status process) 'exit)
;;       (if (/= (process-exit-status process) 0)
;; 	  (message "Git failed")
;; 	(message "Git succed")
;; 	(sidebar-git-parse-buffer process)
;; ;;;	     (ignore-errors (kill-buffer (sidebar-get-git-buffer)))
;; 	)))))))

;;(start-process "my-process" "foo" "ls" "-l" "/bin")
;; (when (buffer-live-p (process-buffer proc))
;;   (let ((stats (dired-k--parse-git-status root proc deep)))
;;     (funcall callback stats curbuf)
;;     (kill-buffer proc-buf))))))))))
;;;  make-hash-table

(defvar sidebar-mode-map nil
  "")
(unless sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x a") 'sidebar-test)
    (define-key map (kbd "q") 'sidebar-close)
    (define-key map (kbd "g") 'sidebar-run-git)
    (define-key map (kbd "SPC") 'sidebar-expand-or-close-dir)
    (define-key map (kbd "DEL") 'sidebar-up-directory)
    (define-key map (kbd "RET") 'sidebar-open-line)
    (define-key map (kbd "h") 'sidebar-refresh)
    (define-key map (kbd "<up>") 'sidebar-previous-line)
    (define-key map (kbd "C-p") 'sidebar-previous-line)
    (define-key map (kbd "<down>") 'sidebar-next-line)
    (define-key map (kbd "C-n") 'sidebar-next-line)
    (setq sidebar-mode-map map)))

(define-derived-mode sidebar-mode nil "Sidebar"
  "Major mode for Sidebar.

\\{sidebar-mode-map}"
  ::group sidebar
  (make-local-variable 'sidebar-header-text)
  (make-local-variable 'sidebar-files)
  (make-local-variable 'sidebar-current-path)
  ;;  (make-local-variable 'sidebar-window-origin)
  (make-local-variable 'sidebar-closed-directories)
  (make-local-variable 'sidebar-root-project)
  (make-local-variable 'sidebar-git-hashtable)
  (add-hook 'after-save-hook 'sidebar-refresh-on-save t)
  (add-hook 'delete-frame-functions 'sidebar-delete-buffer-on-kill)
  ;;  (internal-show-cursor nil nil)
  ;;  (use-local-map sidebar-mode-map)
  ;;  (hl-line-mode)
  ;;  (setq header-line-format '(list "-" sidebar-header-text)
  (setq header-line-format '(list "-" sidebar-header-text)
	buffer-read-only nil
	mode-line-format nil))
;; 	truncate-lines t
;; 	buffer-read-only nil))

;; (if root-project
;;     (message root-project)
;;   nil))



(provide 'sidebar)

;;; sidebar.el ends here
