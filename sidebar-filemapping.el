;;; sidebar-filemapping.el --- sidebar-filemapping  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/sidebar.el
;; Keywords: files, convenience, frames
;; Version: 0.0.1
;; Package-Requires: ((emacs "25" )(dash "2.11.0") (projectile "0.10.0"))

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
;; files mapping

;;; Code:

(require 's)
(require 'dash)

(defvar-local sidebar-filemapping-cache-hashtable nil)

(defvar sidebar-filemapping-extension-hashtable (make-hash-table :test 'equal :size 300))
(defvar sidebar-filemapping-full-hashtable (make-hash-table :test 'equal))
(defvar sidebar-filemapping-prefix-hashtable (make-hash-table :test 'equal))
(defvar sidebar-filemapping-suffix-hashtable (make-hash-table :test 'equal))

(defconst sidebar-filemapping-gui-color
  '((red . "#FF3A20")
    (pink . "#F50057")
    (purple . "#D500F9")
    (blue . "#2979FF")
    (cyan . "#00E5FF")
    (green . "#00E676")
    (yellow . "#FFEA00")
    (orange . "#FF9100")
    (maroon . "#795548")
    (silver . "#757575")
    (light_red . "#FF5D48")
    (light_pink . "#FF4081")
    (light_purple . "#E040FB")
    (light_blue . "#448AFF")
    (light_cyan . "#18FFFF")
    (light_green . "#76FF03")
    (light_yellow . "#FFFF00")
    (light_orange . "#FFAB40")
    (light_maroon . "#8D6E63")
    (light_silver . "#BDBDBD")
    (dark_red . "#D1301B")
    (dark_pink . "#C51162")
    (dark_purple . "#AA00FF")
    (dark_blue . "#2962FF")
    (dark_cyan . "#00B8D4")
    (dark_green . "#2E7D32")
    (dark_yellow . "#FFD600")
    (dark_orange . "#FF6D00")
    (dark_maroon . "#5D4037")
    (dark_silver . "#616161")
    (blue_grey . "#607D8B")))

(defmacro sidebar--add (table data &rest extensions)
  "TABLE DATA EXTENSIONS."
  (let ((result nil)
	    (ext (-flatten extensions)))
    (--each ext
      (push `(puthash ,it ',data ,table) result))
    `(progn ,@result)))

(defmacro sidebar--add-to-hashtable (table &rest items)
  "TABLE ITEMS."
  (let ((resultat nil))
    (--each items
      (if (macrop (car it))
	      (let ((elems (macroexpand-all it)))
	        (--each elems
	          (push `(sidebar--add ,table ,(car it) ,(cdr it)) resultat)))
	    (push `(sidebar--add ,table ,(car it) ,(cdr it)) resultat)))
    `(progn ,@resultat)))

(defmacro sidebar--with-icon (icon &rest items)
  "ICON ITEMS."
  (let ((result nil))
    (--each items
      (push `(,(-concat (car it) `(:icon ,icon)) ,(cdr it)) result))
    result))

(defmacro sidebar--add-to-extensions (&rest items)
  "ITEMS."
  `(sidebar--add-to-hashtable
    sidebar-filemapping-extension-hashtable
    ,@items))

(defmacro sidebar--add-to-full (&rest items)
  "ITEMS."
  `(sidebar--add-to-hashtable
    sidebar-filemapping-full-hashtable
    ,@items))

(defmacro sidebar--add-to-prefix (&rest items)
  "ITEMS."
  `(sidebar--add-to-hashtable
    sidebar-filemapping-prefix-hashtable
    ,@items))

(defmacro sidebar--add-to-suffix (&rest items)
  "ITEMS."
  `(sidebar--add-to-hashtable
    sidebar-filemapping-suffix-hashtable
    ,@items))

(sidebar--add-to-extensions
 ((file_video . red) "mpeg" "mpg")
 ((file_video . dark_blue) "webm")
 ((file_video . red) "flv")
 ((file_video . purple) "mkv")
 ((file_video . cyan) "mov")
 ((file_video . blue) "avi")
 ((file_video . dark_blue) "mp4" "m4v" "h264")
 ((file_video . blue) "3gpp" "3gp")
 ((fa_file_code_o . blue) "xml")
 ((file_tag . red) "gemtags")
 ((file_tag . orange) "pid")
 ((file_tag . blue) "CTAGS" "TAGS")
 ((file_tag . blue) "ctags" "tags")
 ((dev_swift . green) "swift")
 ((file_stylus . green) "styl" "stylus")
 ((mfizz_scala . red) "sc" "scala")
 ((mfizz_python . dark_pink) "tac")
 ((mfizz_python . maroon) "pyw")
 ((mfizz_python . blue) "pyi")
 ((mfizz_python . dark_blue) "py3")
 ((mfizz_python . dark_green) "pyt")
 ((mfizz_python . dark_purple) "pip")
 ((mfizz_python . dark_green) "gypi")
 ((mfizz_python . green) "gyp")
 ((mfizz_python . orange) "pep")
 ((mfizz_python . dark_green) "isolate")
 ((mfizz_python . blue) "ipy")
 ((mfizz_python . dark_blue) "py")
 ((file_php . dark_blue) "php")
 ((mfizz_perl . red) "psgi" "xs")
 ((mfizz_perl . dark_blue) "pm")
 ((mfizz_perl . purple) "plx")
 ((mfizz_perl . dark_purple) "ph" "pl")
 ((mfizz_perl . blue) "perl" "pl")
 ((file_org . dark_green) "org")
 ((mfizz_nodejs . dark_green) "node" "node-version")
 ((mfizz_nodejs . green) "njs" "nvmrc")
 ((oct_markdown . blue) "md" "markdown")
 ((file_julia . purple) "jl")
 ((mfizz_reactjs . blue) "jsx" "react")
 ((mfizz_java_bold . purple) "java")
 ((file_dashboard . green) "cpuprofile")
 ((file_dashboard . orange) "slim|skim")
 ((mfizz_ruby . dark_yellow) "watcher")
 ((mfizz_ruby . dark_red) "rbuild" "rbw" "rbx")
 ((mfizz_ruby . red) "irbrc" "gemrc" "pryrc" "ruby-gemset" "ruby-version")
 ((mfizz_ruby . red) "pluginspec" "podspec" "rabl" "rake" "opal")
 ((mfizz_ruby . red) "ruby" "rb" "ru" "erb" "gemspec" "god" "mspec")
 ((file_haml . maroon) "hamlc")
 ((file_haml . yellow) "haml")
 ((mfizz_haskell . dark_blue) "lhs")
 ((mfizz_haskell . dark_purple) "c2hs")
 ((mfizz_haskell . blue) "hsc")
 ((mfizz_haskell . purple) "hs")
 ((file_mustache . orange) "hbs" "handlebars" "mustache")
 ((file_go . blue) "go")
 ((mfizz_erlang . dark_green) "yrl")
 ((mfizz_erlang . green) "xrl")
 ((mfizz_erlang . maroon) "hrl")
 ((mfizz_erlang . dark_red) "beam")
 ((mfizz_erlang . red) "erl")
 ((mfizz_elm . blue) "elm")
 ((mfizz_elixir . purple) "exs" "eex")
 ((mfizz_elixir . dark_purple) "ex")
 ((dev_coffeescript . blue) "iced")
 ((dev_coffeescript . light_maroon) "litcoffee")
 ((dev_coffeescript . dark_maroon) "cjsx")
 ((dev_coffeescript . maroon) "coffee")
 ((mfizz_clojure . red) "hic")
 ((mfizz_clojure . red) "cljx")
 ((mfizz_clojure . green) "cljc")
 ((mfizz_clojure . purple) "cl2")
 ((mfizz_clojure . blue) "clj")
 ((md_vpn_key . blue) "crt")
 ((md_vpn_key . orange) "pem")
 ((md_vpn_key . yellow) "pub")
 ((md_vpn_key . blue) "key")
 ((dev_bower . yellow) "bowerrc" "bowerfile")
 ((file_babel . yellow) "babel" "babelrc" "languagebabel")
 ((file_babel . dark_yellow) "babelignore")
 ((mfizz_sass . pink) "scss" "sass")
 ((fa_file_image_o . yellow) "gif")
 ((fa_file_image_o . dark_orange) "raw")
 ((fa_file_image_o . red) "bmp")
 ((fa_file_image_o . dark_blue) "webp")
 ((fa_file_image_o . blue) "ico")
 ((fa_file_image_o . green) "jpg")
 ((fa_file_image_o . yellow) "gif")
 ((fa_file_image_o . orange) "png")
 ((file_openoffice . blue) "odt")
 ((file_word . blue) "doc" "docx")
 ((mfizz_database_alt2 . light_pink)  "git")
 ((mfizz_database_alt2 . pink) "qml")
 ((mfizz_database_alt2 . maroon) "cson")
 ((mfizz_database_alt2 . light_red) "yaml" "yml")
 ((mfizz_database_alt2 . yellow) "json")
 ((fa_file_text_o . maroon) "log" "journal")
 ((fa_file_text_o . purple) "srt")
 ((oct_file_binary . light_blue) "asm" "s" "nasm" "masm")
 ((oct_file_binary . light_pink) "elf" "elc")
 ((oct_file_binary . dark_orange) "bin" "bsdiff" "dat" "pak" "pdb")
 ((oct_file_binary . dark_blue) "objdump" "d-objdump")
 ((oct_file_binary . dark_purple) "pyc" "pyo")
 ((file_font . green) "ttf")
 ((file_font . dark_blue) "woff2")
 ((file_font . blue) "woff")
 ((file_font . light_green) "eot")
 ((file_font . dark_green) "ttc")
 ((file_font . dark_yellow) "otf")
 ((mfizz_debian . red) "deb")
 ((oct_package . light_blue) "bundle")
 ((mfizz_osx . red) "dmg")
 ((fa_windows . dark_purple) "exe" "com" "msi" "bat" "cmd" "reg")
 ((md_music_note . red) "mp3")
 ((md_music_note . blue) "wma")
 ((md_music_note . cyan) "m4a")
 ((md_music_note . dark_red) "flac")
 ((md_music_note . yellow) "wav")
 ((md_music_note . dark_cyan) "acc" "ac3" "m4p")
 ((fa_file_pdf_o . red) "pdf")
 ((mfizz_redhat . red) "rpm")
 ((mfizz_redhat . dark_red) "spec")
 ((mfizz_html5 . orange) "html")
 ((mfizz_css3 . dark_blue) "less")
 ((mfizz_css3 . blue) "css")
 ((mfizz_svg . dark_cyan) "svg")
 ((file_test_js . orange) "test-js")
 ((mfizz_nodejs . yellow) "js" "node" "_js" "es6" "es")
 ((oct_terminal . yellow) "tcsh" "csh")
 ((oct_terminal . red) "login" "profile" "inputrc")
 ((oct_terminal . blue) "zsh")
 ((oct_terminal . dark_yellow) "ksh")
 ((oct_terminal . dark_purple) "bashrc" "bash_profile")
 ((oct_terminal . purple) "sh" "rc" "bats" "bash" "tool" "install" "command")
 ((oct_terminal . green) "fish" "fishrc")
 ((oct_file_zip . blue) "rar")
 ((file_config . yellow) "conf" "config" "ini" "desktop" "cfg" "directory" "prefs")
 ((oct_file_zip . light_orange) "egg")
 ((oct_file_zip . dark_orange) "xar")
 ((oct_file_zip . purple) "war")
 ((oct_file_zip . dark_pink) "jar")
 ((oct_file_zip . green) "epub")
 ((oct_file_zip . dark_blue) "whl")
 ((oct_file_zip . red) "gem")
 ((oct_file_zip . orange) "xpi")
 ((oct_file_zip . blue) "iso")
 ((oct_file_zip . light_maroon) "nzb")
 ((oct_file_zip . dark_cyan) "bz2")
 ((oct_file_zip . dark_blue) "tar")
 ((oct_file_zip . red) "apk")
 ((oct_file_zip . maroon) "7z")
 ((oct_file_zip . dark_blue) "tgz" "gz")
 ((oct_file_zip . dark_red) "zip" "xz" "z")
 ((file_cmake . green) "cmake")
 ((mfizz_docker . dark_blue) "dockerfile" "dockerignore")
 ((file_emacs . purple) "el" "emacs" "spacemacs" "emacs")
 ((mfizz_cplusplus . blue) "cpp" "c++" "cxx" "cc")
 ((mfizz_cplusplus . purple) "hh" "hpp" "hxx")
 ((mfizz_c . blue) "c")
 ((mfizz_c . purple) "h")
 ((mfizz_rust . maroon) "rs")
 ((file_powerpoint . light_pink) "ppt" "pps" "ppsx" "pptx")
 ((fa_file_text_o . blue) "txt" "text")
 ((oct_database . green) "cache")
 )

(sidebar--add-to-suffix
 ((mfizz_html5 . red) "html.erb")
 ((file_test_js . orange) "test.js" "test.node" "test._js" "test.es6" "test.es")
 ((dev_bower . yellow) "bower.json")
 ((dev_coffeescript . red) "coffee.erb")
 ((dev_coffeescript . cyan) "coffee.ecr")
 ((mfizz_erlang . dark_maroon) "app.src")
 ((file_haml . red) "haml.deface")
 ((mfizz_reactjs . blue) "react.js")
 )

(sidebar--add-to-full
 ((file_tag . blue) "CTAGS" "TAGS")
 ((file_tag . blue) "ctags" "tags")
 ((oct_file_binary . dark_green) "a.out")
 ((oct_package . green) "*Packages*")
 ((fa_folder . dark_blue) "." "..")
 ((oct_bookmark . light_pink) "bookmark")
 ((file_cmake . red) "CMakeLists.txt")
 ((oct_terminal . red) "depcomp" "libtool" "compile")
 ((oct_terminal . red) "configure" "config.guess" "config.rpath" "config.status" "config.sub" "bootstrap")
 ((file_emacs . purple) ".emacs" ".spacemacs")
 ((file_emacs . dark_purple) ".emacs.d")
 ((md_vpn_key . red) "id_rsa")
 ((mfizz_database_alt2 . silver) "HEAD" "ORIG_HEAD" "FETCH_HEAD" "packed-refs")
 ((mfizz_docker . dark_orange) "docker-sync")
 ((mfizz_docker . dark_blue) "Dockerfile" "docker-compose")
 ((mfizz_elixir . light_purple) "mix.ex" "mix.exs" "mix.lock")
 ((mfizz_erlang . red) "rebar.config.lock" "rebar.lock")
 ((mfizz_erlang . dark_green) "Emakefile")
 ((mfizz_gulp . maroon) "gulpfile.coffee")
 ((mfizz_gulp . red) "gulpfile.js" "gulpfile.babel.js")
 ((mfizz_grunt . maroon) "gruntfile.coffee")
 ((mfizz_grunt . yellow) "gruntfile.js")
 ((mfizz_ruby . red) "irbrc" "gemrc" "pryrc" "ruby-gemset" "ruby-version")
 ((mfizz_ruby . red) "rails")
 ((oct_book . yellow) "LICENSE" "license")
 ((oct_book . dark_blue) "NEWS" "news")
 ((oct_book . blue) "CHANGELOG" "ChangeLog" "changelog")
 ((oct_book . dark_blue) "THANKS" "thanks")
 ((oct_book . dark_blue) "MANIFEST" "manifest")
 ((oct_book . dark_blue) "MAINTAINERS" "maintainers")
 ((oct_book . dark_blue) "INSTALL")
 ((oct_book . dark_blue) "HISTORY")
 ((oct_book . light_blue) "AUTHORS")
 ((oct_book . dark_blue) "HACKING" "hacking")
 ((oct_book . dark_blue) "COPYING" "copying")
 ((oct_book . dark_blue) "CONTRIBUTORS" "contributors")
 ((oct_book . dark_blue) "CONTRIBUTING" "contributing")
 ((oct_book . dark_blue) "CONTRIBUTE" "contribute")
 ((oct_book . dark_blue) "CHANGES" "changes")
 ((oct_book . dark_blue) "BUGS" "bugs")
 ((oct_book . dark_blue) "NOTICE" "notice")
 ((oct_book . green) "README" "readme")
 ((oct_checklist . yellow) "TODO")
 )

(sidebar--add-to-prefix
 ((file_diff . green) "*magit-diff:")
 ((fa_git . green) "*magit:")
 ((md_vpn_key . red) "id_rsa")
 ((oct_book . dark_blue) "THANKS." "thanks.")
 ((oct_book . dark_blue) "THANKS-" "thanks-")
 ((file_php . dark_green) "Phakefile")
 ((file_webpack . blue) "webpack.")
 )

(defun sidebar-filemapping-getcolor (color)
  "COLOR."
  (cdr (assq color sidebar-filemapping-gui-color)))

(defun sidebar-filemapping-find-prefix (filename)
  "FILENAME."
  (catch 'stop-map
    (maphash #'(lambda (key val)
	             (when (s-starts-with? key filename)
		           (throw 'stop-map val)))
	         sidebar-filemapping-prefix-hashtable)))

(defun sidebar-filemapping-find-suffix (filename)
  "FILENAME."
  (catch 'stop-map
    (maphash #'(lambda (key val)
	             (when (s-ends-with? key filename)
		           (throw 'stop-map val)))
	         sidebar-filemapping-suffix-hashtable)))

(defun sidebar-filemapping-emacs-buffers (filename)
  "FILENAME."
  (when (and (eq (elt filename 0) ?\*)
	         (eq (elt filename (1- (length filename))) ?\*))
    '(file_emacs . blue_grey)))

(defun sidebar-filemapping-dotfile (filename)
  "FILENAME."
  (when (eq (elt filename 0) ?\.)
    '(oct_gear . default)))

(defun sidebar-filemapping-setcolor (cons)
  "CONS."
  (cons (car cons)
        (sidebar-filemapping-getcolor (cdr cons))))

(defun sidebar-filemapping-lookup (filename)
  "FILENAME."
  (sidebar-filemapping-setcolor
   (or (gethash filename sidebar-filemapping-full-hashtable)
       ;; (sidebar-filemapping-find-prefix filename)
       ;; (sidebar-filemapping-find-suffix filename)
       (gethash (file-name-extension filename) sidebar-filemapping-extension-hashtable)
       (sidebar-filemapping-emacs-buffers filename)
       (sidebar-filemapping-dotfile filename)
       '(fa_file_o . default))))

(provide 'sidebar-filemapping)

;;; sidebar-filemapping.el ends here
