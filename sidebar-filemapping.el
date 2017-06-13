;;; sidebar-filemapping.el --- sidebar-filemapping

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

;;; Code:

(require 's)
(require 'dash)

(defvar-local sidebar-filemapping-cache-hashtable nil)

(defvar sidebar-filemapping-extension-hashtable (make-hash-table :test 'equal :size 300))
(defvar sidebar-filemapping-full-hashtable (make-hash-table :test 'equal))
(defvar sidebar-filemapping-prefix-hashtable (make-hash-table :test 'equal))
(defvar sidebar-filemapping-suffix-hashtable (make-hash-table :test 'equal))

(defconst sidebar-filemapping-gui-color
  '((red "#FF3A20")
    (pink "#F50057")
    (purple "#D500F9")
    (blue "#2979FF")
    (cyan "#00E5FF")
    (green "#00E676")
    (yellow "#FFEA00")
    (orange "#FF9100")
    (maroon "#795548")
    (silver "#757575")
    (light_red "#FF5D48")
    (light_pink "#FF4081")
    (light_purple "#E040FB")
    (light_blue "#448AFF")
    (light_cyan "#18FFFF")
    (light_green "#76FF03")
    (light_yellow "#FFFF00")
    (light_orange "#FFAB40")
    (light_maroon "#8D6E63")
    (light_silver "#BDBDBD")
    (dark_red "#D1301B")
    (dark_pink "#C51162")
    (dark_purple "#AA00FF")
    (dark_blue "#2962FF")
    (dark_cyan "#00B8D4")
    (dark_green "#2E7D32")
    (dark_yellow "#FFD600")
    (dark_orange "#FF6D00")
    (dark_maroon "#5D4037")
    (dark_silver "#616161")
    (blue_grey "#607D8B")))

(defmacro --add (table data &rest extensions)
  "TABLE DATA EXTENSIONS."
  (let ((result nil)
	(ext (-flatten extensions)))
    (--each ext
      (push `(puthash ,it ',data ,table) result))
    `(progn ,@result)))

(defmacro --add-to-hashtable (table &rest items)
  "TABLE ITEMS."
  (let ((resultat nil))
    (--each items
      (if (macrop (car it))
	  (let ((elems (macroexpand-all it)))
	    (--each elems
	      (push `(--add ,table ,(car it) ,(cdr it)) resultat)))
	(push `(--add ,table ,(car it) ,(cdr it)) resultat)))
    `(progn ,@resultat)))

(defmacro --with-icon (icon &rest items)
  "ICON ITEMS."
  (let ((result nil))
    (--each items
      (push `(,(-concat (car it) `(:icon ,icon)) ,(cdr it)) result))
    result))

(defmacro --add-to-extensions (&rest items)
  "ITEMS."
  `(--add-to-hashtable
    sidebar-filemapping-extension-hashtable
    ,@items))

(defmacro --add-to-full (&rest items)
  "ITEMS."
  `(--add-to-hashtable
    sidebar-filemapping-full-hashtable
    ,@items))

(defmacro --add-to-prefix (&rest items)
  "ITEMS."
  `(--add-to-hashtable
    sidebar-filemapping-prefix-hashtable
    ,@items))

(defmacro --add-to-suffix (&rest items)
  "ITEMS."
  `(--add-to-hashtable
    sidebar-filemapping-suffix-hashtable
    ,@items))

;;;(clrhash sidebar-filemapping-extension-hashtable)

(--add-to-extensions
 ((:icon file_video :color red) "mpeg" "mpg")
 ((:icon file_video :color dark_blue) "webm")
 ((:icon file_video :color red) "flv")
 ((:icon file_video :color purple) "mkv")
 ((:icon file_video :color cyan) "mov")
 ((:icon file_video :color blue) "avi")
 ((:icon file_video :color dark_blue) "mp4" "m4v" "h264")
 ((:icon file_video :color blue) "3gpp" "3gp")
 ((:icon fa_file_code_o :color blue) "xml")
 ((:icon file_tag :color red) "gemtags")
 ((:icon file_tag :color orange) "pid")
 ((:icon file_tag :color blue) "CTAGS" "TAGS")
 ((:icon file_tag :color blue) "ctags" "tags")
 ((:icon dev_swift :color green) "swift")
 ((:icon file_stylus :color green) "styl" "stylus")
 ((:icon mfizz_scala :color red) "sc" "scala")
 ((:icon mfizz_python :color dark_pink) "tac")
 ((:icon mfizz_python :color maroon) "pyw")
 ((:icon mfizz_python :color blue) "pyi")
 ((:icon mfizz_python :color dark_blue) "py3")
 ((:icon mfizz_python :color dark_green) "pyt")
 ((:icon mfizz_python :color dark_purple) "pip")
 ((:icon mfizz_python :color dark_green) "gypi")
 ((:icon mfizz_python :color green) "gyp")
 ((:icon mfizz_python :color orange) "pep")
 ((:icon mfizz_python :color dark_green) "isolate")
 ((:icon mfizz_python :color blue) "ipy")
 ((:icon mfizz_python :color dark_blue) "py")
 ((:icon file_php :color dark_blue) "php")
 ((:icon mfizz_perl :color red) "psgi" "xs")
 ((:icon mfizz_perl :color dark_blue) "pm")
 ((:icon mfizz_perl :color purple) "plx")
 ((:icon mfizz_perl :color dark_purple) "ph" "pl")
 ((:icon mfizz_perl :color blue) "perl" "pl")
 ((:icon file_org :color dark_green) "org")
 ((:icon mfizz_nodejs :color dark_green) "node" "node-version")
 ((:icon mfizz_nodejs :color green) "njs" "nvmrc")
 ((:icon oct_markdown :color blue) "md" "markdown")
 ((:icon file_julia :color purple) "jl")
 ((:icon mfizz_reactjs :color blue) "jsx" "react")
 ((:icon mfizz_java_bold :color purple) "java")
 ((:icon file_dashboard :color green) "cpuprofile")
 ((:icon file_dashboard :color orange) "slim|skim")
 ((:icon mfizz_ruby :color dark_yellow) "watcher")
 ((:icon mfizz_ruby :color dark_red) "rbuild" "rbw" "rbx")
 ((:icon mfizz_ruby :color red) "irbrc" "gemrc" "pryrc" "ruby-gemset" "ruby-version")
 ((:icon mfizz_ruby :color red) "pluginspec" "podspec" "rabl" "rake" "opal")
 ((:icon mfizz_ruby :color red) "ruby" "rb" "ru" "erb" "gemspec" "god" "mspec")
 ((:icon file_haml :color maroon) "hamlc")
 ((:icon file_haml :color yellow) "haml")
 ((:icon mfizz_haskell :color dark_blue) "lhs")
 ((:icon mfizz_haskell :color dark_purple) "c2hs")
 ((:icon mfizz_haskell :color blue) "hsc")
 ((:icon mfizz_haskell :color purple) "hs")
 ((:icon file_mustache :color orange) "hbs" "handlebars" "mustache")
 ((:icon file_go :color blue) "go")
 ((:icon mfizz_erlang :color dark_green) "yrl")
 ((:icon mfizz_erlang :color green) "xrl")
 ((:icon mfizz_erlang :color maroon) "hrl")
 ((:icon mfizz_erlang :color dark_red) "beam")
 ((:icon mfizz_erlang :color red) "erl")
 ((:icon mfizz_elm :color blue) "elm")
 ((:icon mfizz_elixir :color purple) "exs" "eex")
 ((:icon mfizz_elixir :color dark_purple) "ex")
 ((:icon dev_coffeescript :color blue) "iced")
 ((:icon dev_coffeescript :color light_maroon) "litcoffee")
 ((:icon dev_coffeescript :color dark_maroon) "cjsx")
 ((:icon dev_coffeescript :color maroon) "coffee")
 ((:icon mfizz_clojure :color red) "hic")
 ((:icon mfizz_clojure :color red) "cljx")
 ((:icon mfizz_clojure :color green) "cljc")
 ((:icon mfizz_clojure :color purple) "cl2")
 ((:icon mfizz_clojure :color blue) "clj")
 ((:icon md_vpn_key :color blue) "crt")
 ((:icon md_vpn_key :color orange) "pem")
 ((:icon md_vpn_key :color yellow) "pub")
 ((:icon md_vpn_key :color blue) "key")
 ((:icon dev_bower :color yellow) "bowerrc" "bowerfile")
 ((:icon file_babel :color yellow) "babel" "babelrc" "languagebabel")
 ((:icon file_babel :color dark_yellow) "babelignore")
 ((:icon mfizz_sass :color pink) "scss" "sass")
 ((:icon fa_file_image_o :color yellow) "gif")
 ((:icon fa_file_image_o :color dark_orange) "raw")
 ((:icon fa_file_image_o :color red) "bmp")
 ((:icon fa_file_image_o :color dark_blue) "webp")
 ((:icon fa_file_image_o :color blue) "ico")
 ((:icon fa_file_image_o :color green) "jpg")
 ((:icon fa_file_image_o :color yellow) "gif")
 ((:icon fa_file_image_o :color orange) "png")
 ((:icon file_openoffice :color blue) "odt")
 ((:icon file_word :color blue) "doc" "docx")
 ((:icon mfizz_database_alt2 :color light_pink)  "git")
 ((:icon mfizz_database_alt2 :color pink) "qml")
 ((:icon mfizz_database_alt2 :color maroon) "cson")
 ((:icon mfizz_database_alt2 :color light_red) "yaml" "yml")
 ((:icon mfizz_database_alt2 :color yellow) "json")
 ((:icon fa_file_text_o :color maroon) "log" "journal")
 ((:icon fa_file_text_o :color purple) "srt")
 ((:icon oct_file_binary :color light_blue) "asm" "s" "nasm" "masm")
 ((:icon oct_file_binary :color light_pink) "elf" "elc")
 ((:icon oct_file_binary :color dark_orange) "bin" "bsdiff" "dat" "pak" "pdb")
 ((:icon oct_file_binary :color dark_blue) "objdump" "d-objdump")
 ((:icon oct_file_binary :color dark_purple) "pyc" "pyo")
 ((:icon file_font :color green) "ttf")
 ((:icon file_font :color dark_blue) "woff2")
 ((:icon file_font :color blue) "woff")
 ((:icon file_font :color light_green) "eot")
 ((:icon file_font :color dark_green) "ttc")
 ((:icon file_font :color dark_yellow) "otf")
 ((:icon mfizz_debian :color red) "deb")
 ((:icon oct_package :color light_blue) "bundle")
 ((:icon mfizz_osx :color red) "dmg")
 ((:icon fa_windows :color dark_purple) "exe" "com" "msi" "bat" "cmd" "reg")
 ((:icon md_music_note :color red) "mp3")
 ((:icon md_music_note :color blue) "wma")
 ((:icon md_music_note :color cyan) "m4a")
 ((:icon md_music_note :color dark_red) "flac")
 ((:icon md_music_note :color yellow) "wav")
 ((:icon md_music_note :color dark_cyan) "acc" "ac3" "m4p")
 ((:icon fa_file_pdf_o :color red) "pdf")
 ((:icon mfizz_redhat :color red) "rpm")
 ((:icon mfizz_redhat :color dark_red) "spec")
 ((:icon mfizz_html5 :color orange) "html")
 ((:icon mfizz_css3 :color dark_blue) "less")
 ((:icon mfizz_css3 :color blue) "css")
 ((:icon mfizz_svg :color dark_cyan) "svg")
 ((:icon file_test_js :color orange) "test-js")
 ((:icon mfizz_nodejs :color yellow) "js" "node" "_js" "es6" "es")
 ((:icon oct_terminal :color yellow) "tcsh" "csh")
 ((:icon oct_terminal :color red) "login" "profile" "inputrc")
 ((:icon oct_terminal :color blue) "zsh")
 ((:icon oct_terminal :color dark_yellow) "ksh")
 ((:icon oct_terminal :color dark_purple) "bashrc" "bash_profile")
 ((:icon oct_terminal :color purple) "sh" "rc" "bats" "bash" "tool" "install" "command")
 ((:icon oct_terminal :color green) "fish" "fishrc")
 ((:icon oct_file_zip :color blue) "rar")
 ((:icon file_config :color yellow) "conf" "config" "ini" "desktop" "cfg" "directory" "prefs")
 ((:icon oct_file_zip :color light_orange) "egg")
 ((:icon oct_file_zip :color dark_orange) "xar")
 ((:icon oct_file_zip :color purple) "war")
 ((:icon oct_file_zip :color dark_pink) "jar")
 ((:icon oct_file_zip :color green) "epub")
 ((:icon oct_file_zip :color dark_blue) "whl")
 ((:icon oct_file_zip :color red) "gem")
 ((:icon oct_file_zip :color orange) "xpi")
 ((:icon oct_file_zip :color blue) "iso")
 ((:icon oct_file_zip :color light_maroon) "nzb")
 ((:icon oct_file_zip :color dark_cyan) "bz2")
 ((:icon oct_file_zip :color dark_blue) "tar")
 ((:icon oct_file_zip :color red) "apk")
 ((:icon oct_file_zip :color maroon) "7z")
 ((:icon oct_file_zip :color dark_blue) "tgz" "gz")
 ((:icon oct_file_zip :color dark_red) "zip" "xz" "z")
 ((:icon file_cmake :color green) "cmake")
 ((:icon mfizz_docker :color dark_blue) "dockerfile" "dockerignore")
 ((:icon file_emacs :color purple) "el" "emacs" "spacemacs" "emacs")
 ((:icon mfizz_cplusplus :color blue) "cpp" "c++" "cxx" "cc")
 ((:icon mfizz_cplusplus :color purple) "hh" "hpp" "hxx")
 ((:icon mfizz_c :color blue) "c")
 ((:icon mfizz_c :color purple) "h")
 ((:icon file_powerpoint :color light_pink) "ppt" "pps" "ppsx" "pptx")
 ((:icon fa_file_text_o :color blue) "txt" "text")
 ((:icon oct_database :color green) "cache")
 )

(--add-to-suffix
 ((:icon mfizz_html5 :color red) "html.erb")
 ((:icon file_test_js :color orange) "test.js" "test.node" "test._js" "test.es6" "test.es")
 ((:icon dev_bower :color yellow) "bower.json")
 ((:icon dev_coffeescript :color red) "coffee.erb")
 ((:icon dev_coffeescript :color cyan) "coffee.ecr")
 ((:icon mfizz_erlang :color dark_maroon) "app.src")
 ((:icon file_haml :color red) "haml.deface")
 ((:icon mfizz_reactjs :color blue) "react.js")
 )

(--add-to-full
 ((:icon file_tag :color blue) "CTAGS" "TAGS")
 ((:icon file_tag :color blue) "ctags" "tags")
 ((:icon oct_file_binary :color dark_green) "a.out")
 ((:icon oct_package :color green) "*Packages*")
 ((:icon fa_folder :color dark_blue) "." "..")
 ((:icon oct_bookmark :color light_pink) "bookmark")
 ((:icon file_cmake :color red) "CMakeLists.txt")
 ((:icon oct_terminal :color red) "depcomp" "libtool" "compile")
 ((:icon oct_terminal :color red) "configure" "config.guess" "config.rpath" "config.status" "config.sub" "bootstrap")
 ((:icon file_emacs :color purple) ".emacs" ".spacemacs")
 ((:icon file_emacs :color dark_purple) ".emacs.d")
 ((:icon md_vpn_key :color red) "id_rsa")
 ((:icon mfizz_database_alt2 :color silver) "HEAD" "ORIG_HEAD" "FETCH_HEAD" "packed-refs")
 ((:icon mfizz_docker :color dark_orange) "docker-sync")
 ((:icon mfizz_docker :color dark_blue) "Dockerfile" "docker-compose")
 ((:icon mfizz_elixir :color light_purple) "mix.ex" "mix.exs" "mix.lock")
 ((:icon mfizz_erlang :color red) "rebar.config.lock" "rebar.lock")
 ((:icon mfizz_erlang :color dark_green) "Emakefile")
 ((:icon mfizz_gulp :color maroon) "gulpfile.coffee")
 ((:icon mfizz_gulp :color red) "gulpfile.js" "gulpfile.babel.js")
 ((:icon mfizz_grunt :color maroon) "gruntfile.coffee")
 ((:icon mfizz_grunt :color yellow) "gruntfile.js")
 ((:icon mfizz_ruby :color red) "irbrc" "gemrc" "pryrc" "ruby-gemset" "ruby-version")
 ((:icon mfizz_ruby :color red) "rails")
 ((:icon oct_book :color yellow) "LICENSE" "license")
 ((:icon oct_book :color dark_blue) "NEWS" "news")
 ((:icon oct_book :color blue) "CHANGELOG" "ChangeLog" "changelog")
 ((:icon oct_book :color dark_blue) "THANKS" "thanks")
 ((:icon oct_book :color dark_blue) "MANIFEST" "manifest")
 ((:icon oct_book :color dark_blue) "MAINTAINERS" "maintainers")
 ((:icon oct_book :color dark_blue) "INSTALL")
 ((:icon oct_book :color dark_blue) "HISTORY")
 ((:icon oct_book :color light_blue) "AUTHORS")
 ((:icon oct_book :color dark_blue) "HACKING" "hacking")
 ((:icon oct_book :color dark_blue) "COPYING" "copying")
 ((:icon oct_book :color dark_blue) "CONTRIBUTORS" "contributors")
 ((:icon oct_book :color dark_blue) "CONTRIBUTING" "contributing")
 ((:icon oct_book :color dark_blue) "CONTRIBUTE" "contribute")
 ((:icon oct_book :color dark_blue) "CHANGES" "changes")
 ((:icon oct_book :color dark_blue) "BUGS" "bugs")
 ((:icon oct_book :color dark_blue) "NOTICE" "notice")
 ((:icon oct_book :color green) "README" "readme")
 ((:icon oct_checklist :color yellow) "TODO")
 )

(--add-to-prefix
 ((:icon file_diff :color green) "*magit-diff:")
 ((:icon fa_git :color green) "*magit:")
 ((:icon md_vpn_key :color red) "id_rsa")
 ((:icon oct_book :color dark_blue) "THANKS." "thanks.")
 ((:icon oct_book :color dark_blue) "THANKS-" "thanks-")
 ((:icon file_php :color dark_green) "Phakefile")
 ((:icon file_webpack :color blue) "webpack.")
 )

(defun sidebar-filemapping-getcolor (color)
  "COLOR."
  (car (alist-get color sidebar-filemapping-gui-color)))

(defun sidebar-filemapping-put-in-cache (plist file)
  "PLIST FILE."
  (puthash file plist sidebar-filemapping-cache-hashtable)
  plist)

(defun sidebar-filemapping-get-in-cache (filename)
  "FILENAME."
  (when (not sidebar-filemapping-cache-hashtable)
    (setq sidebar-filemapping-cache-hashtable (make-hash-table :test 'equal :size 200)))
  (gethash filename sidebar-filemapping-cache-hashtable))

(defun sidebar-filemapping-find-prefix (filename)
  "FILENAME."
  (catch 'stop-map
    (maphash (lambda (key val)
	       (when (s-starts-with? key filename)
		 (throw 'stop-map val)))
	     sidebar-filemapping-prefix-hashtable)))

(defun sidebar-filemapping-find-suffix (filename)
  "FILENAME."
  (catch 'stop-map
    (maphash (lambda (key val)
	       (when (s-ends-with? key filename)
		 (throw 'stop-map val)))
	     sidebar-filemapping-suffix-hashtable)))

(defun sidebar-filemapping-emacs-buffers (filename)
  "FILENAME."
  (let ((c (elt "*" 0)))
    (when (and (eq (elt filename 0) c)
	       (eq (elt filename (1- (length filename))) c))
      '(:icon file_emacs :color blue_grey))))

(defun sidebar-filemapping-dotfile (filename)
  "FILENAME."
  (let ((c (elt "." 0)))
    (when (eq (elt filename 0) c)
      '(:icon oct_gear))))

(defun sidebar-filemapping-lookup (filename)
  "FILENAME."
  (or (sidebar-filemapping-get-in-cache filename)
      (sidebar-filemapping-put-in-cache
       (-let (((&plist :icon icon :color color)
	       (or (gethash filename sidebar-filemapping-full-hashtable)
		   (sidebar-filemapping-find-prefix filename)
		   (sidebar-filemapping-find-suffix filename)
		   (gethash (file-name-extension filename) sidebar-filemapping-extension-hashtable)
		   (sidebar-filemapping-emacs-buffers filename)
		   (sidebar-filemapping-dotfile filename)
		   '(:icon fa_file_o))))
	 `(:icon ,icon :color ,(sidebar-filemapping-getcolor color)))
       filename)))

(provide 'sidebar-filemapping)

;;; sidebar-filemapping.el ends here
