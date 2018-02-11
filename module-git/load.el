
(load "/home/sebastien/travaux/sidebar.el/module-git/target/release/libsidebar_git")

;; (my-module-func "a")
;; (my-module-open-dir "coucou")

;; (measure-time
;;  (my-module-open-dir "/home/sebastien/travaux/sample"))
;; (measure-time
;;  (my-module-open-dir "/home/sebastien/github/rust"))

;; (my-module-open-dir "coucou" "toi")
;; (my-module-open-dir)

(measure-time (sidebar-git-test "/home/sebastien/github/servo"))
(sidebar-git-head-upstream "/home/sebastien/github/servo")
(sidebar-git-head-upstream "/home/sebastien/github/rls")

default-directory
