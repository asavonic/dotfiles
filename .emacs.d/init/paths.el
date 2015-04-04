;; MS Windows path-variable
(when (system-is-windows)
    (setq sbcl-exe              "C:/sbcl/sbcl.exe")
    (setq init-path         "C:/.emacs.d/init")
    (setq init-slime-path   "C:/.emacs.d/slime"))

;; Unix path-variable
(when (system-is-linux)
    (setq sbcl-exe          "/usr/bin/sbcl")
    (setq init-path         "~/.emacs.d/init")
    (setq init-slime-path   "/usr/share/common-lisp/source/slime/"))
