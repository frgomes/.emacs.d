;; Make sure custom configurations NEVER pollute init.el
;; note: custom.el is ignored in .gitignore
(setq custom-file "~/.emacs.d/custom.el")

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Generate README.el and perform configurations from it
(org-babel-load-file "~/.emacs.d/README.org")
