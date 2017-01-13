;; render README.el and runs it
(org-babel-load-file "~/.emacs.d/README.org")

;; runs configuration for org files, if any.
;; see: http://cestlaz.github.io/posts/using-emacs-26-gcal
(mapcar 'org-babel-load-file
  (directory-files "~/Documents/orgfiles/config" t ".+\.org"))
