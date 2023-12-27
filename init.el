;;; init.el --- -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initialization script reads configuration from README.org and
;; obtains custom configuration from user directories under $HOME/.config/emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond ((version< emacs-version "26.1")
       (warn "Emacs 26.1 or above is required!"))
      ((let* ((early-init-f (expand-file-name "early-init.el" user-emacs-directory))
              (early-init-do-not-edit-d (expand-file-name "early-init-do-not-edit/" user-emacs-directory))
              (early-init-do-not-edit-f (expand-file-name "early-init.el" early-init-do-not-edit-d)))
         (and (version< emacs-version "27")
              (or (not (file-exists-p early-init-do-not-edit-f))
                  (file-newer-than-file-p early-init-f early-init-do-not-edit-f)))
         (make-directory early-init-do-not-edit-d t)
         (copy-file early-init-f early-init-do-not-edit-f t t t t)
         (add-to-list 'load-path early-init-do-not-edit-d)
         (require 'early-init))))

;; Load defaults, if any
;; You probably don't need to define any defaults.
;; In any case, we allow you to do so before the configuration starts.
(setq user-defaults-file "~/.config/emacs/defaults.el")
(if (file-exists-p user-defaults-file) (load user-defaults-file 'noerror))

;; Generate README.el and perform configurations from it.
;; This is where all configuraiotn magic happens.
(org-babel-load-file "~/.emacs.d/README.org")

;; Load custom settings
;; These are configurations managed automagically by Emacs.
;; DO NOT EDIT these custom configurations by hand, since as said: Emacs manages it.
(setq custom-file (locate-user-emacs-file "~/.config/emacs/custom.el"))
(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here
