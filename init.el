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
(setq user-defaults-file "~/.config/emacs/defaults.el")
(if (file-exists-p user-defaults-file) (load user-defaults-file 'noerror))
;;;;; This is my ~/.config/emacs/defaults.el
;; (setq user-org-roam-directory "~/Documents/Emacs/RoamNotes")
;; (setq user-org-gtd-directory  "~/Documents/Emacs/GTD")

;; Generate README.el and perform configurations from it
(org-babel-load-file "~/.emacs.d/README.org")

;; Load custom settings
(setq custom-file (locate-user-emacs-file "~/.config/emacs/custom.el"))
(load custom-file 'noerror)
;;;;; This is my ~/.config/emacs/custom.el
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(column-number-mode t)
;;  '(cua-mode t nil (cua-base))
;;  '(delete-selection-mode nil)
;;  '(global-hl-line-mode 1)
;;  '(lsp-verify-signature nil)
;;  '(markdown-command "pandoc")
;;  '(mouse-wheel-follow-mouse t)
;;  '(mouse-wheel-mode t)
;;  '(mouse-wheel-progressive-speed t)
;;  '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((meta)) ((control) . text-scale)))
;;  '(rg-enable-default-bindings nil)
;;  '(scroll-step 1)
;;  '(warning-suppress-types '((lsp-mode) (lsp-mode) (lsp-mode) (lsp-mode) (comp))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight normal :height 90 :width normal))))
;;  '(hydra-posframe-border-face ((t (:background "#bf616a"))))
;;  '(hydra-posframe-face ((t (:background "#3b4252"))))
;;  '(lsp-ui-doc-background ((t (:background nil))))
;;  '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))))

;; preferences
(put 'downcase-word 'disabled nil)
(put 'upcase-word 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
