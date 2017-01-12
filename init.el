;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;;----------------------------------------------------------------------------------------------------------------------------------

;; Package repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Fetch list of available packages
(package-initialize) 
(unless package-archive-contents (package-refresh-contents))

;; List the package we want
(setq package-list '(use-package diminish bind-key
		     projectile undo-tree highlight-symbol goto-chg company yasnippet yatemplate smartparens helm
		     neotree magit monky gist find-file-in-repository 
                     multiple-cursors window-numbering expand-region
		     monokai-theme
                     ;ensime lua-mode magit gist
                     ;markdown-mode markdown-preview-eww 
                     ;slime yafolding dumb-jump ag
		     ;rainbow-delimiters 
))

;; Install the packages that are missing, if any
(dolist (package package-list)
	(unless (package-installed-p package)
		(package-install package)))

;; Wire use-package https://github.com/jwiegley/use-package
(eval-when-compile (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;;----------------------------------------------------------------------------------------------------------------------------------

(use-package projectile
  :demand
  :init   (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  :bind   (("s-f" . projectile-find-file)
           ("s-F" . projectile-grep)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("s-/" . undo-tree-visualize))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("s-h" . highlight-symbol))

(use-package goto-chg
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package company
  :diminish company-mode
  :commands company-mode
  :init
    (setq
     company-dabbrev-ignore-case nil
     company-dabbrev-code-ignore-case nil
     company-dabbrev-downcase nil
     company-idle-delay 0
     company-minimum-prefix-length 4)
  :config
    ;; disables TAB in company-mode, freeing it for yasnippet
    (define-key company-active-map [tab] nil)
    (define-key company-active-map (kbd "TAB") nil))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))

(use-package yatemplate
  :defer 2 ;; WORKAROUND https://github.com/mineo/yatemplate/issues/3
  :config
    (auto-insert-mode)
    (setq auto-insert-alist nil)
    (yatemplate-fill-alist))

(use-package smartparens
  :diminish smartparens-mode
  :commands
    smartparens-strict-mode
    smartparens-mode
    sp-restrict-to-pairs-interactive
    sp-local-pair
  :init (setq sp-interactive-dwim t)
  :config
    (require 'smartparens-config)
    (sp-use-smartparens-bindings)
    (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
    (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
    (sp-pair "{" "}" :wrap "C-{")
    ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
    (bind-key "C-<left>"  nil smartparens-mode-map)
    (bind-key "C-<right>" nil smartparens-mode-map)
    (bind-key "s-<delete>"    'sp-kill-sexp smartparens-mode-map)
    (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))

(use-package helm
  :bind (("M-x"     . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

(use-package neotree
  :init (neotree)
  :bind ("s-d" . neotree-toggle))

(use-package magit
  :config (setq magit-last-seen-setup-instructions "1.4.0")
  :bind ("M-s M-g" . magit-status))

(use-package monky
  :init (setq monky-process-type 'cmdserver)
  :bind ("M-s M-m" . monky-status))

(use-package gist
  :bind (("M-s M-o" . gist-list)
	 ("M-s M-s" . gist-region-or-buffer)))
  
(use-package find-file-in-repository
  :bind ("M-s M-f" . find-file-in-repository))

(use-package multiple-cursors)

(use-package window-numbering
  :init (window-numbering-mode 1))

(use-package expand-region
  :bind ("C-=" . er/expand-region))
  
(use-package monokai-theme
  :config (load-theme 'monokai t)
  :init (setq frame-background-mode 'dark))

;;----------------------------------------------------------------------------------------------------------------------------------



;(require 'rainbow-delimiters)
;(add-hook 'scala-mode-hook #'rainbow-delimiters-mode)
;(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;(add-hook 'scala-mode-hook #'smartparens-mode)
;(add-hook 'typescript-mode-hook #'smartparens-mode)
;(add-hook 'js-mode-hook #'smartparens-mode)

