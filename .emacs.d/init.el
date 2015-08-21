(setq user-full-name "Andrew Savonichev")
(setq user-mail-address "andrew.savonichev@gmail.com")

(setq homedir (getenv "HOME"))
(add-to-list 'load-path (concat homedir "/.emacs.d"))
(add-to-list 'load-path (concat homedir "/.emacs.d/appearance"))

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/")
  t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; set initial screen to *scratch* buffer with org-mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; disable unused gui elements
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; tabs vs spaces holywar: spaces won for me :)
(setq tab-width 4
      indent-tabs-mode nil)

;; disable backup files
(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(dolist (key '("\C-z"))
  (global-unset-key key))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)

(setq show-paren-delay 0)
(show-paren-mode t)
(use-package smartparens
    :ensure t
    :config (require 'smartparens-config)
            (smartparens-global-mode 1))

(setq-default show-trailing-whitespace t)

(global-linum-mode t)

(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching 1)

(use-package ace-jump-mode
    :ensure t
    :bind ("M-z" . ace-jump-mode))

(require 'asavonic-appearance)
(require 'my-mail)
(require 'functions)

(add-hook 'cc-mode-hook
      (lambda ()
          (local-unset-key (kbd "C-c C-l"))))

(global-set-key (kbd "C-c C-l") 'run-custom-command)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package dired+
    :ensure t)

;; following 4 plugin configs: https://github.com/tuhdo/emacs-c-ide-demo
(use-package ggtags
    :ensure t
    :config (add-hook 'c-mode-common-hook
		      (lambda ()
			  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
			      (ggtags-mode 1))))
            (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
	    (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
	    (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
	    (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
	    (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
	    (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
	    (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))

(use-package helm
    :ensure t
    :config (require 'helm-grep)
	    (global-set-key (kbd "M-x") 'helm-M-x)
	    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
	    (global-set-key (kbd "C-x b") 'helm-mini)
	    (global-set-key (kbd "C-x C-f") 'helm-find-files)
	    (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
	    (global-set-key (kbd "C-c h o") 'helm-occur)

	    (add-hook 'eshell-mode-hook
		      #'(lambda ()
			    (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))
	    ;;; Save current position to mark ring
	    (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

	    ;; show minibuffer history with Helm
	    (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
	    (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history))

(use-package helm-gtags
    :ensure t
    :config (setq helm-gtags-ignore-case t
		  helm-gtags-auto-update t
		  helm-gtags-use-input-at-cursor t
		  helm-gtags-pulse-at-cursor t
		  helm-gtags-prefix-key "\C-cg"
		  helm-gtags-suggested-key-mapping t)
            (add-hook 'dired-mode-hook 'helm-gtags-mode)
	    (add-hook 'eshell-mode-hook 'helm-gtags-mode)
	    (add-hook 'c-mode-hook 'helm-gtags-mode)
	    (add-hook 'c++-mode-hook 'helm-gtags-mode)
	    (add-hook 'java-mode-hook 'helm-gtags-mode)
	    (add-hook 'asm-mode-hook 'helm-gtags-mode)
	    (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
	    (define-key helm-gtags-mode-map (kbd "C-C C-j") 'helm-gtags-select)
	    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
	    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
	    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
	    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))

(use-package helm-swoop
    :ensure t
    :config (global-set-key (kbd "C-c h o") 'helm-swoop)
	    (global-set-key (kbd "C-c s") 'helm-multi-swoop-all)

	    ;; When doing isearch, hand the word over to helm-swoop
	    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

	    ;; From helm-swoop to helm-multi-swoop-all
	    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
	    ;; Save buffer when helm-multi-swoop-edit complete
	    (setq helm-multi-swoop-edit-save t)

	    ;; If this value is t, split window inside the current window
	    (setq helm-swoop-split-with-multiple-windows t)

	    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
	    (setq helm-swoop-split-direction 'split-window-vertically)

	    ;; If nil, you can slightly boost invoke speed in exchange for text color
	    (setq helm-swoop-speed-or-color t))

(use-package ag
    :ensure t)

(use-package shell-switcher
    :ensure t
    :config (setq shell-switcher-mode t))

(use-package company
    :ensure t
    :config (add-hook 'after-init-hook 'global-company-mode)
            (setq company-idle-delay 0))

(use-package elpy
    :ensure t
    :config (elpy-enable))

;; Use M-n M-p to move to next or prev occurence of symbol at point
;; Use M-' to replace all symbols in file, C-u M-' to replace them only in current function
(use-package smartscan
  :ensure t
  :config (smartscan-mode 1)
          (global-set-key (kbd "M-n") 'smartscan-symbol-go-forward)
	  (global-set-key (kbd "M-p") 'smartscan-symbol-go-backward)
	  (global-set-key (kbd "M-'") 'smartscan-symbol-replace))

(use-package lua-mode
  :ensure t
  :config (setq lua-indent-level 4
                lua-indent-string-contents t))
(use-package cmake-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :config (add-hook 'after-init-hook #'global-flycheck-mode))

(setq compilation-skip-threshold 2
      compilation-auto-jump-to-first-error t
      compilation-scroll-output t)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq c-default-style "linux"
      c-basic-offset 4)


(use-package org
  :ensure t
  :config (setq org-log-done t)
          (setq org-agenda-files (list "~/org/teapod.org"
				       "~/org/wiki.org"
				       "~/org/common.org")))
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(use-package blank-mode
  :ensure t)

(provide 'init)
;;; init.el ends here
