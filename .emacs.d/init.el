(setq user-full-name "Andrew Savonichev")
(setq user-mail-address "andrew.savonichev@gmail.com")

(setq homedir (getenv "HOME"))
(add-to-list 'load-path (concat homedir "/.emacs.d/"))
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

;; almost like vim navigation
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-k") 'previous-line)

(use-package evil
    :ensure t
    :config (evil-mode 1))

(require 'asavonic-appearance)
(require 'functions)

(add-hook 'cc-mode-hook
      (lambda ()
          (local-unset-key (kbd "C-c C-l"))))

(global-set-key (kbd "C-c C-l") 'run-custom-command)
(put 'downcase-region 'disabled nil)
