;; My name and e-mail adress
(setq user-full-name   "Andrew Savonichev")
(setq user-mail-adress "andrew.savonichev@gmail.com")

;; System-type definition
(defun system-is-linux()
  (string-equal system-type "gnu/linux"))

(defun system-is-windows()
  (string-equal system-type "windows-nt"))

;; MS Windows path-variable
(when (system-is-windows)
    (setq win-sbcl-exe          "C:/sbcl/sbcl.exe")
    (setq win-init-path         "C:/.emacs.d/init")
    (setq win-init-ct-path      "C:/.emacs.d/plugins/color-theme")
    (setq win-init-ac-path      "C:/.emacs.d/plugins/auto-complete")
    (setq win-init-slime-path   "C:/slime")
    (setq win-init-ac-dict-path "C:/.emacs.d/plugins/auto-complete/dict"))

;; Unix path-variable
(when (system-is-linux)
    (setq unix-sbcl-bin          "/usr/bin/sbcl")
    (setq unix-init-path         "~/.emacs.d/init")
    (setq unix-init-ct-path      "~/.emacs.d/plugins/color-theme")
    (setq unix-init-ac-path      "~/.emacs.d/plugins/auto-complete")
    (setq unix-init-slime-path   "/usr/share/common-lisp/source/slime/")
    (setq unix-init-ac-dict-path "~/.emacs.d/plugins/auto-complete/dict"))

;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("org" . "http://orgmode.org/elpa/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ;; ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ))
(package-initialize)
(setq package-check-signature nil)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

;; set default font in initial window and for any new window
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))
    )
  )
 ((string-equal system-type "darwin")   ; Mac OS X
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))
  )
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))
  )
 )

;; Display the name of the current buffer in the title bar
(setq frame-title-format "GNU Emacs: %b")

;; Load path for plugins
(if (system-is-windows)
  (add-to-list 'load-path win-init-path)
  (add-to-list 'load-path unix-init-path))

;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-files (quote ("~/org")))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-agenda-files (list "~/org/tickets.org"
                             "~/org/validation.org" 
                             "~/org/env.org"))

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t) ;; call explisit with C-h C-a

;; Electric-modes settings
(electric-pair-mode    1) ;; autoclose () {} []
(electric-indent-mode -1) ;; disable auto intendation


;; Delete selection
(delete-selection-mode t) ;; allows to delete selected text inserting new one

;; Disable GUI components
(tooltip-mode      -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(blink-cursor-mode -1) 
(setq use-dialog-box     nil)
(setq redisplay-dont-pause t)
(setq ring-bell-function 'ignore)

;; Coding-system settings
(set-language-environment 'UTF-8)
(if (system-is-linux) ;; for GNU/Linux use utf-8, for MS Windows - windows-1251
    (progn
        (setq default-buffer-file-coding-system 'utf-8)
        (setq-default coding-system-for-read    'utf-8)
        (setq file-name-coding-system           'utf-8)
        (set-selection-coding-system            'utf-8)
        (set-keyboard-coding-system        'utf-8-unix)
        (set-terminal-coding-system             'utf-8)
        (prefer-coding-system                   'utf-8))
    (progn
        (prefer-coding-system                   'windows-1251)
        (set-terminal-coding-system             'windows-1251)
        (set-keyboard-coding-system        'windows-1251-unix)
        (set-selection-coding-system            'windows-1251)
        (setq file-name-coding-system           'windows-1251)
        (setq-default coding-system-for-read    'windows-1251)
        (setq default-buffer-file-coding-system 'windows-1251)))

;; Linum plugin
(require 'linum)
(line-number-mode   t) 
(global-linum-mode  t) 
(column-number-mode t) 
(setq linum-format " %d")

;; Fringe settings
;; (fringe-mode '(8 . 0)) ;;
;; (setq-default indicate-empty-lines t)
;; (setq-default indicate-buffer-boundaries 'left)

;; Display file size/time in mode-line
(setq display-time-24hr-format t) ;; 24-hours format in mode-line
(display-time-mode             t) ;; show time in mode-line
(size-indication-mode          t) ;; file %

;; Line wrapping
(setq word-wrap          t)
(global-visual-line-mode t)

;; Buffer Selection and ibuffer settings
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ;; buffer list on C-x C-b
(global-set-key (kbd "<f2>") 'bs-show) ;; hotkey buffer selection F2

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'gruvbox t)


;; Syntax highlighting
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Indent settings
(setq-default indent-tabs-mode nil) 
(setq-default tab-width          4) 
(setq-default c-basic-offset     4)
(setq-default standart-indent    4) 
(setq-default lisp-body-indent   4) 
(global-set-key (kbd "RET") 'newline-and-indent) 
(setq lisp-indent-function  'common-lisp-indent-function)

;; Scrolling settings
(setq scroll-step 1) 
;; (setq scroll-margin 10)
(setq scroll-conservatively 10000)


;; Short messages in minibuf
(defalias 'yes-or-no-p 'y-or-n-p)


;; use shared X clipboard settings
(setq x-select-enable-clipboard t)

;; Highlight search resaults
(setq search-highlight        t)
(setq query-replace-highlight t)

;; Easy transition between buffers: M-arrow-keys
(if (equal nil (equal major-mode 'org-mode))
    (windmove-default-keybindings 'meta))

;; Bookmark settings
(require 'bookmark)
(setq bookmark-save-flag t) ;; autosave to file
(when (file-exists-p (concat user-emacs-directory "bookmarks"))
    (bookmark-load bookmark-default-file t))
(global-set-key (kbd "<f9>") 'bookmark-set) ;; hotkey create bookmark F9
(global-set-key (kbd "<f10>") 'bookmark-jump) ;; hotkey jump to bookmark F10
(global-set-key (kbd "<f11>") 'bookmark-bmenu-list) ;; hotkey open bookmark list F11
(setq bookmark-default-file (concat user-emacs-directory "bookmarks")) ;; save to .emacs.d/bookmarks

;; evil vimmer
(use-package evil
    :ensure t
    :config (evil-mode 1))

;; NyanMacs
(use-package nyan-mode
    :ensure t
    :config (nyan-mode))

;; GDB debugging
(setq gdb-many-windows t) ;; use gdb-many-windows by default
;; Non-nil means display source file containing the main routine at startup
(setq gdb-show-main t)

;; Helm
(use-package helm
    :ensure t
    :bind 
        (("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
         ("C-i"   . helm-execute-persistent-action) ; make TAB works in terminal
         ("C-z"   . helm-select-action))            ; list actions using C-z
    :config
        ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
        ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
        ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
        (global-set-key (kbd "C-c h") 'helm-command-prefix)
        (global-unset-key (kbd "C-x c"))

        ;; (use-package helm-config :ensure t)
        (helm-mode 1)

    :init
        (setq helm-split-window-in-side-p         t ; open helm buffer inside current window, not occupy whole other window
            helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
            helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
            helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
            helm-ff-file-name-history-use-recentf t))




(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))


(use-package cc-mode)

(use-package cmake-mode
    :ensure t
    :mode "\\(CMakeLists\\.txt\\'\\|\\.cmake\\'\\)")

(use-package semantic
    :ensure t
    :config 
        (global-semanticdb-minor-mode 1)
        (global-semantic-idle-scheduler-mode 1)
        (semantic-mode 1))


;; Org-mode
(use-package org
    :ensure t
    :mode ("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode)
    :init
        (setq org-log-done t)
    :config
        (custom-set-faces)
    :bind
        (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)))


;; compilation
(setq compilation-scroll-output t)
(defun cc-goto-first-error( buffer exit-condition )
    (with-current-buffer buffer
                             (goto-char (point-min))
                                 (compilation-next-error 1)))

(add-to-list 'compilation-finish-functions 'cc-goto-first-error)
(global-set-key (kbd "<f5>") 'compile) ;; hotkey

(eval-after-load 'compile
  '(progn (make-variable-buffer-local 'compile-command)
          (make-variable-buffer-local 'compile-history)))

;; helm-dash
(use-package helm-dash
    :ensure t
    :init 
        (setq helm-dash-docsets-path "/localdisk/doc/dash")
        (setq helm-dash-common-docsets '("C++")) 
        (setq helm-dash-docsets-url "http://raw.github.com/Kapeli/feeds/master"))

;; session managment
(use-package session
    :ensure t
    :init
        (session-initialize))

(use-package flycheck
    :ensure t
    :init (global-flycheck-mode))

;; Firefox as default web-browser
(setq browse-url-browser-function 'browse-url-firefox)

;; Projectile
(use-package projectile
    :ensure t
    :init
        (setq projectile-indexing-method 'alien)
        (setq projectile-enable-caching t)
    :config
        (projectile-global-mode)
        (use-package helm-projectile
            :ensure t
            :config
                (helm-projectile-on)))

(use-package perspective
    :ensure t
    :config
        (persp-mode)
        (use-package persp-projectile :ensure t))

;; backups
(setq backup-directory-alist `(("." . "~/.emacs_backup")))

;; magit setup
(use-package magit
    :ensure t
    :bind ("C-x g" . magit-status))

(provide 'init)
;;; init.el ends here
