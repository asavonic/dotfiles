;; My name and e-mail adress

;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("org" . "http://orgmode.org/elpa/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ;; ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")
			 ))
(package-initialize)
(setq package-check-signature nil)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

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


;; Indent settings
(setq-default indent-tabs-mode nil) 
(setq-default tab-width          4) 
(setq-default c-basic-offset     4)
(setq-default standart-indent    4) 
(setq-default lisp-body-indent   4) 
(setq lisp-indent-function  'common-lisp-indent-function)
(electric-indent-mode)

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

(use-package rainbow-delimiters :ensure t)

;; GDB debugging
(setq gdb-many-windows t) ;; use gdb-many-windows by default
;; Non-nil means display source file containing the main routine at startup
(setq gdb-show-main t)

;; Helm
(use-package helm
    :ensure t
    :config
        (helm-mode 1)
        (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

    :init
        (setq helm-split-window-in-side-p         t ; open helm buffer inside current window, not occupy whole other window
            helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
            helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
            helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
            helm-ff-file-name-history-use-recentf t))




(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))


;; (use-package semantic
;;     :ensure t
;;     :config 
;;         (global-semanticdb-minor-mode 1)
;;         (global-semantic-idle-scheduler-mode 1)
;;         (semantic-mode 1))


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
        (setq helm-dash-docsets-url "http://raw.github.com/Kapeli/feeds/master"))

;; session managment
(use-package session
    :ensure t
    :init
        (session-initialize))

;; Firefox as default web-browser
(setq browse-url-browser-function 'browse-url-firefox)

;; backups
(setq backup-directory-alist `(("." . "~/.emacs_backup")))

(use-package load-relative :ensure t)

(load-relative "init/functions")
(load-relative "init/paths")
(load-relative "init/personal")
(load-relative "init/appearance")
(load-relative "init/platform")
(load-relative "init/dev")

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (gruvbox)))
 '(custom-safe-themes
   (quote
    ("a313c0909db21686da58bbbb26076d85976a30eb47254dd454e21f26e462668a" default)))
 '(org-agenda-files nil)
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
