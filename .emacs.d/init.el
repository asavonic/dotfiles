``/col;; My name and e-mail adress
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

;; Dired
;; (require-package "dired")
;; (require 'dired)
;; (setq dired-recursive-deletes 'top) ;; allows to delete not empty directories

;; Imenu
(require 'imenu)
(setq imenu-auto-rescan t) ;; auto update
(setq imenu-use-popup-menu nil) ;; show dialogs only in minibuffer
(global-set-key (kbd "<f4>") 'imenu) ;; hotkey Imenu to F4

;; Display the name of the current buffer in the title bar
(setq frame-title-format "GNU Emacs: %b")

;; Load path for plugins
(if (system-is-windows)
  (add-to-list 'load-path win-init-path)
  (add-to-list 'load-path unix-init-path))

;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)
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

;; IDO plugin
;; (require 'ido)
;; (ido-mode                      t)
;; (icomplete-mode                t)
;; (ido-everywhere                t)
;; tq ido-vitrual-buffers      t)
;; tq ido-enable-flex-matching t)

;; Buffer Selection and ibuffer settings
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ;; buffer list on C-x C-b
(global-set-key (kbd "<f2>") 'bs-show) ;; hotkey buffer selection F2

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'gruvbox t)

;; Color-theme definition
;; (defun color-theme-init()
;;  (color-theme-initialize)
;;  (setq color-theme-is-global t)
;;  ;; (gruvbox)
;;  ;; (gruvbox-color-theme)
;; )

;; (if (system-is-windows)
;;  (when (file-directory-p win-init-ct-path)
;;      (add-to-list 'load-path win-init-ct-path)
;;      (color-theme-init))
;;  (when (file-directory-p unix-init-ct-path)
;;      (add-to-list 'load-path unix-init-ct-path)
;;      (color-theme-init)))

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
(require 'evil)
(evil-mode 1)

;; NyanMacs
(require 'nyan-mode)
(nyan-mode)

;; GDB debugging
(setq gdb-many-windows t) ;; use gdb-many-windows by default
;; Non-nil means display source file containing the main routine at startup
(setq gdb-show-main t)

;; Helm
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(require 'cc-mode)
(require 'cmake-mode)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

(require 'cedet)

;; function-args
(require 'function-args)
(fa-config-default)
(define-key c-mode-map [(tab)] 'moo-complete)
(define-key c++-mode-map [(tab)] 'moo-complete)
;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
(define-key c-mode-map [(control tab)] 'company-complete)
(define-key c++-mode-map [(control tab)] 'company-complete)
;; company-c-headers
(add-to-list 'company-backends 'company-c-headers)
;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; use helm with company
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))


;; Function args
;; (require 'function-args)
;; (fa-config-default)
;; (define-key c-mode-map  [C-Tab] 'moo-complete)
;; (define-key c++-mode-map  [C-Tab] 'moo-complete)
;; (define-key c-mode-map (kbd "M-o")  'fa-show)
;; (define-key c++-mode-map (kbd "M-o")  'fa-show)

;; Org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
(setq helm-dash-docsets-path "/localdisk/doc/dash")
(setq helm-dash-common-docsets '("C++")) 
(setq helm-dash-docsets-url "http://raw.github.com/Kapeli/feeds/master")

;; session managment
(require 'session)
    (add-hook 'after-init-hook 'session-initialize)

(when (require 'session nil t)
  (add-hook 'after-init-hook 'session-initialize)
  (add-to-list 'session-globals-exclude 'org-mark-ring))

;; expanded folded secitons as required
(defun le::maybe-reveal ()
  (when (and (or (memq major-mode  '(org-mode outline-mode))
                 (and (boundp 'outline-minor-mode)
                      outline-minor-mode))
             (outline-invisible-p))
    (if (eq major-mode 'org-mode)
        (org-reveal)
      (show-subtree))))

(add-hook 'session-after-jump-to-last-change-hook
          'le::maybe-reveal)

;;; Local session.
;;  It will search “.emacs.session” file in the directory where you start Emacs
(unless (daemonp)
  (custom-set-variables '(session-save-file ".emacs.session"))

  (let ((local-session (concat default-directory session-save-file)))
    (if (file-exists-p local-session)
        (progn
          (custom-set-variables '(session-save-file local-session))
          (message (concat "Local session file set to \"" session-save-file "\".")))
      (custom-set-variables '(session-save-file (concat "~/" session-save-file))))))

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Firefox as default web-browser
(setq browse-url-browser-function 'browse-url-firefox)

;; Projectile
(projectile-global-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(require 'helm-projectile)
(helm-projectile-on)

(persp-mode)
(require 'persp-projectile)

;; backups
(setq backup-directory-alist `(("." . "~/.emacs_backup")))

;; magit setup
(global-set-key (kbd "C-x g") 'magit-status) ;; hotkey to use magit
(provide 'init)
;;; init.el ends here
