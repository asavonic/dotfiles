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

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'gruvbox t)

;; Syntax highlighting
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
