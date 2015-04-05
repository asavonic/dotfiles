(use-package cc-mode)

(use-package cmake-mode
    :ensure t
    :mode "\\(CMakeLists\\.txt\\'\\|\\.cmake\\'\\)")

(use-package irony
    :ensure t
    :config
        (add-hook 'c++-mode-hook 'irony-mode)
        (add-hook 'c-mode-hook 'irony-mode)
        (add-hook 'objc-mode-hook 'irony-mode)

        ;; replace the `completion-at-point' and `complete-symbol' bindings in
        ;; irony-mode's buffers by irony-mode's function
        (defun my-irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point]
            'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol]
            'irony-completion-at-point-async))

        (add-hook 'irony-mode-hook 'my-irony-mode-hook)
        (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

        (use-package company-irony
            :ensure t
            :config
                (eval-after-load 'company
                '(add-to-list 'company-backends 'company-irony))
                ;; (optional) adds CC special commands to `company-begin-commands' in order to
                ;; trigger completion at interesting places, such as after scope operator
                ;;     std::|
                (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(use-package flycheck-irony
    :ensure t
    :config
        (eval-after-load 'flycheck
        '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(use-package rtags
    :ensure t
    :bind
        (("M-s s" . rtags-find-symbol-at-point)     ; follow symbol under cursor
         ("M-s S" . rtags-find-symbol)              ; promt for a symbol to go
         ("M-s r" . rtags-find-references-at-point) ; find all references to symbol under cursor
         ("M-s R" . rtags-find-references)          ; promt for reference
         ("M-s f" . rtags-find-file)                ; finds file in project (partially or not)
         ("M-s v" . rtags-find-virtuals-at-point)   ; show the various reimplementations of the function at point
         ("M-s e" . rtags-fixit)                    ; fixes simple errors
         ("M-s q" . rtags-imenu)                    ; shows a subset of the symbols in the current file
         ("M-s p" . rtags-preprocess-file)          ; preprocess current file or selected region
         ("M-s i" . rtags-print-cursorinfo)         ; print some info about symbol under cursor
         ("M-s c" . rtags-rename-symbol)))            ; rename symbol at point

(provide-me)

;;; cpp.el ends here
