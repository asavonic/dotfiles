(use-package company
    :ensure t
    :init (setq company-idle-delay 0)
    :config (global-company-mode))

(use-package flycheck
    :ensure t
    :init (global-flycheck-mode))

(use-package yasnippet
    :ensure t
    :config (yas-global-mode 1))


(load-relative "dev/cpp")
(load-relative "dev/python")
