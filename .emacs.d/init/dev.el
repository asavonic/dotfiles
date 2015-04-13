(use-package magit
    :ensure t
    :bind ("C-x g" . magit-status))

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



(load-relative "dev/cpp")
(load-relative "dev/python")
