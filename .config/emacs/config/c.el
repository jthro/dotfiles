;; c.el
;; configuration for c programming language

(use-package eglot
  :ensure t
  :hook ((c-mode) . (lambda ()
		      ;; my config
		      (eglot-ensure)
		      (setq c-basic-offset 4)
		      (setq indent-tabs-mode nil)
		      ;; stop c-mode from taking over my life
		      (setq c-electric-flag nil)
		      (setq c-syntactic-indentation nil))))

(provide 'c)
