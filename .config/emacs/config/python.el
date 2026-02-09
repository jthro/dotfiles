;; python.el
;; configuration for python programming language

(use-package eglot
  :ensure t
  :hook ((python-mode) . (lambda()
			   (eglot-ensure))))

(provide 'python)
