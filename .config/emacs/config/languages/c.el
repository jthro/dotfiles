;; c.el
;; C and C++ specific config

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

(use-package c-ts-mode
  :hook ((c-ts-mode . lsp-deferred)))

(use-package c++-ts-mode
  :hook ((c++-ts-mode . lsp-deferred)))

(setq c-ts-mode-indent-offset 4)

(provide 'c)
