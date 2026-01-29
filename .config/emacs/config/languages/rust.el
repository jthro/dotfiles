;; rust.el
;; support for the rust language

(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

(use-package rust-ts-mode
  :hook ((rust-ts-mode . lsp-deferred)))

(provide 'rust)
