(use-package haskell-mode
  :straight t
  :hook (haskell-mode . lsp-deferred))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "haskell-language-server-wrapper")
		  :major-modes '(haskell-mode)
		  :server-id 'hls))

(provide 'haskell)
