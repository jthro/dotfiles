(use-package tuareg
  :straight t
  :mode (("\\.ocamlinit\\'" . tuareg-mode))
  :hook (tuareg-mode . lsp-deferred))


(provide 'ocaml)
