;; java.el
;; setup for java

(use-package lsp-java
  :straight t
  :hook (java-mode . lsp-deferred))

(provide 'java)
