;; qml.el
;; i have this for quickshell

(use-package qml-ts-mode
  :straight (:type git :host github :repo "xhcoding/qml-ts-mode")
  :hook ((qml-ts-mode . lsp-deferred))
  :config
  (add-to-list 'lsp-language-id-configuration '(qml-ts-mode . "qml-ts"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("/usr/lib/qt6/bin/qmlls"))
                    :activation-fn (lsp-activate-on "qml-ts")
                    :server-id 'qmlls)))

(provide 'qml)
