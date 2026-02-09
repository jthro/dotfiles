;; rust.el
;; configuration related to rust programming language

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))


(use-package eglot
  :ensure t
  :hook ((rust-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	     '((rust-ts-mode rust-mode) .
	       ("rust-analyzer" :initializationOptions
		( :check (:command "clippy")
		  :procMacro (:enable t)
		  :cargo ( :buildScripts (:enable t)
			   :features "all"))))))

(with-eval-after-load 'project
  (add-to-list 'project-vc-extra-root-markers "Cargo.toml"))

(provide 'rust)
