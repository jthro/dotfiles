;; language.el
;; language-specific configuration

(require 'common)

;; c-mode is yucky and old
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(setq c-ts-mode-indent-offset 4)
(setq c++-ts-mode-indent-offset 4)



;; treesitter
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")))

;; flycheck (necessary for lsp-ui to work properly)
(use-package flycheck)

;; comment hotkey
(use-package evil-nerd-commenter
  :bind ("C-;" . evilnc-comment-or-uncomment-lines))

;; eldoc
(use-package eldoc
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0))

;; code completion
 (use-package company
   :init
   (setq company-idle-delay 0)
   (setq company-minimum-prefix-length 1)
   (global-company-mode)
   :config
   (define-key company-active-map (kbd "<escape>") 'company-abort))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

;; === LSP ===
(use-package lsp-mode
  :after doom-themes
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-snippet t)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-enable-on-type-formatting t)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-face-semhl-type font-lock-type-face)
  (setq lsp-enable-indentation t)
  
  :hook
  ((c-ts-mode) . lsp)
  ((c-ts-mode) . (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
  ((c++-ts-mode) . lsp)
  ((verilog-mode) . lsp)
  ((latex-mode) . lsp)
  ((rust-ts-mode) . lsp)
  ((python-mode) . lsp)
  ((haskell-mode-hook) . lsp)
  :config
  (lsp-enable-which-key-integration t)
  (lsp-register-client
   (make-lsp--client
    :new-connection (lsp-stdio-connection
		     (lambda ()
		       `("verible-verilog-ls" "--rules_config_search")))
    :major-modes '(verilog-mode)
    :server-id 'verible-verilog-ls))
  (lsp-register-client
   (make-lsp--client
    :new-connection (lsp-stdio-connection
		     (lambda ()
		       `("haskell-language-server-wrapper")))
    :major-modes '(haskell-mode)
    :server-id 'haskell-language-server)))


(use-package lsp-ui
  :init
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-diagnostic-max-lines 5))

;; DAP
; (use-package dap-mode
;   :config
;   (require 'dap-gdb))

;; === LANGUAGE SPECIFIC ===

;; haskell
(use-package haskell-mode)

;; verilog
(use-package verilog-mode
  :init
  (setq verilog-indent-level 4
	verilog-indent-level-module 4
	verilog-indent-level-behavioral 4
	verilog-indent-level-directive 4
	verilog-cexp-indent 4
	verilog-case-indent 4
	verilog-auto-newline nil))

;; java
(use-package lsp-java
  :hook
  ((java-mode) . lsp)
  :config
  (setq lsp-java-java-path (concat (getenv "JAVA_HOME") "/bin/java")
        lsp-java-configuration-runtimes '[(:path "/usr/lib/jvm/default/")]
        lsp-java-classpath (list "/usr/lib/jvm/default/lib/modules"))
  (setenv "JAVA_HOME" "/usr/lib/jvm/default/"))

;; lisp
(use-package rainbow-delimiters
  :hook (elisp-mode . rainbow-delimiters-mode))

(provide 'language)
