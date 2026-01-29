;; languages.el
;; entrypoint for lsp, treesitter, debugger config

;; treesitter
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
	(qmljs "https://github.com/yuja/tree-sitter-qmljs")
	(ocaml "https://github.com/tree-sitter/tree-sitter-ocaml")))

(setq treesit-language-source-dir (expand-file-name "tree-sitter" user-emacs-directory))

;; most things work better with this and not flymake
(use-package flycheck
  :straight t
  :hook (lsp-mode . flycheck-mode))

;; comment whole lines
(use-package evil-nerd-commenter
  :straight t
  :bind ("C-;" . 'evilnc-comment-or-uncomment-lines))

;; comment folding
(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :hook ((prog-mode . treesit-fold-mode)))

;; show function signature in echo area
(use-package eldoc
  :straight t
  :hook (prog-mode . eldoc-mode)
  :config (setq eldoc-idle-delay 0.1))

;; prog-mode hooks
(use-package prog-mode
  :hook (prog-mode-hook . highlight-codetags-local-mode))

;; complestion
(use-package yasnippet
  :straight t
  :hook (prog-mode . yas-minor-mode))
(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  (company-selection-wrap-arount -1)
  (company-tooltip-limit 5)
  (company-lsp-enable-snippet t)
  :bind
  (:map company-active-map
	("<return>" . company-complete-selection)))

;; LSP
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-clients-clangd-args '("--clang-tidy"))
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  :hook
  ((lsp-mode . jthro/lsp-keybinds)
   (python-mode . lsp-deferred)))

(use-package lsp-ui
  :straight t
  :custom
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-diagnostic-max-lines 5)
  (lsp-ui-sideline-delay 0.5))

;; lsp ui doc frame toggle
(defvar jthro/focused-doc-frame nil)
(defun jthro/toggle-doc-focus ()
  (interactive)
  (if jthro/focused-doc-frame
      (progn
	(lsp-ui-doc-unfocus-frame)
	(setq jthro/focused-doc-frame nil)))
  (progn
    (lsp-ui-doc-focus-frame)
    (setq jthro/focused-doc-frame t)))

(defvar jthro/in-peek-mode nil)
(defun jthro/toggle-reference-peek ()
  (interactive)
  (if jthro/in-peek-mode
      (progn
	(lsp-ui-peek--abort)
	(setq jthro/in-peek-mode nil)))
  (progn
    (lsp-ui-peek-find-references)
    (setq jthro/in-peek-mode t)))

;; keybinds
(defun jthro/lsp-keybinds ()
  (meow-leader-define-key
   '("l" . jthro/lsp))
  (define-prefix-command 'jthro/lsp)
  (define-key jthro/lsp (kbd "r") 'lsp-find-references)
  (define-key jthro/lsp (kbd "d") 'lsp-find-definition)
  (define-key jthro/lsp (kbd "n") 'lsp-rename)
  (define-key jthro/lsp (kbd "f") 'lsp-format-buffer)
  (define-key jthro/lsp (kbd "a") 'lsp-execute-code-action)
  (define-key jthro/lsp (kbd "k") 'lsp-ui-doc-glance))

;;(define-key jthro/lsp (kbd "k") 'lsp-ui-doc-toggle)
;;(define-key jthro/lsp (kbd "K") 'jthro/toggle-doc-focus)
;;(define-key jthro/lsp (kbd "R") 'jthro/toggle-reference-peek)


;; language modules
(add-to-list 'load-path (expand-file-name "config/languages" user-emacs-directory))
(require 'c)
(require 'common-lisp)
(require 'guile)
(require 'qml)
(require 'ocaml)
(require 'haskell)
(require 'java)
(require 'rust)

(provide 'languages)
