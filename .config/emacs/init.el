;; init.el
;; entry point for emacs config

;; font setup
(defun jthro/set-font-faces ()
  (set-face-attribute 'default nil :font "Monaspace Neon" :height 90)
  (add-to-list 'default-frame-alist '(font . "Monaspace Neon-10")))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(setq doom-modeline-icon t)
		(with-selected-frame frame
		  (jthro/set-font-faces))))
  (jtho/set-font-faces))

;; includes
(add-to-list 'load-path (expand-file-name "config/" user-emacs-directory))

(require 'common)
(require 'defaults)
(require 'language)
(require 'org-conf)
(require 'terminal)

;; Themes
;; NOTE: On a new machine, run M-x all-the-icons-install-fonts

(use-package all-the-icons)
(use-package doom-themes
  :init
  (add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
  :config 
  (load-theme 'doom-laserwave t)
  (doom-themes-org-config))

;; Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 2))

;; Better Help
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Verilog
(use-package verilog-mode
  :init
  (setq verilog-indent-level 4
	verilog-indent-level-module 4
	verilog-indent-level-behavioral 4
	verilog-indent-level-directive 4
	verilog-cexp-indent 4
	verilog-case-indent 4
	verilog-auto-newline nil))

;; Tree-sitter
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")))

(use-package haskell-mode)

(use-package flycheck)

;; Lsp
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

;; Languages
;; Java
(use-package lsp-java
  :hook
  ((java-mode) . lsp)
  :config
  (setq lsp-java-java-path (concat (getenv "JAVA_HOME") "/bin/java")
        lsp-java-configuration-runtimes '[(:path "/usr/lib/jvm/default/")]
        lsp-java-classpath (list "/usr/lib/jvm/default/lib/modules"))
  (setenv "JAVA_HOME" "/usr/lib/jvm/default/"))

(use-package evil-nerd-commenter
  :bind ("C-;" . evilnc-comment-or-uncomment-lines))

;; Verilog/SystemVerilog
(use-package verilog-mode)

(electric-pair-mode 1)

;; Eldoc
(use-package eldoc
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0))

;; Completion
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

;; Make lisp nice
(use-package rainbow-delimiters
  :hook (elisp-mode . rainbow-delimiters-mode))

;; Undo
(use-package undo-fu)

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/Projects" "~/University"))
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-discover-projects-in-search-path))

(use-package counsel-projectile
  :init
  ;; Default behaviour - open in dired
  (setq counsel-projectile-switch-project-action (lambda (projname) (dired projname)))
  :custom (counsel-projectile-mode t))

;; Binds
(use-package general
  :config
  (general-create-definer jthro/leader
    :states '(normal visual emacs)
    :prefix "SPC"
    :keymaps 'override)
  (jthro/leader
    "." '(counsel-find-file :which-key "find-file"))

  (general-create-definer jthro/window
    :states '(normal visual emacs)
    :prefix "SPC w"
    :keymaps 'override)
  (jthro/window
    "h" '(evil-window-left :which-key "window-left")
    "j" '(evil-window-down :which-key "window-down")
    "k" '(evil-window-up :which-key "window-up")
    "l" '(evil-window-right :which-key "window-right")
    "v" '(evil-window-vsplit :which-key "window-vsplit")
    "s" '(evil-window-split :which-key "window-split")
    "c" '(evil-window-delete :which-key "window-delete"))

  (general-create-definer jthro/buffer
    :states '(normal visual emacs)
    :prefix "SPC b"
    :keymaps 'override)
  (jthro/buffer
    "k" '(kill-current-buffer :which-key "kill-current-buffer")
    "b" '(ivy-switch-buffer :which-key "switch-buffer")
    "f" '(lsp-format-buffer :which-key "format-buffer"))

  (general-create-definer jthro/lsp
    :states '(normal visual emacs)
    :prefix "SPC c"
    :keymaps 'override)
  (jthro/lsp
   "d" '(lsp-find-definition :which-key "goto-definition")
   "r" '(lsp-find-references :which-key "find-references")
   "n" '(lsp-rename :which-key "rename")
   "a" '(lsp-execute-code-action :which-key "code-action")
   "i" '(lsp-find-implementation :which-key "find-implementations")
   "D" '(lsp-find-declaration :which-key "goto-declaration")
   "t" '(lsp-find-type-definition :which-key "type definition"))

  (general-create-definer jthro/projectile
    :states '(normal visual emacs)
    :prefix "SPC p"
    :keymaps 'override)
  (jthro/projectile
    "p" '(counsel-projectile-switch-project :which-key "switch-project")
    "f" '(counsel-projectile-find-file :which-key "project-find-file")
    "s" '(counsel-projectile-grep :which-key "project-grep"))

  (general-create-definer jthro/org-display
    :states 'normal
    :prefix "SPC m"
    :keymaps 'org-mode-map)
  (jthro/org-display
    "l" '(org-latex-preview :which-key "toggle-latex")
    "z" '(org-toggle-inline-images :which-key "toggle-images"))

  (general-create-definer jthro/org-roam
    :states 'normal
    :prefix "SPC n"
    :keymaps 'override)
  (jthro/org-roam
   "r f" '(org-roam-node-find :which-key "roam-find-node")
   "r i" '(org-roam-node-insert :which-key "roam-insert-node")))

;; Get dired to play nice
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "SPC") nil))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "K") nil)
  (define-key evil-normal-state-map (kbd "K") #'lsp-ui-doc-toggle))


;; EVIL
(use-package evil
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    (kbd "RET") #'dired-find-file))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0"
     "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
     "9b9d7a851a8e26f294e778e02c8df25c8a3b15170e6f9fd6965ac5f2544ef2a9"
     "f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326"
     "166a2faa9dc5b5b3359f7a31a09127ebf7a7926562710367086fcc8fc72145da"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "2d96ef4ebc505478d857783aa24837b5e6931c0c54c4260556a7f89c9df43e65"
     "710ab6cc1e5dac21c6b31b3e6d341b5960e10f3ae55be9a2cad9b6bd52b9ae1f"
     "1e33781c0c68716f061ac8f66cd35278c835d34e86b21bc57aed4ea62546bcd2"
     default))
 '(package-selected-packages
   '(all-the-icons company corfu counsel-projectile dap-lldb
		   doom-modeline doom-themes evil-collection
		   evil-nerd-commenter flycheck general haskell-mode
		   helpful ivy-rich lsp-ivy lsp-java lsp-ui
		   no-littering org-bullets org-roam-ui
		   rainbow-delimiters undo-fu visual-fill-column vterm
		   yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
