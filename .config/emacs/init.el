(setq inhibit-startup-message t)

(scroll-bar-mode -1)		; Turn off scrollbar
(tool-bar-mode -1)		; Turn off toolbar
(tooltip-mode -1)		; Turn off tooltips
(set-fringe-mode -1)		; Turn off weird gaps on the side

(menu-bar-mode -1)		; Turn off menu bar

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

;; Make Esc quit buffers
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Make prompt to quit active process less obnoxious
(fset 'yes-or-no-p 'y-or-n-p)


;; Line numbers
(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(dolist (mode '(term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; c-mode is yucky and old
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(setq c-ts-mode-indent-offset 4)
(setq c++-ts-mode-indent-offset 4)

;; Autosave is a fuck
(setq make-backup-files nil)
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/" user-emacs-directory)
      lock-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/"
								user-emacs-directory ) t)))


;; Packages
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ; Always pull packages if they don't exist

;; Org / Org-roam
(defun jthro/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :init
  (setq org-startup-with-latex-preview t)
  (setq org-startup-with-inline-images t)
  (set-face-attribute 'variable-pitch nil :family "Monaspace Neon" :height 90)
  :hook
  (org-mode . jthro/org-mode-setup)
  :config
  (local-set-key (kbd "RET") 'org-open-at-point)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1))
  (setq org-ellipsis ""
	org-hide-emphasis-markers t)
  (setq org-agenda-files
	'("~/org-roam/Workspace/2025-07-17-backlog.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-into-drawer t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '(" ")))

(use-package org-roam
  :init
  (setq org-roam-directory (file-truename "~/org-roam"))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
	`(("l" "Lecture" plain
           "* ${title}\n"
           :target (file+head "Sources/Lectures/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n#+date: %<%Y-%m-%d>\n\n")
           :unnarrowed t)

          ("s" "Source" plain
           "* ${title}\n"
           :target (file+head "Sources/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n#+date: %<%Y-%m-%d>\n\n")
           :unnarrowed t)
	  
          ("n" "Note" plain
           "Tags:\n\n* ${title}\n\n\n* Sources:\n"
           :target (file+head "Notes/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n\n")
           :unnarrowed t)
	  
          ("t" "Tag" plain
           "* ${title}\n"
           :target (file+head "Tags/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
	  
          ("r" "Workspace Note" plain
           "Tags:\n\n* ${title}\n"
           :target (file+head "Workspace/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
	  
          ("i" "Index" plain
           "* ${title}\n"
           :target (file+head "Indices/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode))

;; Pretty graph
(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  :after org-roam
  (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t))

;; Vterm
(use-package vterm)

(defun my/vterm-new ()
  "Create a new vterm buffer."
  (interactive)
  (let ((buffer (generate-new-buffer-name "*vterm*")))
    (vterm buffer)))

(defvar my/vterm-escape-timer nil
"Timer to detect double ESC in vterm.")
(defvar my/vterm-escape-delay 0.2
"Time window to detect double ESC.")
(defun my/vterm-escape ()
  "Handle ESC in vterm: first press sends ESC to terminal, second quickly goes to normal mode."
  (interactive)
  (if (and my/vterm-escape-timer
           (timerp my/vterm-escape-timer))
      (progn
	(cancel-timer my/vterm-escape-timer)
	(setq my/vterm-escape-timer nil)
	;; Second ESC press — pass to Emacs (simulate `keyboard-quit`)
	(evil-force-normal-state))
    ;; First ESC press — send to vterm
    (progn
      (setq my/vterm-escape-timer
            (run-with-timer my/vterm-escape-delay nil
                            (lambda () (setq my/vterm-escape-timer nil))))
      (vterm-send-escape))))

(defun my/vterm-toggle ()
  "Toggle a persistent vterm buffer at the bottom of the screen."
  (interactive)
  (let ((buf-name "*vterm-toggle*"))
    (if (get-buffer-window buf-name)
        (delete-window (get-buffer-window buf-name))
      (let ((buf (get-buffer-create buf-name)))
        (unless (comint-check-proc buf)
          (with-current-buffer buf
            (vterm-mode)))
        (display-buffer-in-side-window
         buf
         '((side . bottom)
           (slot . 0)
           (window-height . 0.3)))
	(select-window (get-buffer-window buf-name))))))

(with-eval-after-load 'vterm
  (general-define-key
   :states 'insert
   :keymaps 'vterm-mode-map
   "<escape>" 'my/vterm-escape)
  (general-define-key
   :states 'normal
   :keymaps 'override
   "SPC o t" 'my/vterm-toggle
   "SPC o T" 'my/vterm-new))

;; Ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-switch-buffer-map
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1))


;; Ivy-rich (more information inside Ivy)
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; Counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

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
  :config
  (lsp-enable-which-key-integration t)
  (lsp-register-client
   (make-lsp--client
    :new-connection (lsp-stdio-connection
		     (lambda ()
		       `("verible-verilog-ls" "--rules_config_search")))
    :major-modes '(verilog-mode)
    :server-id 'verible-verilog-ls)))

(use-package lsp-ui
  :init
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-doc-position 'at-point))

;; DAP
(use-package dap-mode
  :config
  (require 'dap-gdb))

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
		   evil-nerd-commenter flycheck general helpful
		   ivy-rich lsp-ivy lsp-java lsp-ui no-littering
		   org-bullets org-roam-ui rainbow-delimiters undo-fu
		   visual-fill-column vterm yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
