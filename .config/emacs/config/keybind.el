;; keybind.el
;; general keybinds + associated stuff

;; which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 2))

;; binds
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
   "r i" '(org-roam-node-insert :which-key "roam-insert-node")
   "RET" '(org-open-at-point :which-key "org-open-at-point"))

  (general-create-definer jthro/tab
    :states 'normal
    :prefix "SPC TAB"
    :keymaps 'override)
  (jthro/tab
    "n" '(tab-new :which-key "new-tab")
    "d" '(tab-close :which-key "close-tab")
    "1" '(tab-select 1 :which-key "tab-1")
    "2" '(tab-select 2 :which-key "tab-2")
    "3" '(tab-select 3 :which-key "tab-3")
    "4" '(tab-select 4 :which-key "tab-4")
    "5" '(tab-select 5 :which-key "tab-5")
    "6" '(tab-select 6 :which-key "tab-6")
    "7" '(tab-select 7 :which-key "tab-7")
    "8" '(tab-select 8 :which-key "tab-8")
    "9" '(tab-select 9 :which-key "tab-9")))

;; get dired to play nice
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "SPC") nil))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "K") nil)
  (define-key evil-normal-state-map (kbd "K") #'lsp-ui-doc-toggle))


;; evil
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

(provide 'keybind)
