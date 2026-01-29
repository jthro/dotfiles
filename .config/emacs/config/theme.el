;; theme.el
;; theme configuration
(use-package all-the-icons
  :straight t)

(use-package kanagawa-themes
  :straight t)

(use-package base16-theme
  :straight t)

(use-package doom-themes
  :straight t
  :config
  (load-theme 'base16-black-metal-venom t)
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :ensure t
  :init (doom-modeline-mode 1))

;; set fonts
;; interactive because i switch to larger font if i am coding with an audience
(defun jthro/set-font-faces ()
  (interactive)
  (set-frame-parameter nil 'alpha-background 80)
  (set-face-attribute 'default nil :font "Monaspace Argon" :height 100)
  (add-to-list 'default-frame-alist '(font . "Monaspace Argon"))
  (load-theme 'base16-black-metal-venom t))

(defun jthro/tutor-mode ()
  (interactive)
  (set-face-attribute 'default nil :font "Monaspace Neon" :height 180))

(provide 'theme)
