;; misc.el
;; stuff that doesn't fit anywhere else

;; ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-switch-buffer-map
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1))


;; ivy-rich (more information inside Ivy)
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

;; better help
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; undo
(use-package undo-fu)

;; projectile
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

(provide 'misc)
