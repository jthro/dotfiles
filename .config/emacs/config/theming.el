;; theming.el
;; all configuration related to themes

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


;; on a new machine, run M-x all-the-icons-install-fonts
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

(provide 'theming)
