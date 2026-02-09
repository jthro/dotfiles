;; include paths
(add-to-list 'load-path (expand-file-name "config/" user-emacs-directory))

;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)
(require 'use-package)

(defun jthro/initialise-defaults ()
  (interactive)
  (setq inhibit-startup-message t)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode -1)
  (menu-bar-mode -1)

  (fset 'yes-or-no-p 'y-or-n-p)
  
  (column-number-mode t)
  ;; i hate autosave
  (setq make-backup-files nil)

  ;; brackets
  (electric-pair-mode 1)

  ;; opacity
  (set-frame-parameter nil 'alpha-background 80)

  ;; copy/paste
  (setq select-enable-clipboard t)

  ;; minibuffer completion
  (fido-vertical-mode))

(jthro/initialise-defaults)

;; modules
(require 'meow-binds)
(require 'themes)
(require 'completion)
(require 'navigation)
(require 'language-generic)
(require 'c)
(require 'rust)
(require 'python)
(require 'lisp)
(require 'org-config)
(require 'clipboard)

;; visuals
(when (daemonp)
  (set-face-attribute 'default nil :font "Monaspace Argon" :height 100)
  (add-to-list 'default-frame-alist '(font . "Monaspace Argon")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(base16-theme cape corfu geiser-guile meow orderless org-bullets
		  org-roam-ui)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
