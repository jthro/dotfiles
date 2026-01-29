;; navigation.el
;; navigation keys etc

(define-prefix-command 'jthro/window-prefix)
(define-key jthro/window-prefix (kbd "s") 'split-window-below)
(define-key jthro/window-prefix (kbd "v") 'split-window-horizontally)
(define-key jthro/window-prefix (kbd "c") 'delete-window)
(define-key jthro/window-prefix (kbd "h") 'windmove-left)
(define-key jthro/window-prefix (kbd "j") 'windmove-down)
(define-key jthro/window-prefix (kbd "k") 'windmove-up)
(define-key jthro/window-prefix (kbd "l") 'windmove-right)

(meow-leader-define-key
 '("b" . jthro/buffer-prefix))
(define-prefix-command 'jthro/buffer-prefix)

(defun internal-kill-buffer()
  (interactive)
  (kill-buffer (current-buffer)))
(define-key jthro/buffer-prefix (kbd "k") 'internal-kill-buffer)
(define-key jthro/buffer-prefix (kbd "b") 'counsel-switch-buffer)

(meow-leader-define-key
 '("d" . jthro/describe-prefix))
(define-prefix-command 'jthro/describe-prefix)
(define-key jthro/describe-prefix (kbd "v") 'counsel-describe-variable)
(define-key jthro/describe-prefix (kbd "f") 'counsel-describe-function)

(meow-leader-define-key
 '("w" . jthro/window-prefix))

;; projects
(use-package projectile
  :straight t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)
	   (projectile-project-search-path '("~/Projects" "~/University")))
  :init (projectile-discover-projects-in-search-path))

(use-package counsel-projectile
  :after (projectile counsel)
  :straight t
  ; :custom ((counsel-projectile-switch-project-action #'projectile-dired))
  :config (counsel-projectile-mode))

(define-prefix-command 'jthro/project-prefix)
(define-key jthro/project-prefix (kbd "p") 'counsel-projectile-switch-project)
(define-key jthro/project-prefix (kbd "f") 'counsel-projectile-find-file)
(define-key jthro/project-prefix (kbd "r") 'counsel-projectile-rg)
(define-key jthro/project-prefix (kbd "b") 'counsel-projectile-switch-to-buffer)
(define-key jthro/project-prefix (kbd "h") 'projectile-find-other-file)

(meow-leader-define-key
 '("p" . jthro/project-prefix))


(provide 'navigation)
