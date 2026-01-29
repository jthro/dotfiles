;; guile.el
;; configuration for scheme - i use guile

(defun jthro/guile-keybinds ()
  (interactive)
  (meow-leader-define-key
   '("l" . jthro/geiser))
  (define-prefix-command 'jthro/geiser)
  (define-key jthro/geiser (kbd "k") 'geiser-doc-symbol-at-point))

(use-package geiser-guile
  :straight t
  :hook ((scheme-mode . jthro/guile-keybinds)
	 (scheme-mode . geiser-mode)))

(provide 'guile)
