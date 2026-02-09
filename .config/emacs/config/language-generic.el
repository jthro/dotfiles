;; language-generic.el
;; configuration for programming

(add-hook 'prog-mode (lambda ()
		       (setq indent-tabs-mode nil)
		       (setq-local electric-indent-mode nil)
		       (local-unset-key (kbd "C-d"))))

(provide 'language-generic)
