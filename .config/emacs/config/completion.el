;; completion.el
;; in-buffer completion

;; corfu - completion framework
(use-package corfu
  :ensure t
  :after meow
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto nil)
  :config
  (meow-define-keys 'insert
    '("C-f" . completion-at-point)))

;; orderless - fuzzy matching
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; cape - file path completion
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(provide 'completion)
