;; navigation.el
;; buffer and window navigation

(with-eval-after-load 'meow
  (define-prefix-command 'jthro/meow-window-map)
  (define-key jthro/meow-window-map (kbd "v" )'split-window-horizontally)
  (define-key jthro/meow-window-map (kbd "s") 'split-window-vertically)
  (define-key jthro/meow-window-map (kbd "c") 'delete-window)
  (define-key jthro/meow-window-map (kbd "h") 'windmove-left)
  (define-key jthro/meow-window-map (kbd "j") 'windmove-down)
  (define-key jthro/meow-window-map (kbd "k") 'windmove-up)
  (define-key jthro/meow-window-map (kbd "l") 'windmove-right)
  (meow-leader-define-key
    '("w" . jthro/meow-window-map))

  (define-prefix-command 'jthro/meow-buffer-map)
  (define-key jthro/meow-buffer-map (kbd "b") 'switch-to-buffer)
  (define-key jthro/meow-buffer-map (kbd "k") 'kill-buffer)
  (define-key jthro/meow-buffer-map (kbd "f") 'eglot-format-buffer)
  (meow-leader-define-key
   '("b" . jthro/meow-buffer-map)))

(provide 'navigation)
