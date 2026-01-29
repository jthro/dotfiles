;; common.el
;; default settings etc.

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
  (setq select-enable-clipboard t))

(use-package ivy
  :straight t
  :init (ivy-mode 1))

(use-package ivy-posframe
  :straight t
  :custom
  ((ivy-display-functions-alit '((t . ivy-posframe-display-at-frame-center))))
  :init (ivy-posframe-mode 1))

(use-package counsel
  :straight t
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :straight t
  :init (ivy-rich-mode 1))

(use-package helpful
  :straight t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(defun meow-clipboard-save (beg end)
  "Copy selected region to the system clipboard and kill ring."
  (interactive "r")
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties beg end)))
      (kill-new text)
      (gui-set-selection 'CLIPBOARD text)
      (deactivate-mark)
      (message "Copied to clipboard"))))

(define-key meow-normal-state-keymap (kbd "y") #'meow-clipboard-save)

(bind-key "C-u" 'undo-redo)

(provide 'common)
