;; defaults.el
;; default variable settings

(setq inhibit-startup-message t) ; Disable startup message

(scroll-bar-mode -1) ; scrollbar off
(tool-bar-mode -1)   ; toolbar off
(tooltip-mode -1)    ; tooltips off
(set-fringe-mode -1) ; side gaps off
(menu-bar-mode -1)   ; menu bar off

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; make esc quit buffers

(fset 'yes-or-no-p 'y-or-n-p) ; fix quit prompt


;; line numbers
(column-number-mode)                       ; column numbers in modeline
(setq display-line-numbers-type 'relative) ; relative line numbers
(global-display-line-numbers-mode t)       ; always display line numbers

;; disable line numbers in term mode
(dolist (mode '(vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; autosave is a fuck
(setq make-backup-files nil)
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/" user-emacs-directory)
      lock-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/"
								user-emacs-directory ) t)))

(provide 'defaults)
