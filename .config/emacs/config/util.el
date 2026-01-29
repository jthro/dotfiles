;; terminal.el
;; per-perspective terminal toggle

(use-package vterm
  :straight t)

;; behaviour: single tap sends <esc> to vterm
;;            double tap exits insert mode
(defvar jthro/vterm-escape-timer nil)
(defvar jthro/vterm-escape-delay 0.3)
(defun jthro/vterm-escape ()
  (interactive)
  (if (and jthro/vterm-escape-timer
           (timerp jthro/vterm-escape-timer))
      (progn
        (cancel-timer jthro/vterm-escape-timer)
        (setq jthro/vterm-escape-timer nil)
        (meow-normal-mode))
    (progn
      (vterm-send-escape)
      (setq jthro/vterm-escape-timer
            (run-with-timer jthro/vterm-escape-delay nil
                            (lambda () (setq jthro/vterm-escape-timer nil)))))))

;; override meow's escape by making a minor mode
(defvar jthro/vterm-escape-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [escape] #'jthro/vterm-escape)
    map))

(define-minor-mode jthro/vterm-escape-minor-mode
  "vterm escape intercept mode"
  :init-value nil
  :lighter nil
  :keymap jthro/vterm-escape-minor-mode-map
  :global nil)

(add-hook 'vterm-mode-hook #'jthro/vterm-escape-minor-mode)

;; new vterm buffer
(defun jthro/vterm-new ()
  "Create a new vterm buffer."
  (interactive)
  (let ((buffer (generate-new-buffer-name "*vterm*")))
    (vterm buffer)))

;; helper for generating names per-perspective
(defun jthro/get-persp-name ()
  (persp-name (persp-curr)))

(defvar jthro/vterm-list '() "List of active vterm minibuffers")
(defvar jthro/vterm-active-index 0 "Currently selected vterm minibuffer")

;; get the currently active vterm buffer
(defun jthro/vterm-get-current ()
  (if (null jthro/vterm-list)
      (jthro/vterm-create-new)
    (nth jthro/vterm-active-index jthro/vterm-list)))

;; create a new vterm mini window
(defun jthro/vterm-create-new ()
  (interactive)
  (let* ((new-index (length jthro/vterm-list))
	 (buf-name (format "*vterm-%d*" (1+ new-index)))
	 (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'vterm-mode)
	(vterm-mode)))
    (setq jthro/vterm-list (append jthro/vterm-list (list buf)))
    (setq jthro/vterm-active-index new-index)
    (let ((win (get-buffer-window-list nil nil t)))
      (dolist (w win)
	(when (member (window-buffer w) jthro/vterm-list)
	  (set-window-buffer w buf))))
    buf))

;; toggle vterm per-perspective
(defun jthro/vterm-toggle ()
  (interactive)
  (let* ((buf (jthro/vterm-get-current))
	 (window (get-buffer-window buf)))
    (if window
	(delete-window window)
      (display-buffer-in-side-window
       buf
       '((side . bottom)
	 (slot . 0)
	 (window-height . 0.3)))
      (select-window (get-buffer-window buf)))))

;; select next vterm window
(defun jthro/vterm-next ()
  (interactive)
  (when jthro/vterm-list
    (setq jthro/vterm-active-index (mod (1+ jthro/vterm-active-index) (length jthro/vterm-list)))
    (let ((buf (nth jthro/vterm-active-index jthro/vterm-list)))
      (let ((win (get-buffer-window-list nil nil t)))
	(dolist (w win)
	  (when (member (window-buffer w) jthro/vterm-list)
	    (set-window-buffer w buf)
	    (select-window w)))))))

;; select prev vterm window
(defun jthro/vterm-prev ()
  (interactive)
  (when jthro/vterm-list
    (setq jthro/vterm-active-index (mod (1- jthro/vterm-active-index) (length jthro/vterm-list)))
    (let ((buf (nth jthro/vterm-active-index jthro/vterm-list)))
      (let ((win (get-buffer-window-list nil nil t)))
	(dolist (w win)
	  (when (member (window-buffer w) jthro/vterm-list)
	    (set-window-buffer w buf)
	    (select-window w)))))))

;; remove a vterm window
(defun jthro/vterm-kill ()
  (interactive)
  (when jthro/vterm-list
    (let ((buf (nth jthro/vterm-active-index jthro/vterm-list)))
      (when (kill-buffer buf)
	(jthro/vterm-next)
	(setq jthro/vterm-list (delete buf jthro/vterm-list))))))



(define-prefix-command 'jthro/open-prefix)
(define-key jthro/open-prefix (kbd "T") 'jthro/vterm-new)
(define-key jthro/open-prefix (kbd "t") 'jthro/vterm-toggle)
(define-key jthro/open-prefix (kbd "c") 'jthro/vterm-create-new)
(define-key jthro/open-prefix (kbd "n") 'jthro/vterm-next)
(define-key jthro/open-prefix (kbd "p") 'jthro/vterm-prev)
(define-key jthro/open-prefix (kbd "x") 'jthro/vterm-kill)



(meow-leader-define-key
 '("o" . jthro/open-prefix))

(provide 'util)
