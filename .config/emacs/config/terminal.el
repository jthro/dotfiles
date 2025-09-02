;; terminal.el
;; vterm config

(require 'common)

(use-package vterm)

(defun jthro/vterm-new ()
  "Create a new vterm buffer."
  (interactive)
  (let ((buffer (generate-new-buffer-name "*vterm*")))
    (vterm buffer)))


;; tap esc once to send to terminal
;; tap twice to go to normal mode
(defvar jthro/vterm-escape-timer nil)
(defvar jthro/vterm-escape-delay 0.2)
(defun jthro/vterm-escape ()
  (interactive)
  (if (and jthro/vterm-escape-timer
           (timerp jthro/vterm-escape-timer))
      (progn
	(cancel-timer jthro/vterm-escape-timer)
	(setq jthro/vterm-escape-timer nil)
	(evil-force-normal-state))
    (progn
      (setq jthro/vterm-escape-timer
            (run-with-timer jthro/vterm-escape-delay nil
                            (lambda () (setq jthro/vterm-escape-timer nil))))
      (vterm-send-escape))))


;; toggle persistent vterm buffer
(defun jthro/vterm-toggle ()
  (interactive)
  (let ((buf-name "*vterm-toggle*"))
    (if (get-buffer-window buf-name)
        (delete-window (get-buffer-window buf-name))
      (let ((buf (get-buffer-create buf-name)))
        (unless (comint-check-proc buf)
          (with-current-buffer buf
            (vterm-mode)))
        (display-buffer-in-side-window
         buf
         '((side . bottom)
           (slot . 0)
           (window-height . 0.3)))
	(select-window (get-buffer-window buf-name))))))


(with-eval-after-load 'vterm
  (general-define-key
   :states 'insert
   :keymaps 'vterm-mode-map
   "<escape>" 'jthro/vterm-escape)
  (general-define-key
   :states 'normal
   :keymaps 'override
   "SPC o t" 'jthro/vterm-toggle
   "SPC o T" 'jthro/vterm-new))

(provide 'terminal)
