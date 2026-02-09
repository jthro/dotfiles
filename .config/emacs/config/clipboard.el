;; clipboard.el
;; clipboard configuration for wayland

(setq select-enable-clipboard t)
(setq select-enable-primary nil)

(unless (display-graphic-p)
  (with-eval-after-load 'meow
    (setq interprogram-cut-function
	  (lambda (text &optional push)
	    (let ((process-connection-type nil))
	      (let ((proc (start-process "wl-copy" nil "wl-copy" "--type" "text/plain" text)))
		(process-send-string proc text)
		(process-send-eof proc)))))
    (setq interprogram-paste-function
	  (lambda ()
	    (let ((process-connection-type nil))
	      (shell-command-to-string "wl-paste -n"))))))

(provide 'clipboard)
