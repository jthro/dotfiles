;; pomodoro.el
;; pomodoro timer plugin

(use-package org-pomodoro
  :straight t
  :custom
  (org-pomodoro-length 30)
  (org-pomodoro-finished-sound "/home/jthro/.config/emacs/samsung-notification-sound-effect-but-bass-boost.mp3")
  (org-pomodoro-short-break-sound "/home/jthro/.config/emacs/samsung-notification-sound-effect-but-bass-boost.mp3")
  (org-pomodoro-long-break-sound "/home/jthro/.config/emacs/samsung-notification-sound-effect-but-bass-boost.mp3")
  (org-pomodoro-audio-player "mpv"))


(provide 'pomodoro)
