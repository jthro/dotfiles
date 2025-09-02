;; org-conf.el
;; org-mode and org-roam configuration

(require 'common)

(defun jthro/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :init
  (setq org-startup-with-latex-preview t)
  (setq org-startup-with-inline-images t)
  (set-face-attribute 'variable-pitch nil :family "Monaspace Neon" :height 90)
  :hook
  (org-mode . jthro/org-mode-setup)
  :config
  ;(local-set-key (kbd "RET") 'org-open-at-point)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1))
  (setq org-ellipsis ""
	org-hide-emphasis-markers t)
  (setq org-agenda-files
	'("~/org-roam/Workspace/2025-07-17-backlog.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-into-drawer t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '(" ")))

(use-package org-roam
  :init
  (setq org-roam-directory (file-truename "~/org-roam"))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
	`(("l" "Lecture" plain
           "* ${title}\n"
           :target (file+head "Sources/Lectures/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n#+date: %<%Y-%m-%d>\n\n")
           :unnarrowed t)

          ("s" "Source" plain
           "* ${title}\n"
           :target (file+head "Sources/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n#+date: %<%Y-%m-%d>\n\n")
           :unnarrowed t)
	  
          ("n" "Note" plain
           "Tags:\n\n* ${title}\n\n\n* Sources:\n"
           :target (file+head "Notes/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n\n")
           :unnarrowed t)
	  
          ("t" "Tag" plain
           "* ${title}\n"
           :target (file+head "Tags/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
	  
          ("r" "Workspace Note" plain
           "Tags:\n\n* ${title}\n"
           :target (file+head "Workspace/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
	  
          ("i" "Index" plain
           "* ${title}\n"
           :target (file+head "Indices/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode))


;; pretty graph
(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  :after org-roam
  (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t))

(provide 'org-conf)
