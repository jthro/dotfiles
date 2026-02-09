;; org-config.el
;; configuration pertaining to org-mode and org-roam

(use-package org
  :ensure t
  :init
  (setq org-startup-with-latex-preview t)
  (setq org-startup-with-inline-images t)
  :hook
  (org-mode . (lambda ()
		(org-indent-mode)
		(visual-line-mode 1)))
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1))
  (setq org-ellipsis ""
	org-hide-emphasis-markers t)
  (setq org-agenda-files
	'("~/org-roam/Workspaces/2025-11-26-planning.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-into-drawer t))


(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '(" ")))

(use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-directory (file-truename "~/new-org-roam"))
  (setq org-roam-capture-templates
	`(("l" "Lecture" plain
           "Tags: [[id:d43c8b52-dc69-41c9-a090-88ccad51bfaa][lecture]]\n\n* ${title}\n"
           :target (file+head "%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n#+date: %<%Y-%m-%d>\n\n")
           :unnarrowed t)

          ("w" "Website" plain
           "Tags: [[id:aeb64efc-9684-4ca3-8bcb-c5ada948805f][website]]\n\n* ${title}\n"
           :target (file+head "%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n#+date: %<%Y-%m-%d>\n\n")
           :unnarrowed t)
	  
	  ("b" "Book" plain
           "Tags: [[id:4bc8457f-bace-4256-94a1-9e535bdb8bd8][book]]\n\n* ${title}\n"
           :target (file+head "%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n#+date: %<%Y-%m-%d>\n\n")
           :unnarrowed t)
	  
          ("n" "Note" plain
           "Tags: [[id:3478124c-8837-47ed-ad2f-5e7233b923b3][note]]\n\n* ${title}\n\n\n* Sources:\n"
           :target (file+head "%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n\n")
           :unnarrowed t)
	  
          ("t" "Tag" plain
           "* ${title}\n"
           :target (file+head "%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
	  
          ("r" "Workspace Note" plain
           "Tags: [[id:436401b3-4b53-45de-89db-78b18a51bdaf][workspace]]\n\n* ${title}\n"
           :target (file+head "Workspaces/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)

	  ("d" "Daily" plain
	   "Tags: [[id:e6e86dc2-2149-411d-9d28-fe61b864dc04][Triage]]\n\n* ${title}\n"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: Daily Note %<%Y-%m-%d>\n")
           :unnarrowed t)
	  
          ("i" "Index" plain
           "Tags: [[id:b7285dda-54cc-477e-8a1e-7e1333e44405][index]]\n\n* ${title}\n"
           :target (file+head "%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n")

	   
           :unnarrowed t)))
  :config)

(use-package org-roam-ui
  :ensure t
  :after org-roam
  (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t))

(define-prefix-command 'jthro/org)
(define-key jthro/org (kbd "f") 'org-roam-node-find)
(define-key jthro/org (kbd "i") 'org-roam-node-insert)
(define-key jthro/org (kbd "RET") 'org-open-at-point)
(define-key jthro/org (kbd "a") 'org-agenda-list)
(define-key jthro/org (kbd "z") 'org-latex-preview)
(define-key jthro/org (kbd "l") 'org-toggle-inline-images)
(meow-leader-define-key '("n" . jthro/org))


(provide 'org-config)
