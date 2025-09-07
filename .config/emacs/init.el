;; init.el
;; entry point for emacs config

;; includes
(add-to-list 'load-path (expand-file-name "config/" user-emacs-directory))

(require 'common)
(require 'misc)
(require 'defaults)
(require 'language)
(require 'org-conf)
(require 'terminal)
(require 'theming)
(require 'keybind)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0"
     "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
     "9b9d7a851a8e26f294e778e02c8df25c8a3b15170e6f9fd6965ac5f2544ef2a9"
     "f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326"
     "166a2faa9dc5b5b3359f7a31a09127ebf7a7926562710367086fcc8fc72145da"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "2d96ef4ebc505478d857783aa24837b5e6931c0c54c4260556a7f89c9df43e65"
     "710ab6cc1e5dac21c6b31b3e6d341b5960e10f3ae55be9a2cad9b6bd52b9ae1f"
     "1e33781c0c68716f061ac8f66cd35278c835d34e86b21bc57aed4ea62546bcd2"
     default))
 '(package-selected-packages
   '(all-the-icons company corfu counsel-projectile dap-lldb
		   doom-modeline doom-themes evil-collection
		   evil-nerd-commenter flycheck general haskell-mode
		   helpful ivy-rich lsp-ivy lsp-java lsp-ui
		   no-littering org-bullets org-roam-ui
		   rainbow-delimiters undo-fu visual-fill-column vterm
		   yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
