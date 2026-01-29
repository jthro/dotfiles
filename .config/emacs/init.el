;; init.el
;; entrypoint, package stuff

(require 'package)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; modules
(add-to-list 'load-path (expand-file-name "config/" user-emacs-directory)) ; change path later
(require 'meow-binds)
(require 'common)
(require 'theme)
(require 'languages)
(require 'navigation)
(require 'org-conf)
(require 'util)
(require 'pomodoro)
(require 'codetags)

;; init sequence
(jthro/initialise-defaults)

(add-hook 'after-make-frame-functions (lambda (frame)
  (with-selected-frame frame
    (jthro/initialise-defaults))))

(jthro/set-font-faces)
(when (daemonp)
  (sleep-for 1)
  (jthro/set-font-faces))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b54b64550215242ad351a5c7219f1b30d2cec3d1710a2121862ae6d51c7a1c12"
     "6751dfe84d9ccee2b93a3055db7deb4da7d69f49b45e4aaf81dbcb7ab75e19a5"
     "a0724a2dbf003213ef1bd93c7df0f07a5368399554078fc85819b2f9f6152101"
     "75863c7d5dca60cfb927b5de5c8f39b2a9d7756c5f346990e4423aba33cb3a9d"
     "b2a3b2bbe9aea795fd23ecb46ba1fbd28988b7d528b6a1f6e7f8a1122a9025aa"
     "5d67552ed2e841039034dc8245ee1746ab4f00614366ca7018386041f9b0a96f"
     "e57ad9eb8465dcb6b85eeb2f1be11a37aee7b1f24e5f99155b39ff0679e664d5"
     "1fc299974daa270e19d1b206ec40aab3a0ce35e7c6a7d389b156bcd411e41a3c"
     "e33404f86acc5162322ac573f53a409333a1629b8cf1d284299743421af03368"
     "bff7b12e47678c11d4ca664ef81e49c6bfaea997493de89bcd5b39ae24311242"
     "298f62f457484d770462f0f5b2f4a7a7240e82e24785481266b7ed2527ecf4bd"
     "d8b8c09a745470f6c088dce5df19ade98894f4ced69ce32d53aded94d512826d"
     "e223120256455daba01b6c68510b48fac813acab05c314510e47aea377b23634"
     "de385583975ed8e83b71b212b3094ee74785834718e2413bc3acff36224fba8d"
     "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d"
     "fffef514346b2a43900e1c7ea2bc7d84cbdd4aa66c1b51946aade4b8d343b55a"
     "daa27dcbe26a280a9425ee90dc7458d85bd540482b93e9fa94d4f43327128077"
     "c20728f5c0cb50972b50c929b004a7496d3f2e2ded387bf870f89da25793bb44"
     "d2ab3d4f005a9ad4fb789a8f65606c72f30ce9d281a9e42da55f7f4b9ef5bfc6"
     default))
 '(safe-local-variable-values
   '((projectile-project-test-cmd . "./build/lsm_tree")
     (projectile-project-compilation-cmd . "cmake --build build"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
