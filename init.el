(add-to-list 'load-path "~/.emacs.d/lisp/")

;; AOT compile everything
(setq comp-deferred-compilation t)

;; Performance variables.
(setq gc-cons-threshold (* 1024 1024 1024)
      jit-lock-defer-time 0.05
      read-process-output-max (* 1024 1024)
      package-native-compile t)

(let ((gc-cons-percentage .6)
      (gc-cons-threshold most-positive-fixnum))

  ;; Disable that pesky echo message
  (put 'inhibit-startup-echo-area-message 'saved-value t)
  (setq inhibit-startup-echo-area-message (user-login-name))

  ;; Set repositories
  (require 'package)
  (setq-default
   load-prefer-newer t
   package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
  (package-initialize)

  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'use-package)
  (require 'use-package)
  (straight-use-package 'org-mode)
  (setq straight-use-package-by-default 1)

  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
  (garbage-collect))
