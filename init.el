(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)


(add-to-list 'load-path "~/.emacs.d/elpa/darcula-theme-20141211.128")

(require 'darcula-theme)

(add-to-list 'default-frame-alist '(font .  "DejaVu Sans Mono-10.5" ))
(set-frame-font   "DejaVu Sans Mono-10.5" nil t)


;; indentation

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(add-to-list 'load-path "~/.emacs.d/elpa/magit-20141214.1225")
