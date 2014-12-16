(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)


(add-to-list 'load-path "~/.emacs.d/elpa/darcula-theme-20141211.128")

(require 'darcula-theme)

(add-to-list 'default-frame-alist '(font .  "DejaVu Sans Mono-11" ))
(set-frame-font   "DejaVu Sans Mono-11" nil t)
