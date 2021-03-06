;;; javascript-customisations

;;; Commentary:
;;
;;

;;; Code:

;; TODO: javascript auto-completion in js2-mode is not case sensitive
(require 'js2-mode)
(require 'company-dabbrev-code)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'company-dabbrev-code-modes 'js2-mode)

;; JavaScript is a camel-case language, so let's switch on subword-mode
;; to make kill/delete word delete a sub-word when it sees a camel-case word
(add-hook 'js2-mode-hook 'subword-mode)


;; tern provides superb javascript code analysing
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook
          (lambda () (local-set-key (kbd "<f3>") 'tern-find-definition)))

(add-to-list 'company-backends 'company-tern)

(provide 'javascript-customisations)
;;; javascript-customisations.el ends here
