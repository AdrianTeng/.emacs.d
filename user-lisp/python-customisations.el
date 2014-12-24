;;; python-customisation

;;; Commentary:
;;

;;; Code:
(require 'company-anaconda)
(add-to-list 'company-backends 'company-anaconda)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook '(lambda ()
                               (setq python-indent 4)))
;; TODO: Use executable-find to defun ipython
;;(defun ipython()
;;    (let (ipython (executable-find "ipython"))
;;        (call-process ipython))
;;)

(provide 'python-customisations)
;;; python-customisations.el ends here
