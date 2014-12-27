;;; python-customisation

;;; Commentary:
;;

;;; Code:

;; I use company mode for code completion ui across the board, with anacoda
;; providing the suggestions at back end
(require 'company-anaconda)
(add-to-list 'company-backends 'company-anaconda)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook '(lambda ()
                               (setq python-indent 4)))

;; Use IPython as the default python interpreter, if available
(when (executable-find "ipython")
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(defalias 'ipython 'run-python)

;; TODO: execute line in python buffer - python-shell-send-region
(setq-default flycheck-flake8-maximum-line-length 110)

(provide 'python-customisations)
;;; python-customisations.el ends here
