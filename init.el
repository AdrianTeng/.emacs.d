(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)


;; Quick reload of init.el

(global-set-key (kbd "<f12>") (lambda() (interactive)(load-file "~/.emacs.d/init.el")))

;; Save and reload all buffers when Emacs exit and start

(desktop-save-mode 1)

;;------------------------------------------------------------------------------
;; UI related
;;------------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/elpa/darcula-theme-20141211.128")

(require 'darcula-theme)

(add-to-list 'default-frame-alist '(font .  "DejaVu Sans Mono-10.5" ))
(set-frame-font   "DejaVu Sans Mono-10.5" nil t)

;; Disable toolbar
(tool-bar-mode 0)


;; indentation

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(defun indent-buffer ()
  "Indent the everything in the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(add-hook 'before-save-hook 'whitespace-cleanup)


;;------------------------------------------------------------------------------
;; File I/O
;;------------------------------------------------------------------------------
;; Recently opened files

(require 'recentf)

;; offer recently accessed files from the menu
(recentf-mode t)

;; remember this many files
(setq recentf-max-saved-items 200)

(defun ido-recentf-open ()
  "Use `ido-completing  -read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; Kill all buffers except the one you are opening
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))


;; Autosave and back file to be stored centrally
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name
                (concat user-emacs-directory "auto-save")) t)))

;; Open files in same repo

(require 'find-file-in-repository)

(global-set-key (kbd "C-S-r") 'find-file-in-repository)

;; git

(global-set-key (kbd "C-c g") 'magit-status)

;;------------------------------------------------------------------------------
;; Cursor moving
;;------------------------------------------------------------------------------

(require 'highlight-symbol)

(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)

(defun highlight-symbol-first ()
  "Jump to the first location of symbol at point."
  (interactive)
  (push-mark)
  (eval
   `(progn
      (goto-char (point-min))
      (search-forward-regexp
       (rx symbol-start ,(thing-at-point 'symbol) symbol-end)
       nil t)
      (beginning-of-thing 'symbol))))

(global-set-key (kbd "<f3>") 'highlight-symbol-first) ;; Clearly I used to use eclipse

(defun beginning-of-line-dwim ()
  "Toggles between moving point to the first non-whitespace character, and
    the start of the line."
  (interactive)
  (let ((start-position (point)))
    ;; see if going to the beginning of the line changes our position
    (move-beginning-of-line nil)

    (when (= (point) start-position)
      ;; we're already at the beginning of the line, so go to the
      ;; first non-whitespace character
      (back-to-indentation))))

(global-set-key (kbd "C-a") 'beginning-of-line-dwim)

;; Jump!
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;;-----------------------------------------------------------------------------
;; Generic Prog mode
;;-----------------------------------------------------------------------------

;; Autocomplete

(add-hook 'prog-mode-hook 'company-mode)
(global-set-key (kbd "M-SPC") 'company-complete)

;; Always enable flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Delimiter highlight
(show-smartparens-global-mode +1)
(smartparens-global-mode t)
(set-variable 'sp-autoescape-string-quote nil)

;;-----------------------------------------------------------------------------
;; Program language specifc
;;-----------------------------------------------------------------------------

;; emacs-lisp
(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(set-face-foreground 'rainbow-delimiters-depth-1-face "white")
(set-face-foreground 'rainbow-delimiters-depth-2-face "cyan")
(set-face-foreground 'rainbow-delimiters-depth-3-face "yellow")
(set-face-foreground 'rainbow-delimiters-depth-4-face "green")
(set-face-foreground 'rainbow-delimiters-depth-5-face "orange")
(set-face-foreground 'rainbow-delimiters-depth-6-face "purple")
(set-face-foreground 'rainbow-delimiters-depth-7-face "white")
(set-face-foreground 'rainbow-delimiters-depth-8-face "cyan")
(set-face-foreground 'rainbow-delimiters-depth-9-face "yellow")
(set-face-foreground 'rainbow-delimiters-unmatched-face "red")


;; javascript

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; python
(require 'company-anaconda)
(add-to-list 'company-backends 'company-anaconda)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)


(provide 'init)
;;; init.el ends here
