;;; init.el
;;
;;; Commentary:
;; The following external applications are being used:
;; jshint - by flycheck in js2-mode (javascript)
;; git - by magit
;; pandoc - for markdown-preview in markdown-mode (markdown)
;; ag - Silver Searcher - Replacement of rgrep (See https://github.com/ggreer/the_silver_searcher)

;;; Code:


(add-to-list 'load-path "~/.emacs.d/user-lisp/")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))



(package-initialize)


;; Quick reload of init.el

(global-set-key (kbd "<f12>") (lambda() (interactive)(load-file "~/.emacs.d/init.el")))


;;------------------------------------------------------------------------------
;; UI related
;;------------------------------------------------------------------------------

;; (add-to-list 'load-path "~/.emacs.d/elpa/darcula-theme-20141211.128")
;; (load-theme 'darcula)

(add-to-list 'load-path "~/.emacs.d/elpa/tangotango-theme-20141123.1354")
(load-theme 'tangotango t)

(add-to-list 'default-frame-alist '(font .  "DejaVu Sans Mono-10.5" ))
(set-frame-font "DejaVu Sans Mono-10.5" nil t)

;; Disable toolbar
(tool-bar-mode 0)

;;------------------------------------------------------------------------------
;; Emacs command line interaction
;;------------------------------------------------------------------------------

;; Smex provides fuzzy completion with M-x command calling
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; fuzzy completion for a bunch of commands. switch-to-buffer, for example.
(require 'ido)
(ido-mode t)


;; Tracking command usage
;;(keyfreq-mode) TODO: FIX

;;------------------------------------------------------------------------------
;; Editing
;;------------------------------------------------------------------------------
;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(setq indent-line-function 'insert-tab)

(defun indent-buffer ()
  "Indent the everything in the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(add-hook 'before-save-hook 'whitespace-cleanup)

;; line breaking
(setq-default fill-column '110)

;; overwrite when text is selected
(delete-selection-mode t)

;; spell checking
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Who uses C-z to minimise Emacs anyway?
(global-set-key (kbd "C-z") 'undo)

;; TODO: make backward-delete-word to delete up to end of previous word

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

;; If I want stuff to get into kill-ring, I'll mark-region -> kill-region,
;; rather than using C-delete | C-backspace
(global-set-key (kbd "<C-delete>") 'delete-word)
(global-set-key (kbd "<C-backspace>") 'backward-delete-word)

;; TODO: Write subword-delete

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

;; A very common default key-binding in modern IDE. I suppose with this, Emacs
;; is a 'modern IDE' too ;)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

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
      `((".*" . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name
                (concat user-emacs-directory "auto-save")) t)))

;; Open files in same repo

;;TODO: Fix. Does not search all files
(require 'find-file-in-project)

(global-set-key (kbd "C-S-r") 'find-file-in-project)

;; Reload from disk
(global-set-key (kbd "<f5>") 'revert-buffer)


;; Silver Searcher - a better version of rgrep
(global-set-key (kbd "C-<f5>") 'ag)

;; Lock files makes grunt go nuts
(setq create-lockfiles nil)

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
(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(setq ace-jump-mode-submode-list
      '(ace-jump-char-mode
        ace-jump-word-mode
        ace-jump-line-mode) )

(global-set-key (kbd "M-l") 'goto-line)

;; Have had enough of mistyping C-x o as C-x C-o
(global-set-key (kbd "C-x C-o") 'other-window)

;;-----------------------------------------------------------------------------
;; Generic Prog mode
;;-----------------------------------------------------------------------------

;; Autocomplete

(require 'company)
(add-hook 'text-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'company-mode)
(global-set-key (kbd "M-SPC") 'company-complete)
(setq-default company-dabbrev-downcase nil)

;; TODO: auto complete file dir

;; Always enable flycheck
(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Delimiter highlight
(require 'smartparens)
(show-smartparens-global-mode +1)
(smartparens-global-mode t)
(set-variable 'sp-autoescape-string-quote nil)

;; (foo bar) -> foo bar
(define-key smartparens-mode-map (kbd "M-s") 'sp-splice-sexp)

;; (foo bar) -> [foo bar]
(define-key smartparens-mode-map (kbd "M-S") 'sp-rewrap-sexp)

;; (foo) bar -> (foo bar)
(define-key smartparens-mode-map (kbd "<C-M-right>") 'sp-slurp-hybrid-sexp)


;; Vertical line indicator of exceeding line length of 110
(require 'column-marker)
(add-hook 'prog-mode-hook (lambda () (interactive) column-marker-1 111))

;; TODO: Begin new line with comment character if new line is inserted in comment block

(require 'yasnippet)
(yas-global-mode 1)

(add-hook 'prog-mode-hook 'hungry-delete-mode)

;;-----------------------------------------------------------------------------
;; Program language specifc
;;-----------------------------------------------------------------------------

(require 'python-customisations)
(require 'javascript-customisations)

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

;; html
(add-to-list 'company-dabbrev-code-modes 'html-mode)
;; less
(add-to-list 'company-dabbrev-code-modes 'less-css-mode)

;; Java
(add-hook 'java-mode-hook 'subword-mode)

;; Markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(if (executable-find "pandoc")
    (setq-default
     '(markdown-command (executable-find "pandoc"))))


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("c5898bd49bea497d5d356d698f82379c12c7325a3697604a9d1ad7f98c8e6647" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
