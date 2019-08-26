(setq user-full-name "EndlessDex"
      user-mail-address "ampdexter@gmail.com")

(require 'package)
(setq package-enable-at-startup nil)
(unless (assoc-default "melpa-stable" package-archives)
  (add-to-list 'package-archives
               '("melpa" . "http://stable.melpa.org/packages/") t))
(unless (assoc-default "marmalade" package-archives)
  (add-to-list 'package-archives
               '("marmalade" . "htt://marmalade-repo.org/packages/") t))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

;; Lisp autocompilation
(use-package auto-compile
  :config
  (auto-compile-on-load-mode t)
  (auto-compile-on-save-mode t)
  (setq load-prefer-newer t))

;; Lisp lists (needed for some reason)
(use-package dash)

;; Assign backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Keep permenant command history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode t)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-molokai t))

;; Hide toolbar and menu
(tool-bar-mode -1)
(menu-bar-mode -1)

;;Highlight line
;; (global-hl-line-mode t)
(add-hook 'prog-mode-hook 'hl-line-mode)

;; Show line numbers
(global-linum-mode t)

(add-hook 'after-change-major-mode-hook
          '(lambda ()
             (linum-mode
              (if (or
                   (equal major-mode 'text-mode)
                   (equal major-mode 'term-mode)
                   (equal major-mode 'help-mode))
                  0 1))))

;; Show column number
(column-number-mode t)

;; Edit cursor
(blink-cursor-mode 0)
(when (display-graphic-p)
  (setq-default cursor-type 'bar))
(setq x-stretch-cursor t)

;; Highlight everywhere
(global-font-lock-mode t)

;; Maximize # of colors
(setq font-lock-maximum-decoration t)

;; Sentences end with a single space
(setq sentence-end-double-space nil)

;; yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; hide the startup message
(setq inhibit-startup-message t)


;; Better builtin terminal
(use-package multi-term
  :config
  (require 'multi-term)
  (setq multi-term-program "/usr/bin/zsh")
  (add-hook 'term-mode-hook
            (lambda ()
              (setq term-buffer-maximum-size 10000)
              (setq show-trailing-whitespace nil))))

;; Make multi-term default
(multi-term)
(add-to-list 'frame-notice-user-settings
             '(switch-to-buffer "*terminal<1>*"))

;; Better scrolling
(setq scroll-conservatively 10000)
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)
(global-set-key (kbd "<mouse-6>") (lambda () (interactive) nil))
(global-set-key (kbd "<mouse-7>") (lambda () (interactive) nil))

;; Highlight matching delimiters
;; (show-paren-mode t)

;; Delimiter pairing
(electric-pair-mode t)

;; Delimiter coloring
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


;; UTF-8 encoding
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Default tab width
(setq-default tab-width 2)

;; Always indent when going to newline
(global-set-key (kbd "RET") 'newline-and-indent)

;; Delete selected text when typing
(delete-selection-mode t)

;; Make minibuffer better size
(setq resize-mini-windows t)
(setq max-mini-window-height 0.33)

;; Follow symlinks when C-x C-f
(setq vc-follow-symlinks t)

;; Multiple Cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; Return to place in file
(use-package saveplace
  :config (save-place-mode))

;; Better file completion
(use-package ido
  :config
  (ido-mode t)
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-ignore-extensions t))

;; Vertical menus
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode t)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  (setq ido-vertical-show-count t))

;; Better Ido
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode t))

;; Insert hyphen for space in smex and ido
(use-package ido-complete-space-or-hyphen)

;; Better menu completion
(use-package smex
  :bind (("M-x" . smex)
         ("M-S-x" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :config
  (setq smex-save-file "~/.emacs.d/.smex-items")
  (smex-initialize))

;; Undo trees
(use-package undo-tree
  :diminish undo-tree-mode
  :bind (("C-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo))
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;; Same file same buffer
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Show colors not hex
(use-package rainbow-mode)

;; Python development environment
(use-package elpy
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3"))

;; Flycheck syntax checking
(use-package flycheck
  :config
  (global-flycheck-mode t)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-clang-language-standard "c++1y")
  (use-package flycheck-pos-tip
    :config
    (flycheck-pos-tip-mode t)))

(use-package company
  :config
  (global-company-mode t)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-tooltip-limit 10)
  (setq company-show-numbers t)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t))

;;Indent buffer
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defvar dont-untabify-modes
  '(makefile-makepp-mode makefile-bsdmake-mode
                         makefile-imake-mode makefile-automake-mode
                         makefile-mode makefile-gmake-mode))

;; Replace tabs with spaces before saving (except above)
(defun untabify-buffer ()
  (interactive)
  (unless (member major-mode dont-untabify-modes)
    (untabify (point-min) (point-max))))
(add-hook 'before-save-hook 'untabify-buffer)

;; Remove trailing whitespace before saving
(defun dewhite-buffer ()
  (interactive)
  (unless (member major-mode '(markdown-mode))
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'dewhite-buffer)

;; Better commenting and binding
(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(define-key global-map "\M-;" 'comment-or-uncomment-region-or-line)

;; Goes to beginning of text not first column
(defun smarter-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  ;; Move cursor
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(define-key global-map [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; Make newline from anywhere
(defun make-newline ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(define-key global-map (kbd "C-o") 'make-newline)

;; Make previous newline from anywhere
(defun make-prev-newline ()
  (interactive)
  (forward-line -1)
  (end-of-line)
  (make-newline))
(define-key global-map (kbd "C-S-o") 'make-prev-newline)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "dark violet"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "white")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(package-selected-packages
   (quote
    (multiple-cursors multi-term yasnippet-snippets use-package undo-tree smex rainbow-mode rainbow-delimiters ido-vertical-mode ido-completing-read+ ido-complete-space-or-hyphen flycheck-pos-tip elpy doom-themes company-jedi auto-compile))))
