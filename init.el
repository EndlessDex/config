;;; package --- Summery
;;; Commentary:
;;; Code:
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))

(defvar package-list
  '(
    auto-compile
    better-defaults
    company
    company-c-headers
    company-irony
    dash
    disaster
    flycheck
    flycheck-irony
    flycheck-pos-tip
    guide-key
    highlight-numbers
    ido-ubiquitous
    ido-vertical-mode
    irony
    multi-term
    multiple-cursors
    rainbow-delimiters
    smex
    visual-fill-column
    yasnippet
    ))

;; Update package databases
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install new packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Autocompile elisp files
(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; Follow symlinks when C-x C-f
(setq vc-follow-symlinks t)

(require 'better-defaults)

;;Make multiterm page default
;; (multi-term)
(setq inhibit-startup-screen t)
;; (add-to-list 'frame-notice-user-settings '(switch-to-buffer "*terminal<1>*"))

(elpy-enable)

;;Make cursor a bar instead of block
(setq-default cursor-type 'bar)

;;Highlight line
(global-hl-line-mode)

;;Enable syntax highlighting for numbers
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(defvar dont-indent-modes
  '(makefile-makepp-mode makefile-bsdmake-mode makefile-imake-mode makefile-automake-mode makefile-mode makefile-gmake-mode))

;;Replace tabs with spaces
(defun untabify-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (unless (member major-mode dont-indent-modes)
    (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'untabify-buffer)

(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region
     beg end)))
(define-key global-map "\M-;" 'comment-or-uncomment-region-or-line)

;;Newline and indent on enter key
(define-key global-map (kbd "RET") 'newline-and-indent)

;;Make prompts y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;;Indent buffer
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max) nil))

;;Turn on syntax hilighting
(global-font-lock-mode t)

;;Maximize # of colors
(setq font-lock-maximum-decoration t)

;;Set font
(setq default-frame-alist'((font . "DejaVu Sans Mono 12")))

(require 'ido)
(require 'ido-vertical-mode)
(require 'ido-ubiquitous)

;;Better file autocomplete everywhere
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)
(ido-ubiquitous-mode t)

;;Vertical file list
(ido-vertical-mode t)

;;Arrow keys to files
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

;;Show number of matching files
(setq ido-vertical-show-count t)

;;Autocomplete for commands
(require 'smex)
(smex-initialize)
(define-key global-map (kbd "M-x") 'smex)
(define-key global-map (kbd "M-X") 'smex-major-mode-commands)

;;Auto-pair border symbols
(electric-pair-mode t)

;;Show lin/cloumn number in status bar
(line-number-mode t)
(column-number-mode t)

;;Make newline from anywhere
(defun make-newline ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(define-key global-map (kbd "C-o") 'make-newline)

;;Make previous newline from anywhere
(defun make-prev-newline ()
  (interactive)
  (forward-line -1)
  (end-of-line)
  (make-newline))
(define-key global-map (kbd "C-S-o") 'make-prev-newline)

;;Coding autocompletion
(require 'cc-mode)
(require 'company)
(require 'company-c-headers)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)

;;Better C/C++ autocompletion
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backend (delete 'company-semantic company-backends))
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/6.1.1/")
(eval-after-load 'company
  '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))

(setq company-transformers '(company-sort-by-occurrence))

;;More coding autocompletion
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))

(defun my-irony-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;;Set coding style
(setq c-default-style "bsd")
;;indent set to 4 spaces
(setq c-basic-offset 4)
;;set switch indent
(c-set-offset 'case-label '+)

;;More error checking
(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-irony-setup))
(add-hook 'c++-mode-hook (lambda ()
                           (setq flycheck-clang-language-standard "c++1y"
                                 flycheck-clang-standard-library "libc++")
                           (setq flycheck-checker 'c/c++-clang)))

(defun smarter-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(define-key global-map [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

(require 'multi-term)
(setq multi-term-program "/bin/zsh")

(setq scroll-conservatively 10000)
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)

(require 'multiple-cursors)
(define-key global-map (kbd "C->") 'mc/mark-next-like-this)
(define-key global-map (kbd "C-<") 'mc/mark-previous-like-this)

;;Temporaraly disable terminal
(defun term-toggle-mode ()
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))
(define-key global-map (kbd "C-c t") 'term-toggle-mode)

(require 'visual-fill-column)
(add-hook 'text-mode-hook 'flyspell-mode)
(setq visual-fill-column-width 100)
(add-hook 'text-mode-hook 'turn-on-visual-fill-column-mode)
(setq flyspell-issue-message-flag nil)
(add-hook 'c++-mode-hook
          (lambda ()
            (flyspell-prog-mode)
            ; ...
            ))
(add-hook 'c-mode-hook
          (lambda ()
            (flyspell-prog-mode)
            ; ...
          ))
;;Deletes selected text when start typing
(delete-selection-mode t)

;;Display key binding cutocomplete
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)

(require 'yasnippet)
(yas-global-mode t)
(add-hook 'term-mode-hook
          (lambda()
            (yas-minor-mode -1)))

;; Add yasnippet support for all company ba;; ckends
;; ;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas)
          (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar 'company-mode/backend-with-yas company-backends))

(defun check-expansion() "check-expansion"
       (save-excursion
         (if (looking-at "\\_>") t
           (backward-char 1)
           (if (looking-at "\\.") t
             (backward-char 1)
             (if (looking-at "->") t nil)))))

(defun do-yas-expand () "Do-yas-expand."
       (let ((yas-fallback-behavior 'return-nil))
         (yas-expand)))

(defun tab-indent-or-complete () "Tab-indent-or-complete."
       (interactive)
       (cond
        ((minibufferp)
         (minibuffer-complete))
        ((eq major-mode 'term-mode)
         (term-send-raw-string "\t"))
        (t
         (indent-for-tab-command)
         (if (or (not yas-minor-mode)
                 (null (do-yas-expand)))
             (if (check-expansion)
                 (progn
                   (company-manual-begin)
                   (if (null company-candidates)
                       (progn
                         (company-abort)
                         (indent-for-tab-command)))))))))

(defun tab-complete-or-next-field () "Tab-complete-or-next-field."
       (interactive)
       (if (or (not yas-minor-mode)
               (null (do-yas-expand)))
           (if company-candidates
               (company-complete-selection)
             (if (check-expansion)
                 (progn
                   (company-manual-begin)
                   (if (null company-candidates)
                       (progn
                         (company-abort)
                         (yas-next-field))))
               (yas-next-field)))))

(defun expand-snippet-or-complete-selection () "Expand-snippet-or-complete-selection."
       (interactive)
       (if (or (not yas-minor-mode)
               (null (do-yas-expand))
               (company-abort))
           (company-complete-selection)))

(defun abort-company-or-yas () "Abort-company-or-yas."
       (interactive)
       (if (null company-candidates)
           (yas-abort-snippet)
         (company-abort)))

(define-key global-map [tab] 'tab-indent-or-complete)
(define-key global-map (kbd "TAB") 'tab-indent-or-complete)
(global-set-key [(control return)] 'company-complete-common)

(define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
(define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)

(define-key yas-minor-mode-map [tab] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(define-key yas-keymap [tab] 'tab-complete-or-next-field)
(define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
(define-key yas-keymap [(control tab)] 'yas-next-field)
(define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)

(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\(FIXME:\\|TODO:\\|BUG:\\|@todo\\|@param\\)" 1 font-lock-warning-face t)))))


(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(package-selected-packages
   (quote
    (highlight-numbers yasnippet visual-fill-column smex rainbow-delimiters powerline multiple-cursors multi-term ido-vertical-mode ido-ubiquitous guide-key golden-ratio flycheck-pos-tip flycheck-irony doom company-irony company-c-headers better-defaults)))
 '(rainbow-delimiters-max-face-count 8)
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "lime green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "medium blue"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "dark violet"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "magenta")))))

(defun maybe-delete-frame-buffer (frame)
  "When a dedicated FRAME is deleted, also kill its buffer.
A dedicated frame contains a single window whose buffer is not
displayed anywhere else."
  (let ((windows (window-list frame)))
    (when (eq 1 (length windows))
      (let ((buffer (window-buffer (car windows))))
        (when (eq 1 (length (get-buffer-window-list buffer nil t)))
          (kill-buffer buffer))))))
(add-to-list 'delete-frame-functions #'maybe-delete-frame-buffer)

;; C/C++ Disassembler
(require 'disaster)
(define-key c-mode-base-map (kbd "C-c d") 'disaster)

;;; init.el ends here
