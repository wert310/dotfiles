(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USE-PACKAGE / MELPA / PACAKGE.EL

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))

(when (not (package-installed-p 'use-package))
      (package-refresh-contents)
      (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC

(defun minibuffer-keyboard-quit ()
  "http://github.com/davvil/.emacs.d/init.el"
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*")
      (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defmacro global-set-key-progn (key &rest body)
  (let ((s (gensym)))
    `(progn (defun ,s ()
              (interactive)
              ,@body)
            (global-set-key ,key ',s))))

(defun swap-buffers ()
  "Swap the current buffer and the buffer on the right or left
   side of the split."
  (interactive)
  (flet ((swap (other buffer)
               (set-window-buffer (selected-window) (window-buffer other))
               (set-window-buffer other buffer)
               (select-window other)))
    (let ((left   (windmove-find-other-window 'left))
          (right  (windmove-find-other-window 'right))
          (buffer (window-buffer (selected-window))))
      (cond (left  (swap left buffer))
            (right (swap right buffer))
            (t (error "Cannot find left or right split"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HIGHLIGHT TODOs (htodo-mode) based on superego.el

(require 'font-lock)

(defgroup htodo nil "Highlight TODOs" :group 'convenience :prefix 'htodo-)

(defvar htodo-face 'htodo-face)
(defface htodo-face
  '((t (:underline t :foreground "red" :background "yellow")))
  "htodo highlight face"
  :group 'htodo)

(defcustom htodo-regex (regexp-opt (list "TODO" "FIXME" "undefined"))
  "Regexp to match TODO-like keywords"
  :group 'htodo :type 'regexp)

(defvar htodo-keywords (list htodo-regex 0 'htodo-face 'prepend))

(defvar htodo-mode-line " hTODO")
(or (assq 'htodo-mode minor-mode-alist)
    (push '(htodo-mode htodo-mode-line)
                minor-mode-alist))

(defvar htodo-mode nil)
(defun htodo-mode (&optional arg)
  (interactive "P")
  (make-local-variable 'htodo-mode)
  (when (not arg)
    (setq arg (if htodo-mode -1 1)))
  (cond ((>= arg 0)
         (font-lock-add-keywords nil (list htodo-keywords))
         (when font-lock-mode
           (font-lock-fontify-buffer)
           (font-lock-mode 1))
         (setq htodo-mode t))
        ((< arg 0)
         (font-lock-remove-keywords nil (list htodo-keywords))
         (when font-lock-mode
           (font-lock-fontify-buffer))
         (setq htodo-mode nil))))

(define-globalized-minor-mode global-htodo-mode
  htodo-mode htodo-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUICK YES

(require 'advice)

(defun quick-yes () (interactive) (insert "yes") (exit-minibuffer))
(defun quick-no  () (interactive) (insert "no")  (exit-minibuffer))

(defvar quick-yes-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "M-y") 'quick-yes)
    (define-key m (kbd "M-n") 'quick-no)
    m))

(defadvice yes-or-no-p (around quick-yes activate)
  (set-keymap-parent quick-yes-map minibuffer-local-map)
  (let ((minibuffer-local-map quick-yes-map))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES

;; Do not show warnings on startup
(setq byte-compile-warnings nil)

(use-package evil
  :init
  (evil-mode 1)
  (evil-set-initial-state 'term-mode 'emacs)
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (define-key evil-window-map (kbd "<right>") 'evil-window-right)
  (define-key evil-window-map (kbd "<up>") 'evil-window-up)
  (define-key evil-window-map (kbd "<down>") 'evil-window-down)
  (define-key evil-window-map (kbd "<left>") 'evil-window-left))

(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package highlight-symbol
  :init
  (global-set-key (kbd "C-x p") 'highlight-symbol)
  (global-set-key (kbd "C-x n") 'highlight-symbol-next)
  (global-set-key (kbd "C-x N") 'highlight-symbol-prev)
  (setq-default highlight-symbol-idle-delay 1)
  (define-globalized-minor-mode global-highlight-symbol-mode
    highlight-symbol-mode highlight-symbol-mode)
  (global-highlight-symbol-mode 1))

(use-package engine-mode
  :init
  (engine-mode 1)
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d"))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode)))

(use-package neotree
  :init
  (global-set-key [f9] 'neotree-toggle)
  (setq neo-smart-open t)
  (evil-set-initial-state 'neotree-mode 'emacs))


(use-package rust-mode)
(use-package cargo :init (add-hook 'rust-mode-hook 'cargo-minor-mode))
(use-package tuareg)
(use-package column-enforce-mode)
(use-package flymake)
(use-package flymake-cursor)
(use-package markdown-mode)

;; Show compiler warnings
(setq byte-compile-warnings t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFAULTS

;;; set current theme to the bundled "misterioso"
(load-theme 'misterioso)

;;; Bracket highlighting
(show-paren-mode 1)

;;; Hide toolbar
(tool-bar-mode -1)

;;; Hide scrollbar
(scroll-bar-mode -1)

;;; Hide menu-bar
(menu-bar-mode -1)

;; Default Font
(setq wert-font "Hack 9"); monospace 9
(set-face-attribute 'default nil :font wert-font)
(add-to-list 'default-frame-alist `(font .  ,wert-font))

;; Fulscreen on startup
(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; Line and column Numbers
(global-linum-mode t)
(setq column-number-mode t)

;;; Linum mode disabled for org, term
(setq linum-disabled-modes
      '(org-mode term-mode calendar-mode calc-mode doc-view-mode speedbar-mode))
(defun linum-on () ;redefining linum-on
  (unless (or (minibufferp)
              (member major-mode linum-disabled-modes))
    (linum-mode 1)))

;;; Disable backup
(setq make-backup-files nil)

;;; Indent Using only spaces
(setq-default indent-tabs-mode nil)

;;; Scrolling 1 line at time
(setq scroll-step            1
      scroll-conservatively  10000)

;;; Inhibit splash screen
(setq inhibit-startup-message t)

;; Default interpreters for inferior modes
(setq scheme-program-name "csi")
(setq inferior-lisp-program "ecl")
(setq python-shell-interpreter "python2")

;;; Dired Omit Mode (hide .files)
(require 'dired-x)
(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
(setq-default dired-omit-mode t)

;;; Org mode new entities
(require 'org)
(add-to-list
 'org-entities-user
 '("bowtie" "\\bowtie" t "&#x22C8;" "[bowtie]" "[bowtie]" "⋈"))

;;; Shift Select Org Mode
(setq-default org-support-shift-select t)

;;; Global hTODO mode
(global-htodo-mode 1)

;;; Global auto-revert-mode
(global-auto-revert-mode 1)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYS

(global-set-key (kbd "C-<") 'dabbrev-expand)
(global-set-key (kbd "s-<left>") 'move-beginning-of-line)
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "S-<f9>") 'yank) ; shift - vol down
(global-set-key (kbd "S-<backspace>") 'kill-region)
(global-set-key-progn (kbd "s-<up>") (goto-char 1))
(global-set-key-progn (kbd "s-<down>") (goto-char (buffer-end 1)))
(global-set-key-progn (kbd "s-<backspace>") (delete-char 1))
(global-set-key-progn (kbd "s-S-<backspace>")
                      (delete-region (line-beginning-position)
                                     (line-end-position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (neotree markdown-mode web-mode use-package tuareg highlight-symbol haskell-mode flymake-cursor evil engine-mode column-enforce-mode cargo))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
