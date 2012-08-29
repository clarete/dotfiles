(add-to-list 'load-path "~/.emacs.d/elisp")

(column-number-mode)
(setq fill-column 59)
(setq-default fill-column 72)

;; No backup files
(setq make-backup-files nil)

;; splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)

;; encoding
(setq current-language-environment "UTF-8")

;; setting up a color theme
(add-to-list 'load-path "~/.emacs.d/elisp/color-theme")
(require 'color-theme)
(eval-after-load "color-theme"
 '(progn
    (color-theme-initialize)
    (color-theme-tty-dark)))

;; ssh and local sudo/su
(require 'tramp)

;; Always do syntax highlighting
(global-font-lock-mode 1)

;; Font face/size
(add-to-list 'default-frame-alist '(font . "6x13"))

;;; Also highlight parens
(setq show-paren-delay 0 show-paren-style 'parenthesis)
(show-paren-mode 1)

;; no bars
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; highlight mark region
(transient-mark-mode 1)

;; gdb
(setq gdb-many-windows 1)

;; There's no place like home
(setq default-directory "~/")

;; show line numbers
(require 'linum)
(global-linum-mode 1)

;; scroll smoothly
(setq scroll-conservatively 10000)

(put 'upcase-region 'disabled nil)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Better use the default C-x *left* and C-x *right*
;(global-set-key [(meta next)]  'next-buffer)
;(global-set-key [(meta prior)] 'previous-buffer)
;; (global-set-key "\M-\C-n" 'next-buffer)
;; (global-set-key "\M-\C-p" 'previous-buffer)

; clipboard
(setq x-select-enable-clipboard t)

;; ---- key bindings ---

;; comments
(global-set-key [(ctrl c) (c)] 'comment-region)
(global-set-key [(ctrl c) (d)] 'uncomment-region)

;; scrolling

(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 1))
(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 1))

(global-set-key [(meta n)] 'gcm-scroll-down)
(global-set-key [(meta p)] 'gcm-scroll-up)

;; Mac specific stuff
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (menu-bar-mode 1))

;; scrolling other window
;(global-set-key "\M-\C-n" 'scroll-other-window)
;(global-set-key "\M-\C-p" 'scroll-other-window-down)

;; new window
;(global-set-key "\C-x\C-n"  'make-frame)

;; other window
;(global-set-key [C-tab] 'other-window)

;; Adding marmalade as a repo to the package module
(require 'package)
(add-to-list
 'package-archives
 '("marmalade" .
   "http://marmalade-repo.org/packages/")
 '("melpa" .
   "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; mutt
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;; Mode Configuration

;; css config
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level 4)

;; javascript config
(setq js2-consistent-level-indent-inner-bracket-p 1)
(setq js2-pretty-multiline-decl-indentation-p 1)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; sass and haml mode
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

;; less mode
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
(setq less-compile-at-save nil)

;; Markdown mode
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; vala mode
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

;; lua mode
(require 'lua-mode)

;; CoffeScript mode
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(defun coffee-custom () "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook '(lambda() (coffee-custom)))

;; Auto complete
(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-dwim 2)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; Muttrc mode
(require 'muttrc-mode)

;; Configuring the dropdown list, submodule used by yasnippet
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt))

;; Personal snippets
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)
