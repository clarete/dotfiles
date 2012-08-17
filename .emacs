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
;(add-to-list 'default-frame-alist '(font . "Monospace 8"))
;(add-to-list 'default-frame-alist '(font . "Monospace 12"))
(add-to-list 'default-frame-alist '(font . "6x13"))

;;; Also highlight parens
(setq show-paren-delay 0 show-paren-style 'parenthesis)
(show-paren-mode 1)

;; no bars
(scroll-bar-mode)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; highlight mark region
(transient-mark-mode 1)

;; gdb
(setq gdb-many-windows 1)

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
  (global-set-key [kp-delete] 'delete-char)) ;; sets fn-delete to be right-delete

;; scrolling other window
;(global-set-key "\M-\C-n" 'scroll-other-window)
;(global-set-key "\M-\C-p" 'scroll-other-window-down)

;; new window
;(global-set-key "\C-x\C-n"  'make-frame)

;; other window
;(global-set-key [C-tab] 'other-window)

;; mutt
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;; Mode Configuration

;; css config
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level 4)

;; javascript config
(setq js2-consistent-level-indent-inner-bracket-p 1)
(setq js2-pretty-multiline-decl-indentation-p 1)
(add-to-list 'load-path "~/.emacs.d/elisp/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; jade mode
(add-to-list 'load-path "~/.emacs.d/elisp/jade-mode")
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; sass and haml mode
(add-to-list 'load-path "~/.emacs.d/elisp/sass-mode")
(add-to-list 'load-path "~/.emacs.d/elisp/haml-mode")
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;; less mode
(add-to-list 'load-path "~/.emacs.d/elisp/less-mode")
(require 'less-mode)
(add-to-list 'auto-mode-alist '("\\.less$" . less-mode))
(setq less-compile-at-save nil)

;; Markdown mode
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; vala mode
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

;; -- currently disabled modules --

;; lua mode
;; (require 'lua-mode)

;; Django mode
;; (add-to-list 'load-path "~/.emacs.d/django-mode")
;; (require 'django-html-mode)
;; (require 'django-mode)
;; (yas/load-directory "~/.emacs.d/django-mode/snippets")
;; (add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode))

;; rhtml mode
;; (add-to-list 'load-path "~/.emacs.d/rhtml")
;; (require 'rhtml-mode)
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))

;; CoffeScript mode
;; (add-to-list 'load-path "~/.emacs.d/elisp/coffee-mode")
;; (require 'coffee-mode)
;; (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
;; (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
;; (defun coffee-custom ()
;;   "coffee-mode-hook"
;;  (set (make-local-variable 'tab-width) 2))
;; (add-hook 'coffee-mode-hook
;;   '(lambda() (coffee-custom)))

;; emms
;; (require 'emms-setup)
;; (require 'emms-info-libtag)
;; (require 'emms-player-mpg321-remote)
;; (emms-devel)
;; (emms-default-players)
;; (push 'emms-player-mpg321-remote emms-player-list)
;; (push 'emms-player-mplayer emms-player-list)
;; (push 'emms-player-mplayer-playlist emms-player-list)
