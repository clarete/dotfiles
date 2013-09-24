;; No bars. Doing this first to avoid showing/hidding delay on startup
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Adding marmalade as a repo to the package module. Also, we can't
;; configure any package before requiring `package.el`
(require 'package)
(add-to-list
 'package-archives
 '("marmalade" .
   "http://marmalade-repo.org/packages/")
 '("melpa" .
   "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; setting up a color theme
(require 'monokai-theme)

;; Adding my default elisp package path
(add-to-list 'load-path "~/.emacs.d/elisp")

;; utf-8 for good (is there any other encoding related var I could set?)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq current-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Basic config for columns
(column-number-mode)

;; No backup files
(setq make-backup-files nil)

;; No splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)

;; No f*cking bell
(setq ring-bell-function 'ignore)

;; Always do syntax highlighting
(global-font-lock-mode 1)

;; Font face/size
(add-to-list 'default-frame-alist '(font . "6x13"))

;;; Also highlight parens
(setq show-paren-delay 0 show-paren-style 'parenthesis)
(show-paren-mode 1)

;; highlight mark region
(transient-mark-mode 1)

;; gdb
(setq gdb-many-windows 1)

;; There's no place like home
(setq default-directory "~/")

;; Set up a 79-column rule
(setq-default fill-column 79)
(add-to-list 'load-path "~/.emacs.d/elisp/fill-column-indicator")
(require 'fill-column-indicator)
(setq fci-style 'rule)
(setq fci-rule-color "#33333")
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda () (fci-mode t)))
(global-fci-mode t)

;; show line numbers
(require 'linum)
(global-linum-mode 1)

;; scroll smoothly
(setq scroll-conservatively 10000)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Do not wrap lines
(setq-default truncate-lines t)

;; clipboard
(setq x-select-enable-clipboard t)

;; Whitespaces
(setq show-trailing-whitespace t)

;; Reloading the buffer instead of pissing me off with "what should I
;; do" questions
(defun ask-user-about-supersession-threat (filename)
  (revert-buffer t t)
  (message "This buffer was refreshed due to external changes"))

;; ---- key bindings ---

;; comments
(global-set-key [(ctrl c) (c)] 'comment-region)
(global-set-key [(ctrl c) (d)] 'uncomment-region)

;; join lines
(global-set-key [(ctrl J)] '(lambda () (interactive) (join-line -1)))

;; moving from one window to another
(global-set-key [(ctrl <)] 'next-multiframe-window)
(global-set-key [(ctrl >)] 'previous-multiframe-window)

;; moving from one frame to another
(global-set-key [(C-tab)] 'other-window)
(global-set-key [(shift C-tab)] '(lambda () (interactive) (other-window -1)))

;; scrolling without changing the cursor
(global-set-key [(meta n)] '(lambda () (interactive) (scroll-up 1)))
(global-set-key [(meta p)] '(lambda () (interactive) (scroll-down 1)))

;; scrolling other window
(global-set-key [(meta j)] '(lambda () (interactive) (scroll-other-window 1)))
(global-set-key [(meta k)] '(lambda () (interactive) (scroll-other-window -1)))

;; Mac specific stuff
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  ;; sets fn-delete to be right-delete
  (global-set-key [kp-delete] 'delete-char)
  (menu-bar-mode 1))

;; ssh and local sudo/su
(require 'tramp)

;; mutt and muttrc modes
(require 'muttrc-mode)
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;; A simple way to insert license headers
(require 'xlicense)

;; lua mode
(require 'lua-mode)

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; nginx
(add-to-list 'auto-mode-alist '("nginx.conf$" . nginx-mode))

;; html mode
(add-hook 'html-mode-hook (lambda() (setq sgml-basic-offset 4)))

;; css config
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level 4)

;; javascript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-consistent-level-indent-inner-bracket-p 1)
(setq js2-pretty-multiline-decl-indentation-p 1)
(setq js2-bounce-indent-p t)

;; sass mode
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(setq sass-indent-offset 4)

;; less mode
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
(setq less-compile-at-save nil)

;; Web mode
(add-to-list 'load-path "~/.emacs.d/elisp/web-mode")
(require 'web-mode)
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (set-face-attribute 'web-mode-doctype-face nil :foreground
                      (face-foreground font-lock-function-name-face))
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground
                      (face-foreground font-lock-variable-name-face))
  (set-face-attribute 'web-mode-html-attr-value-face nil :foreground
                      (face-foreground font-lock-type-face)))
(add-hook 'web-mode-hook  'web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Markdown mode
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook '(lambda() (flyspell-mode)))

;; Vala mode
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

;; CoffeScript mode
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(defun coffee-custom () "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook '(lambda() (coffee-custom)))

;; Auto complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-dwim 2)
(ac-config-default)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; Loading YAS personal snippets
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)

;; Configuring the dropdown list, submodule used by yasnippet
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt))

;; Esk search!
(add-to-list 'load-path "~/.emacs.d/elisp/esk")
(require 'esk)
(global-set-key "\M-s" 'esk-find-file)
(global-set-key "\M-\S-s" 'esk-find-in-project)
(setq esk-find-binary "gfind")

;; Some git shortcuts
(defun git () (interactive) (magit-status "."))
(defun git-blame () (interactive) (mo-git-blame-current))

;; Python stuff
;; ------------

;; Jedi mode for python. $ pip install jedi epc
(add-to-list 'load-path "~/.emacs.d/elisp/emacs-jedi")
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys nil)  ;; TODO: bind jedi stuff to some nice shortcuts

;; Pyflakes stuff
(require 'flymake-cursor)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "flake8"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(setq flymake-gui-warnings-enabled nil)
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Customizing colors used in diff mode
(defun custom-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute
   'diff-added nil :foreground "green")
  (set-face-attribute
   'diff-removed nil :foreground "red")
  (set-face-attribute
   'diff-changed nil :foreground "purple"))
(eval-after-load "diff-mode" '(custom-diff-colors))

;; Loading some custom functions after loading everything else
(load "~/.emacs.d/defuns.el")

;; Enabling the server mode by default
(server-mode)
