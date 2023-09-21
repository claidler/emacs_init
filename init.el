;; init.el

;; Package Management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; UI
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Treesitter
(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; Theme
(use-package catppuccin-theme
  :ensure t)
(load-theme 'catppuccin :no-confirm)

;; JavaScript and TypeScript
(use-package rjsx-mode
  :ensure t
  :mode ("\\.js\\'" . rjsx-mode))

(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;; Auto-completion
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t))

;; Linting
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Prettier
(use-package prettier-js
  :ensure t
  :hook ((js2-mode . prettier-js-mode)
         (web-mode . prettier-js-mode))
  :config
  (setq prettier-js-args '("--tab-width" "2"
                           "--single-quote"
                           "--trailing-comma" "all"
                           "--no-semi")))

;; Navigation
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq
        projectile-require-project-root nil))
(use-package ivy
  :ensure
  )

;; Terminal
(use-package vterm
    :ensure t)

;; GPT
(use-package gptel
  :ensure t)

;; LSP
(use-package eglot
  :ensure t)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(typescript-mode . ("typescript-language-server" "--stdio"))))

 (add-hook 'typescript-mode-hook 'eglot-ensure)


(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ivy projectile apheleia tree-sitter-langs tree-sitter catppuccin-theme prettier-js flycheck web-mode rjsx-mode use-package typescript-mode smartparens prettier exec-path-from-shell eshell-vterm elixir-mode eglot doom-themes company-quickhelp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
