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

;; Keys
(global-set-key(kbd "<C-z>") nil)

;; System
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves" t)))
(setq create-lockfiles nil)

;; UI
(setq ring-bell-function 'ignore)
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
(use-package flymake
  :custom
  (flymake-fringe-indicator-position nil))

(use-package flymake-eslint
  :preface
  (defun me/flymake-eslint-enable-maybe ()
    (when-let* ((root (locate-dominating-file (buffer-file-name) "package.json"))
                (rc (locate-file ".eslintrc" (list root) '(".js" ".json"))))
      (make-local-variable 'exec-path)
      (push (file-name-concat root "node_modules" ".bin") exec-path)
      (flymake-eslint-enable))))

;; Prettier
(use-package prettier
  :ensure t)

;; Navigation
(use-package projectile
  :ensure t
  :config
  (setq projectile-indexing-method 'native)
  (setq projectile-sort-order 'recentf)
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/workspace/"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1))

;; Terminal
(use-package vterm
    :ensure t)

;; GPT
(use-package gptel
  :ensure t)

;; LSP
(use-package eglot
  ;; :straight nil
  :custom
  (eglot-autoshutdown t)
  :hook
  (eglot-managed-mode . me/flymake-eslint-enable-maybe)
  (typescript-ts-base-mode . eglot-ensure)
  :init
  (put 'eglot-server-programs 'safe-local-variable 'listp)
  :config
  ;; (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy)
  ;; (put 'eglot-error 'flymake-overlay-control nil)
  ;; (put 'eglot-warning 'flymake-overlay-control nil)
  (setq eglot-confirm-server-initiated-edits nil)
  (advice-add 'eglot--apply-workspace-edit :after #'me/project-save)
  (advice-add 'project-kill-buffers :before #'me/eglot-shutdown-project)
  :preface
  (defun me/eglot-shutdown-project ()
    "Kill the LSP server for the current project if it exists."
    (when-let ((server (eglot-current-server)))
      (ignore-errors (eglot-shutdown server)))))

(use-package eglot
  :ensure t)

;; Git
(use-package magit
  :ensure t)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(typescript-mode . ("typescript-language-server" "--stdio"))))

(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'flymake-eslint-enable)
(add-hook 'typescript-mode-hook 'prettier-mode)

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flymake-eslint ivy projectile apheleia tree-sitter-langs tree-sitter catppuccin-theme prettier-js flycheck web-mode rjsx-mode use-package typescript-mode smartparens prettier exec-path-from-shell eshell-vterm elixir-mode eglot doom-themes company-quickhelp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
