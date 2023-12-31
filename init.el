;;; init.el -- Emacs configuration
;;; Commentary:
;;; Code:

;; Package Management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
			 ("gnu-devel" . "https://elpa.gnu.org/devel/")))
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/extensions")
(use-package quelpa
  :ensure t)
(use-package quelpa-use-package
  :ensure t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

;; Keys
(global-set-key(kbd "<C-z>") nil)

;; System
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves" t)))
(setq create-lockfiles nil)
(when (display-graphic-p)
  (add-hook 'emacs-startup-hook 'toggle-frame-maximized))
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;;performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; UI
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
;; dired-style buffer menu
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;font
(set-face-attribute 'default nil :font "JetBrains Mono:pixelsize=13")
(setq-default line-spacing 2)
;;minibuffer
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(help-key-binding ((t (:inherit fixed-pitch :background "grey19" :foreground "DeepSkyBlue1" :box (:line-width (-1 . -1) :color "grey35")))))
 '(highlight ((t (:background "#DA89A1" :foreground "white")))))

;; Treesitter
;; FIRST: git clone https://github.com/casouri/tree-sitter-module
;;        bash batch.sh
;; THEN : sudo cp dist/* /usr/local/lib
;; FINALLY:
(setq treesit-extra-load-path '("/usr/local/lib"))
(setq treesit-font-lock-level 4)

;; Theme
(use-package catppuccin-theme
  :ensure t)
(load-theme 'catppuccin :no-confirm)

;; LSP

;; FIRST: pip3 install epc orjson sexpdata six paramiko rapidfuzz
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :init
  (global-lsp-bridge-mode))

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion))
  :hook (prog-mode . copilot-mode))
 
;; Format
(use-package prettier
  :ensure t)
(add-hook 'after-init-hook #'global-prettier-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-javascript-eslint-executable "eslint_d"))

;; Editing
(quelpa '(combobulate :fetcher github-ssh :repo "mickeynp/combobulate"))
(setq combobulate-key-prefix "C-c o")
(add-hook 'tsx-ts-mode-hook 'combobulate-mode)
(add-hook 'typescript-ts-mode-hook 'combobulate-mode)

;; Navigation
(fido-vertical-mode 1)

;; Terminal
(use-package vterm
    :ensure t)

;; Org
(add-hook 'org-mode-hook 'visual-line-mode)

;; GPT
(use-package gptel
  :ensure t
  :init
  (setq gptel-directives
        '((default . "You are a programmer. Give concise answers. Answer with just code if possible. Only use step by step if required for complex calculations. Your output will be used in org mode - syntax highlight appropriately.")
          ))
  (setq gptel-model "gpt-4")
  :config
  (defun gptel-open-and-clear ()
    (interactive)
    (let ((gptel-buffer (call-interactively 'gptel)))
      (with-current-buffer gptel-buffer
        (delete-region (point-min) (point-max)))))
  :bind (("C-c l" . gptel)
         ("C-c L" . gptel-open-and-clear)
         ("C-c k" . curser-code-replace)))

;; git clone git@github.com:claidler/curser-el.git
(load-file "~/.emacs.d/extensions/curser-el/curser.el")

;; GIT
(use-package magit
  :ensure t)
(quelpa '(code-review :fetcher github-ssh :repo "phelrine/code-review" :branch "fix/closql-update"))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6ca663019600e8e5233bf501c014aa0ec96f94da44124ca7b06d3cf32d6c5e06" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" default))
 '(global-font-lock-mode t)
 '(gptel-default-mode 'org-mode)
 '(gptel-model "gpt-4")
 '(gptel-temperature 0.0)
 '(package-selected-packages
   '(combobulate quelpa-use-package flymake-css lsp-mode flymake-eslint ivy catppuccin-theme use-package typescript-mode smartparens prettier exec-path-from-shell eshell-vterm eglot company-quickhelp)))

