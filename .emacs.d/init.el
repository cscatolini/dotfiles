;;; init.el --- Emacs config 2026 -*- lexical-binding: t -*-

;;; ── Package bootstrap ──────────────────────────────────────────────────────

(require 'package)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; use-package is built-in since Emacs 29
(require 'use-package)
(setq use-package-always-ensure t)

;;; ── Core settings ──────────────────────────────────────────────────────────

(setq inhibit-startup-message t
      make-backup-files        nil
      auto-save-default        nil
      column-number-mode       t
      ring-bell-function       'ignore
      use-short-answers        t)   ; replaces (defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode          nil
              tab-width                 2
              fill-column               100
              show-trailing-whitespace  t)

(menu-bar-mode        -1)
(delete-selection-mode t)
(global-auto-revert-mode t)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;; ── Built-in visual improvements ───────────────────────────────────────────

;; Line numbers (replaces deprecated linum-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-width 3)

;; Column indicator at 100 chars (replaces column-marker package)
(setq-default display-fill-column-indicator-column 100)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; which-key is built-in since Emacs 30 — shows keybinding hints
(which-key-mode t)

;;; ── Theme ──────────────────────────────────────────────────────────────────

(use-package nord-theme
  :config (load-theme 'nord t))

;;; ── Shell PATH on macOS ────────────────────────────────────────────────────

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;;; ── Completion: vertico + orderless + marginalia + consult ─────────────────
;;
;; Replaces the old ido + smex + helm stack.
;; vertico = vertical minibuffer UI
;; orderless = space-separated fuzzy matching ("go mod" finds "go.mod")
;; marginalia = annotations in completion lists
;; consult = enhanced commands (grep, find, buffer switch, etc.)

(use-package vertico
  :init (vertico-mode t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode t))

(use-package consult
  :bind
  ("C-x b"   . consult-buffer)
  ("C-x f"   . consult-find)
  ("C-c s"   . consult-ripgrep)
  ("M-g g"   . consult-goto-line)
  ("M-g i"   . consult-imenu))

;; In-buffer popup completion (replaces auto-complete)
(use-package corfu
  :custom
  (corfu-auto       t)
  (corfu-auto-delay 0.2)
  (corfu-cycle      t)
  :init (global-corfu-mode t))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;;; ── Tree-sitter ─────────────────────────────────────────────────────────────
;;
;; treesit-auto automatically uses *-ts-mode variants when grammars are
;; available. Run M-x treesit-auto-install-all to download grammars.

(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; ── LSP via eglot (built-in since Emacs 29) ────────────────────────────────
;;
;; Lighter and simpler than lsp-mode. Works out of the box with:
;;   Go:         gopls          → brew install gopls
;;   TypeScript: ts-ls          → npm i -g typescript-language-server typescript
;;   Python:     pyright        → pip install pyright

(use-package eglot
  :hook
  (go-ts-mode         . eglot-ensure)
  (python-ts-mode     . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode        . eglot-ensure)
  (js-ts-mode         . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  :config
  (setq-default eglot-workspace-configuration
                '(:gopls (:usePlaceholders t
                          :staticcheck    t
                          :gofumpt        t))))

;;; ── Go ──────────────────────────────────────────────────────────────────────

(add-hook 'go-ts-mode-hook
          (lambda ()
            (setq tab-width        4
                  indent-tabs-mode t)
            ;; format + organize imports on save via gopls
            (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

(use-package gotest
  :bind (:map go-ts-mode-map
         ("C-c t a" . go-test-current-project)
         ("C-c t f" . go-test-current-file)
         ("C-c t t" . go-test-current-test)
         ("C-c t r" . go-run)))

;;; ── TypeScript / JavaScript ─────────────────────────────────────────────────
;; typescript-ts-mode and js-ts-mode come built-in with tree-sitter support.
;; .tsx files → tsx-ts-mode (handled by treesit-auto)

(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

;;; ── Python ──────────────────────────────────────────────────────────────────
;; python-ts-mode is built-in. pyright handles completion + diagnostics via eglot.

;;; ── Git ─────────────────────────────────────────────────────────────────────

(use-package magit
  :bind ("C-x g" . magit-status))

;; diff-hl works better than git-gutter in terminal + Emacs 29+
(use-package diff-hl
  :hook (prog-mode            . diff-hl-mode)
  :hook (magit-pre-refresh    . diff-hl-magit-pre-refresh)
  :hook (magit-post-refresh   . diff-hl-magit-post-refresh))

;;; ── Navigation ──────────────────────────────────────────────────────────────

(use-package avy
  :bind
  ("C-x :" . avy-goto-char)
  ("C-x ;" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1))

(use-package projectile
  :init (projectile-mode t)
  :custom (projectile-enable-caching t)
  :bind-keymap ("C-c p" . projectile-command-map))

;;; ── Editing utilities ───────────────────────────────────────────────────────

(use-package multiple-cursors
  :bind
  ("C-x C-l" . mc/edit-lines)
  ("C-x >"   . mc/mark-next-like-this)
  ("C-x <"   . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package expand-region
  :bind ("C-\\" . er/expand-region))

;;; ── Code folding (built-in, replaces origami) ───────────────────────────────

(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "C-c f") #'hs-toggle-hiding)
(global-set-key (kbd "C-c F") #'hs-show-all)

;;; ── Untabify on save (except Makefiles and Go) ──────────────────────────────

(defvar-local untabify-this-buffer nil)
(defun untabify-all ()
  (and untabify-this-buffer (untabify (point-min) (point-max))))
(define-minor-mode untabify-mode
  "Untabify buffer on save." :lighter " untab"
  (setq-local untabify-this-buffer
              (not (derived-mode-p 'makefile-mode 'go-mode 'go-ts-mode)))
  (add-hook 'before-save-hook #'untabify-all nil t))
(add-hook 'prog-mode-hook #'untabify-mode)

;;; ── Utility functions ───────────────────────────────────────────────────────

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun revert-buffer-no-confirm ()
  (interactive)
  (revert-buffer t t))

;;; ── ANSI colors in compilation ──────────────────────────────────────────────

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;;; ── Global key bindings ─────────────────────────────────────────────────────

(global-set-key (kbd "RET")     #'newline-and-indent)
(global-set-key (kbd "C-c C-k") #'compile)
(global-set-key (kbd "C-x g")   #'magit-status)
(global-set-key (kbd "C-x i")   #'indent-buffer)
(global-set-key (kbd "M-r")     #'revert-buffer-no-confirm)
(global-set-key (kbd "C-x s")   #'sort-lines)
(global-set-key (kbd "C-x p")   #'previous-multiframe-window)

(global-set-key (kbd "C-c <left>")  #'windmove-left)
(global-set-key (kbd "C-c <right>") #'windmove-right)
(global-set-key (kbd "C-c <up>")    #'windmove-up)
(global-set-key (kbd "C-c <down>")  #'windmove-down)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
