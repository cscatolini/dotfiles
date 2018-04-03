;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.milkbox.net/packages/") t)

(add-to-list 'load-path "~/.emacs.d/custom/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm helm-ag helm-projectile projectile smex avy flycheck-gometalinter exec-path-from-shell fzf ag flycheck multiple-cursors origami magit go-guru go-autocomplete auto-complete go-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; general configuration
(delete-selection-mode t)
(setq column-number-mode t)
(setq make-backup-files nil)
(setq-default show-trailing-whitespace t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; smex - autocomplete M-x
(require 'smex)
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ido-mode - autocomplete C-c C-f
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; go related configuration
(setenv "GOPATH" "/Users/cscatolini/Code/go")
(add-to-list 'exec-path "/Users/cscatolini/Code/go/bin")

(add-hook 'go-mode-hook
          (lambda ()
            (setq-default)
            (setq tab-width 4)
            (setq standard-indent 4)
            (setq indent-tabs-mode t)
            (setq untabify-this-buffer nil)))

(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; jump to next compile error with C-x `
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; tabs
(setq tab-width 2
      indent-tabs-mode nil)
(defvar untabify-this-buffer)
(defun untabify-all ()
  "Untabify the current buffer, unless `untabify-this-buffer' is nil."
  (and untabify-this-buffer (untabify (point-min) (point-max))))
(define-minor-mode untabify-mode
  "Untabify buffer on save." nil " untab" nil
  (make-variable-buffer-local 'untabify-this-buffer)
  (setq untabify-this-buffer (not (derived-mode-p 'makefile-mode)))
  (add-hook 'before-save-hook #'untabify-all))
(add-hook 'prog-mode-hook 'untabify-mode)
(defvar untabify-this-buffer)
(defun untabify-all ()
  "Untabify the current buffer, unless `untabify-this-buffer' is nil."
  (and untabify-this-buffer (untabify (point-min) (point-max))))
(define-minor-mode untabify-mode
  "Untabify buffer on save." nil " untab" nil
  (make-variable-buffer-local 'untabify-this-buffer)
  (setq untabify-this-buffer (not (derived-mode-p 'makefile-mode)))
  (add-hook 'before-save-hook #'untabify-all))
(add-hook 'prog-mode-hook 'untabify-mode)

;; buffer
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

;; before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; line numbers
(global-linum-mode t)
(setq linum-format "%d ")

;; theme
(if window-system
    (load-theme 'solarized-light t)
    (load-theme 'seti t))
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; git gutters

(require 'git-gutter)
(global-git-gutter-mode t)
(git-gutter:linum-setup)
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")



;; multiple-cursors
;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
(global-set-key (kbd "C-x C-l") 'mc/edit-lines)
(global-set-key (kbd "C-x >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-x <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; flycheck
(require 'flycheck)
;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)
;; check on save
(setq flycheck-check-syntax-automatically '(mode-enabled save))
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")
;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
;; highlight per-line instead
(setq flycheck-highlighting-mode 'symbols)
(setq flycheck-indication-mode 'left-fringe)
;; highlight per-line instead
(setq flycheck-highlighting-mode 'symbols)
(setq flycheck-indication-mode 'left-fringe)

(set-face-attribute 'flycheck-warning nil
                    :background "#FF8C00"
                    :foreground "#ffffff"
                    :underline nil)

(set-face-attribute 'flycheck-error nil
                    :background "#800000"
                    :foreground "#ffffff"
                    :underline nil)

;; whitespace
(require 'whitespace)
(setq
 whitespace-style '(face empty tabs lines-tail trailing)
 whitespace-line-column 100)
(global-whitespace-mode t)
(setq whitespace-global-modes '(not go-mode))

;; column marker
(require 'column-marker)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 100)))

;; origami
(global-origami-mode t)

;; move lines up and down
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; fuzzy finder
(helm-mode t)
(projectile-global-mode t)
(setq projectile-enable-caching t)

;; key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x i") 'indent-buffer)
(global-set-key (kbd "C-x :") 'avy-goto-char)
(global-set-key (kbd "C-x ;") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "C-c f") 'origami-recursively-toggle-node)
(global-set-key (kbd "C-c g") 'origami-toggle-all-nodes)
(global-set-key (kbd "C-c s") 'origami-show-only-node)
(global-set-key (kbd "C-c u") 'origami-undo)
(global-set-key (kbd "C-c n") 'origami-show-node)
(global-set-key (kbd "C-c c") 'origami-close-node)
(global-set-key (kbd "C-c o") 'origami-open-node)
(global-set-key (kbd "C-c 0") 'origami-open-node-recursively)
(global-set-key (kbd "C-c a") 'origami-open-all-nodes)
(global-set-key (kbd "<ESC> <up>") 'move-line-up)
(global-set-key (kbd "<ESC> <down>") 'move-line-down)
(global-set-key (kbd "C-x r") 'revert-buffer)
(global-set-key (kbd "C-x i") 'indent-buffer)
(global-set-key (kbd "C-x s") 'sort-lines)
(global-set-key (kbd "C-x f") 'helm-projectile-find-file-dwim)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(provide 'init)
;;; init.el ends here
