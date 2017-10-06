; FROM http://aaronbedra.com/emacs.d/
(setq user-full-name "Camila Scatolini")
(setq user-mail-address "camila.scatolini@gmail.com")

(require 'cl)

(setenv "GOPATH" "/Users/cscatolini/Code/go")

(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" magit f)))

(defvar cscatolini/packages '(ac-slime
                          auto-complete
                          autopair
                          ess
                          f
                          feature-mode
                          flycheck
                          go-autocomplete
                          go-eldoc
                          go-mode
                          graphviz-dot-mode
                          haml-mode
                          htmlize
                          magit
                          markdown-mode
                          marmalade
                          org
                          powerline
                          rvm
                          smex
                          solarized-theme
                          web-mode
                          writegood-mode
                          yaml-mode)
  "Default packages")

(defun cscatolini/packages-installed-p ()
  (loop for pkg in cscatolini/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (cscatolini/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg cscatolini/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq tab-width 2
      indent-tabs-mode nil)

 ;; if indent-tabs-mode is off, untabify before saving
 (add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                         (untabify (point-min) (point-max)))))

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


(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-linum-mode t)
(setq linum-format "%d ")
;; (setq linum-format "%4d \u2502 ") ;; with a solid line separator
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

(defvar cscatolini/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path cscatolini/vendor-dir)

(dolist (project (directory-files cscatolini/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-show-log t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-deadlines t)
(setq org-agenda-files (list "~/Dropbox/org/personal.org"))

(require 'org)
(require 'org-install)
(require 'org-habit)
(add-to-list 'org-modules "org-habit")
(setq org-habit-preceding-days 7
      org-habit-following-days 1
      org-habit-graph-column 80
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)

(require 'ob)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (ditaa . t)
   (plantuml . t)
   (dot . t)
   (ruby . t)
   (js . t)
   (C . t)))

(add-to-list 'org-src-lang-modes (quote ("dot". graphviz-dot)))
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure
  '((:results . "silent") (:tangle . "yes")))

(defun org-babel-execute:clojure (body params)
  (lisp-eval-string body)
  "Done!")

(provide 'ob-clojure)

(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook (lambda ()
                                          (condition-case nil
                                              (org-display-inline-images)
                                            (error nil)))
          'append)

(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

(define-skeleton skel-org-block-elisp
  "Insert an emacs-lisp block"
  ""
  "#+begin_src emacs-lisp\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "elsrc" "" 'skel-org-block-elisp)

(define-skeleton skel-org-block-js
  "Insert a JavaScript block"
  ""
  "#+begin_src js\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "jssrc" "" 'skel-org-block-js)

(define-skeleton skel-header-block
  "Creates my default header"
  ""
  "#+TITLE: " str "\n"
  "#+AUTHOR: Aaron Bedra\n"
  "#+EMAIL: aaron@aaronbedra.com\n"
  "#+OPTIONS: toc:3 num:nil\n"
  "#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://thomasf.github.io/solarized-css/solarized-light.min.css\" />\n")

(define-abbrev org-mode-abbrev-table "sheader" "" 'skel-header-block)

(define-skeleton skel-org-html-file-name
  "Insert an HTML snippet to reference the file by name"
  ""
  "#+HTML: <strong><i>"str"</i></strong>")

(define-abbrev org-mode-abbrev-table "fname" "" 'skel-org-html-file-name)

(define-skeleton skel-ngx-config
  "Template for NGINX module config file"
  ""
  "ngx_addon_name=ngx_http_" str  "_module\n"
  "HTTP_MODULES=\"$HTTP_MODULES ngx_http_" str "_module\"\n"
  "NGX_ADDON_SRCS=\"$NGX_ADDON_SRCS $ngx_addon_dir/ngx_http_" str "_module.c\"")

(define-abbrev fundamental-mode-abbrev-table "ngxcnf" "" 'skel-ngx-config)

(define-skeleton skel-ngx-module
  "Template for NGINX modules"
  ""
  "#include <nginx.h>\n"
  "#include <ngx_config.h>\n"
  "#include <ngx_core.h>\n"
  "#include <ngx_http.h>\n\n"

  "ngx_module_t ngx_http_" str "_module;\n\n"

  "static ngx_int_t\n"
  "ngx_http_" str "_handler(ngx_http_request_t *r)\n"
  "{\n"
  >"if (r->main->internal) {\n"
  >"return NGX_DECLINED;\n"
  "}" > \n
  \n
  >"ngx_log_error(NGX_LOG_ERR, r->connection->log, 0, \"My new module\");\n\n"
  > _ \n
  >"return NGX_OK;\n"
  "}" > "\n\n"

  "static ngx_int_t\n"
  "ngx_http_"str"_init(ngx_conf_t *cf)\n"
  "{\n"
  >"ngx_http_handler_pt *h;\n"
  >"ngx_http_core_main_conf_t *cmcf;\n\n"

  >"cmcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_core_module);\n"
  >"h = ngx_array_push(&cmcf->phases[NGX_HTTP_ACCESS_PHASE].handlers);\n\n"

  >"if (h == NULL) {\n"
  >"return NGX_ERROR;\n"
  "}" > \n
  \n
  >"*h = ngx_http_"str"_handler;\n\n"

  >"return NGX_OK;\n"
  "}" > \n
  \n
  "static ngx_http_module_t ngx_http_"str"_module_ctx = {\n"
  >"NULL,                 /* preconfiguration */\n"
  >"ngx_http_"str"_init,  /* postconfiguration */\n"
  >"NULL,                 /* create main configuration */\n"
  >"NULL,                 /* init main configuration */\n"
  >"NULL,                 /* create server configuration */\n"
  >"NULL,                 /* merge server configuration */\n"
  >"NULL,                 /* create location configuration */\n"
  >"NULL                  /* merge location configuration */\n"
  "};" > \n
  \n

  "ngx_module_t ngx_http_"str"_module = {\n"
  >"NGX_MODULE_V1,\n"
  >"&ngx_http_"str"_module_ctx,  /* module context */\n"
  >"NULL,                        /* module directives */\n"
  >"NGX_HTTP_MODULE,             /* module type */\n"
  >"NULL,                        /* init master */\n"
  >"NULL,                        /* init module */\n"
  >"NULL,                        /* init process */\n"
  >"NULL,                        /* init thread */\n"
  >"NULL,                        /* exit thread */\n"
  >"NULL,                        /* exit process */\n"
  >"NULL,                        /* exit master */\n"
  >"NGX_MODULE_V1_PADDING\n"
  "};" >)

(require 'cc-mode)
(define-abbrev c-mode-abbrev-table "ngxmod" "" 'skel-ngx-module)

(define-skeleton skel-ngx-append-header
  "Template for header appending function for NGINX modules"
  ""
  "static void append_header(ngx_http_request_t *r)\n"
  "{\n"
  > "ngx_table_elt_t *h;\n"
  > "h = ngx_list_push(&r->headers_out.headers);\n"
  > "h->hash = 1;\n"
  > "ngx_str_set(&h->key, \"X-NGINX-Hello\");\n"
  > "ngx_str_set(&h->value, \"Hello NGINX!\");\n"
  "}\n")

(define-abbrev c-mode-abbrev-table "ngxhdr" "" 'skel-ngx-append-header)

(setq org-ditaa-jar-path "~/.emacs.d/vendor/ditaa0_9.jar")

(setq org-plantuml-jar-path "~/.emacs.d/vendor/plantuml.jar")

(setq deft-directory "~/Dropbox/deft")
(setq deft-use-filename-as-title t)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

(setq column-number-mode t)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(require 'autopair)

(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode
                   clojure-mode))

(defvar lisp-power-map (make-keymap))
(define-minor-mode lisp-power-mode "Fix keybindings; add power."
  :lighter " (power)"
  :keymap lisp-power-map
  (paredit-mode t))
(define-key lisp-power-map [delete] 'paredit-forward-delete)
(define-key lisp-power-map [backspace] 'paredit-backward-delete)

(defun cscatolini/engage-lisp-power ()
  (lisp-power-mode t))

(dolist (mode lisp-modes)
  (add-hook (intern (format "%s-hook" mode))
            #'cscatolini/engage-lisp-power))

(setq inferior-lisp-program "clisp")
(setq scheme-program-name "racket")

(require 'auto-complete-config)
(ac-config-default)

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

;; gomode config
(add-hook 'go-mode-hook
  (lambda ()
   (setq-default)
   (setq tab-width 4)
   (setq standard-indent 4)
   (setq indent-tabs-mode t)
   (setq untabify-this-buffer nil)))

(with-eval-after-load 'go-mode
   (require 'go-autocomplete))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(add-to-list 'exec-path "/Users/cscatolini/Code/go/bin")
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)

  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go vet"))


  ; godef key bindings
  (local-set-key (kbd "M-?") 'godef-jump)
  (local-set-key (kbd "M-.") 'pop-tag-mark)
  )

(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)
; -----------------------------------------------------------------------------------------

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(setq-default show-trailing-whitespace t)

(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

(require 'f)

(setq eshell-visual-commands
      '("less" "tmux" "htop" "top" "bash" "zsh" "fish"))

(setq eshell-visual-subcommands
      '(("git" "log" "l" "diff" "show")))

;; Prompt with a bit of help from http://www.emacswiki.org/emacs/EshellPrompt
(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun eshell/abbr-pwd ()
  (let ((home (getenv "HOME"))
        (path (eshell/pwd)))
    (cond
     ((string-equal home path) "~")
     ((f-ancestor-of? home path) (concat "~/" (f-relative path home)))
     (path))))

(defun eshell/my-prompt ()
  (let ((header-bg "#161616"))
    (concat
;     (with-face user-login-name :foreground "#dc322f")
;     (with-face (concat "@" hostname) :foreground "#268bd2")
;     " "
     (with-face (eshell/abbr-pwd) :foreground "#008700")
     (if (= (user-uid) 0)
         (with-face "#" :foreground "red")
       (with-face "$" :foreground "#2345ba"))
     " ")))

(setq eshell-prompt-function 'eshell/my-prompt)
(setq eshell-highlight-prompt nil)
(setq eshell-prompt-regexp "^[^#$\n]+[#$] ")

(setq eshell-cmpl-cycle-completions nil)

(require 'powerline)
(powerline-default-theme)

(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

(setq web-mode-style-padding 2)
(setq web-mode-script-padding 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(add-hook 'ruby-mode-hook
          (lambda ()
            (autopair-mode)))

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))

;; (rvm-use-default) ;; This is causing a 1.5 second slow down to startup, disabling for now

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2))

(add-hook 'coffee-mode-hook 'coffee-custom)

(defun js-custom ()
  "js-mode-hook"
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'js-custom)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")
(setq markdown-css-paths `(,(expand-file-name "markdown.css" cscatolini/vendor-dir)))

(setq idris-interpreter-path "/usr/local/bin/idris")

(define-derived-mode cpsa-mode scheme-mode
  (setq mode-name "CPSA")
  (setq cpsa-keywords '("defmacro" "defprotocol" "defrole" "defskeleton" "defstrand"))
  (setq cpsa-functions '("cat" "hash" "enc" "string" "ltk" "privk" "pubk" "invk" "send" "recv"  "non-orig" "uniq-orig" "trace" "vars"))
  (setq cpsa-types '("skey" "akey" "name" "text"))
  (setq cpsa-keywords-regexp (regexp-opt cpsa-keywords 'words))
  (setq cpsa-functions-regexp (regexp-opt cpsa-functions 'words))
  (setq cpsa-types-regexp (regexp-opt cpsa-types 'words))
  (setq cpsa-font-lock-keywords
        `(
          (,cpsa-keywords-regexp . font-lock-keyword-face)
          (,cpsa-functions-regexp . font-lock-function-name-face)
          (,cpsa-types-regexp . font-lock-type-face)))
  (setq font-lock-defaults '((cpsa-font-lock-keywords))))

(add-to-list 'auto-mode-alist '("\\.cpsa$" . cpsa-mode))

(require 'go-autocomplete)

(add-hook 'go-mode-hook
          (lambda ()
            (go-eldoc-setup)
            (add-hook 'before-save-hook 'gofmt-before-save)))

(if window-system
    (load-theme 'solarized-light t)
  (load-theme 'afternoon t))
;; wombat
;; seti
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "++ ")
 '(git-gutter:deleted-sign "-- ")
 '(git-gutter:modified-sign "== ")
 '(package-selected-packages
   (quote
    (toggle-window column-marker exec-path-from-shell json-mode multiple-cursors git-gutter flycheck-gometalinter flycheck-pycheckers go-complete golint govet go-mode afternoon-theme yaml-mode writegood-mode web-mode solarized-theme smex seti-theme rvm powerline marmalade markdown-mode magit htmlize haml-mode graphviz-dot-mode go-eldoc go-autocomplete flycheck feature-mode f ess autopair ac-slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ;; flycheck-gometalinter
;; ;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
;; (setq flycheck-gometalinter-vendor t)
;; ;; only show errors
;; (setq flycheck-gometalinter-errors-only t)
;; ;; only run fast linters
;; (setq flycheck-gometalinter-fast t)
;; ;; use in tests files
;; (setq flycheck-gometalinter-test t)
;; ;; disable linters
;; (setq flycheck-gometalinter-disable-linters '("gotype" "gocyclo"))
;; ;; Only enable selected linters
;; (setq flycheck-gometalinter-disable-all t)
;; (setq flycheck-gometalinter-enable-linters '("golint"))
;; ;; Set different deadline (default: 5s)
;; (setq flycheck-gometalinter-deadline "10s")

;; git-gutter
(require 'git-gutter)
;; If you enable global minor mode
(global-git-gutter-mode t)
;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)
(global-set-key (kbd "C-x C-g") 'git-gutter)
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")

;; multiple-cursors
;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
(global-set-key (kbd "C-x C-l") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

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

;; ;; disable json-jsonlist checking for json files
;; (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;;     '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

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

(require 'whitespace)
(setq
 whitespace-style '(face empty tabs lines-tail trailing)
 whitespace-line-column 100)
(global-whitespace-mode t)

(require 'column-marker)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 100)))
