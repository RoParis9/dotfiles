;;; -*- lexical-binding: t; -*-

  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'use-package)

  (setq straight-use-package-by-default t)

;; Better default modes
(electric-pair-mode t)
(show-paren-mode 1)

(setq-default indent-tabs-mode nil)

(save-place-mode t)

(savehist-mode t)
;;keep track of open files
(recentf-mode t)
;;Keep files up-to-date if modified outside emacs
(global-auto-revert-mode t)

;; Add unique buffer names in the minibuffer where there are many
;; identical files. This is super useful if you rely on folders for
;; organization and have lots of files with the same name,
;; e.g. foo/index.ts and bar/index.ts.
(require 'uniquify)
(require 'project)

;; to open links when press enter on org-mode
(setq org-return-follows-link t)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Remove Welcome Screen
(setq inhibit-startup-message t)

(setq custom-safe-themes t)

(setq frame-resize-pixelwise t)

;;Fixing the Scratch buffer
(setq initial-scratch-message "")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
          #'(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; Remove Menus and Scroll Bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(auto-save-mode 1)

;;show recent files
(recentf-mode 1)

(add-to-list 'load-path "/home/rodrigo/.emacs.d/lisp")

;;autosave
;;(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;;remember cursor place
(save-place-mode 1)

;; instead of yes or no i want y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;;auto save
(setq make-backup-files nil
      auto-save-default t)

;;Highlight current line
(global-hl-line-mode 1)

;;save backup directory
(setq backup-directory-alist '(("." . "~/.saves")))

;;delete selected words
(delete-selection-mode 1)


;; Display line numbers only when in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)


(setq uniquify-buffer-name-style 'forward
    window-resize-pixelwise t
    frame-resize-pixelwise t
    load-prefer-newer t
    backup-by-copying t
    ;; Backups are placed into your Emacs directory, e.g. ~/.config/emacs/backups
    backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
    ;; I'll add an extra note here since user customizations are important.
    ;; Emacs actually offers a UI-based customization menu, "M-x customize".
    ;; You can use this menu to change variable values across Emacs. By default,
    ;; changing a variable will write to your init.el automatically, mixing
    ;; your hand-written Emacs Lisp with automatically-generated Lisp from the
    ;; customize menu. The following setting instead writes customizations to a
    ;; separate file, custom.el, to keep your init.el clean.
    custom-file (expand-file-name "custom.el" user-emacs-directory))

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                vterm-mode-hook
                eshell-mode-hook))
  (add-hook mode #'(lambda () (display-line-numbers-mode 0))))

;;highlight parenteses
(show-paren-mode 1)
(setq-default fill-column 80)

(add-to-list 'default-frame-alist '(alpha-background . 85))

        (use-package doom-themes
          :straight t
          :config
          (doom-themes-visual-bell-config)
          (load-theme 'doom-palenight))

       (use-package doom-modeline
          :ensure t
          :hook (after-init . doom-modeline-mode)
          :custom
          (doom-modeline-major-mode-icon t)
          (doom-modeline-major-mode-color-icon t)
          (doom-modeline-buffer-state-icon t)
          (doom-modeline-lsp-icon t)
          (doom-modeline-lsp t)
          (doom-modeline-time-live-icon t)
          (doom-modeline-time-analogue-clock t)
          (doom-modeline-buffer-name t)
          (doom-modeline-column-zero-based t)
          (doom-modeline-time-icon t)
          (doom-modeline-minor-modes nil)
          (doom-modeline-icon t)
          (doom-modeline-buffer-file-name-style 'truncate-except-project)
          (doom-modeline-lsp t))

        ;;Dark Themes that i like: dracula | material-dark | gruvbox | molokai | monokai-pro| palenight | xcode | vibrant 
        ;;Light Themes that i like: modus-operandi | doom-one-light | kaolin-light |

        (use-package all-the-icons
          :straight t)

        ;;all-the-icons on vertico
        (use-package all-the-icons-completion
          :straight t
          :after (marginalia all-the-icons)
          :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
          :init
          (all-the-icons-completion-mode))

       (set-face-attribute
        'default
        nil
        :height 120
        :family "Fira Code Nerd Font"
        :weight 'medium
        :width 'normal)

    (use-package beacon
       :straight t)

     (beacon-mode 1)

     ;;emacs dashboard
     (use-package dashboard
       :straight t
       :init
       (progn
         (setq dashboard-items '((recents .  1)
                       (projects . 1)))
         (setq dashboard-banner-logo-title "Lock in")
         (setq dashboard-set-file-icons t)
         (setq dashboard-set-heading-icons t)
         (setq dashboard-startup-banner '1)
         (setq dashboard-items '((recents  . 5)
                   (bookmarks . 5)
                   (projects . 5)
                   (agenda . 1))))
       :config
       (dashboard-setup-startup-hook))

  ;;tabs on top of the buffer
  (use-package centaur-tabs
    :straight t
    :bind
    ("C-c n" . centaur-tabs-forward)
    ("C-c b" . centaur-tabs-forward)
    :config
    (setq centaur-tabs-set-bar 'over
    centaur-tabs-set-icons t
    centaur-tabs-style 'bar
    centaur-tabs-gray-out-icons 'buffer
    centaur-tabs-height 30
    centaur-tabs-set-modified-marker t
    centaur-tabs-modifier-marker "*")
    (centaur-tabs-mode t))

(defun disable-centaur-tabs-on-dashboard ()
  "Disable Centaur Tabs on the dashboard."
  (when (string-equal (buffer-name) "*dashboard*")
    (centaur-tabs-local-mode 1)))  ;; Enable local mode to disable tabs

;; Add the hook for when the dashboard is displayed
(add-hook 'dashboard-mode-hook 'disable-centaur-tabs-on-dashboard)

(use-package gcmh
  :straight t
  :diminish
  :init (setq gc-cons-threshold most-positive-fixnum)
  :hook (emacs-startup . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024)))

(use-package treemacs
  :straight t
  :bind
  (:map global-map
        ("C-\\" . treemacs)
        ("C-c q" . treemacs)
        ("C-0" . treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(use-package treemacs-evil
  :after (treemacs evil)
  :straight t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :straight t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :straight t)

(use-package treemacs-magit
  :after (treemacs magit)
  :straight t)

(use-package treemacs-all-the-icons
  :straight t
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-all-the-icons
  :straight t
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package which-key
  :straight t
  :init
  (which-key-mode t))

(use-package evil
    :straight t
    :config
    :init
    (setq evil-want-keybinding nil)
    (setq evil-want-integration t)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    :config
    (evil-mode)
    (evil-set-undo-system 'undo-redo)
    ;; Use visual line motions even outside visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
    (evil-define-key 'normal lsp-mode-map "gr" 'lsp-find-references)
    (evil-define-key 'normal lsp-mode-map "gd" 'lsp-find-definition)
    (evil-define-key 'normal lsp-mode-map "r" 'lsp-rename)
    (evil-set-initial-state 'dashboard-mode 'normal)
    (evil-set-initial-state 'message-buffer-mode 'normal))

  (use-package evil-nerd-commenter
    :straight t
    :bind
    ("C-c c" . evilnc-comment-or-uncomment-lines))

  
(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))

(use-package vertico
  :straight t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

;; Searching package 
(use-package consult
  :straight t
  :bind(("C-s" . consult-line)
        ("C-c b" . consult-buffer)
        ("C-c g" . consult-ripgrep))
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package orderless
  :straight t
  :init (setq completion-styles '(orderless)))

(use-package savehist
  :init
  (savehist-mode))

;; (use-package corfu
;;     :straight t
;;     :config
;;     (setq tab-always-indent 'complete)
;;     :custom
;;     (corfu-cycle t)                 ; Allows cycling through candidates
;;     (corfu-auto t)                  ; Enable auto completion
;;     (corfu-auto-prefix 0)
;;     (corfu-auto-delay 0)
;;     (corfu-popupinfo-delay '(0.5 . 0.2))
;;     (corfu-preview-current 'insert) ; insert previewed candidate
;;     (corfu-preselect 'prompt)
;;     (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
;;     ;; Optionally use TAB for cycling, default is `corfu-complete'.
;;     :bind (:map corfu-map
;;                 ("TAB"        . corfu-next)
;;                 ([tab]        . corfu-next)
;;                 ("S-TAB"      . corfu-previous)
;;                 ("RET"        . corfu-complete))
;;     :init
;;     (global-corfu-mode)
;;     (corfu-history-mode)
;;     (corfu-popupinfo-mode))

;;   (use-package kind-icon
;;     :straight t
;;     :after corfu
;;     :custom
;;     (kind-icon-blend-background t)
;;     (kind-icon-default-face 'corfu-default) ; only needed with blend-background
;;     :config
;;     (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package company-mode
    :straight t
    :bind (:map company-active-map ("<tab>" . company-complete-selection))
    :config
    (setq company-idle-delay 0.1
          company-tooltip-align-annotations 't
          company-tooltip-flip-when-above t
          company-vscode-dark-icons-margin t
          company-minimun-prefix-length 1
          company-frontends '(company-preview-frontend)
          company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend))
    :init
    (add-hook 'after-init-hook 'global-company-mode))

  (use-package company-box
    :straight t
    :hook (company-mode . company-box-mode))

(use-package magit
  :straight t
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :straight t
  :config
  (global-git-gutter-mode 1))

(use-package git-link
  :straight t
  :custom
  (git-link-use-commit t)
  (git-link-use-single-line-number t)
  :commands (git-link git-link-commit git-link-homepage))

(use-package projectile
  :straight t
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1))

(global-set-key (kbd "C-c p") 'projectile-find-file)

(use-package pdf-tools
  :straight t)

(use-package vterm
  :straight t
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-copy-mode t)
  (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
  :straight t
  :bind
  (("C-`"   . vterm-toggle)
   ("C-c t" . vterm-toggle)
   :map vterm-mode-map
   ("<C-return>" . vterm-toggle-insert-cd))
   :config
   (add-to-list 'display-buffer-alist
	           '("\*vterm\*"
	             (display-buffer-in-side-window)
	             (window-height . 0.3)
	             (side . bottom)
	             (slot . 0))))

(use-package multi-vterm
  :straight t)

(use-package org
  :config
  (setq org-agenda-files '("~/Agenda/agenda.org"))
  (setq org-ellipsis " ▾"))

(global-set-key (kbd "C-c o a") #'org-agenda)

(use-package org-roam
    :straight t
    :init
    (setq org-roam-v2-ack t)
    :custom
    (org-roam-directory "~/OrgNotes")
    :bind (("C-c o l" . org-roam-buffer-toggle)
           ("C-c o f" . org-roam-node-find)
           ("C-c o i" . org-roam-node-insert)
           ("C-c o I" . org-roam-node-inert-immediate))
    :config
    (org-roam-setup))

  ;; Bind this to C-c n I
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

  (use-package org-roam-ui
    :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package lsp-mode
    :straight t
    :hook((lsp-java . lsp))
    :bind(:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
    :config
    (lsp-enable-which-key-integration t)
    :custom
    (lsp-headerline-breadcrum-enable t))

 (use-package lsp-ui
     :straight t
     :commands (lsp-ui-mode)
     :custom
     ;;Sideline
     (lsp-ui-sideline-show-diagnostics nil)
     (lsp-ui-sidelin-enable t)
     (lsp-ui-sideline-show-hover nil)
     (lsp-ui-sideline-delay 0)
     (lsp-ui-update-mode 'line)
     ;;Documentantion
     (lsp-ui-doc-enable t)
     (lsp-ui-doc-header t)
     (lsp-ui-doc-delay 0.2)
     (lsp-ui-doc-position 'bottom)
     ;; IMenu
     (lsp-ui-imenu-window-width 0)
     (lsp-ui-imenu--custom-mode-line-format nil)
     :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
   :straight t
   :commands lsp-treemacs-erros-list)

(use-package dap-mode
  :straight t
  :after lsp-mode
  :hook ((lsp-mode . dap-mode)
         (lsp-mode . dap-ui-mode))
  :custom
  (lsp-enable-dap-auto-configure t)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (dap-auto-configure-mode)
  :bind
  (("<f7>" . dap-step-in)
   ("<f8>" . dap-next)
   ("<f9>" . dap-continue)))

(require 'dap-firefox)

(defun rk/rustic-mode-hook ()
    ;; so that run C-c C-c C-r works without having to confirm, but don't try to
    ;; save rust buffers that are not file visiting. Once
    ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
    ;; no longer be necessary.
    (when buffer-file-name
    (setq-local buffer-save-without-query t))
    (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package rustic
    :ensure
    :bind (:map rustic-mode-map
                ("M-j" . lsp-ui-imenu)
                ("M-?" . lsp-find-references)
                ("C-c C-c l" . flycheck-list-errors)
                ("C-c C-c a" . lsp-execute-code-action)
                ("C-c C-c r" . lsp-rename)
                ("C-c C-c q" . lsp-workspace-restart)
                ("C-c C-c Q" . lsp-workspace-shutdown)
                ("C-c C-c s" . lsp-rust-analyzer-status))
    :config
    ;; uncomment for less flashiness
    (setq lsp-eldoc-hook nil)
    (setq lsp-enable-symbol-highlighting nil)
    (setq lsp-signature-auto-activate nil)

    ;; comment to disable rustfmt on save
    (setq rustic-format-on-save t)
    (add-hook 'rustic-mode-hook 'lsp)
    (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(use-package jtsx
  :straight t
  :mode (("\\.jsx\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode))
   :custom
   (js-indent-level 2)
   (typescript-ts-mode-indent-offset 2)
   (jtsx-switch-indent-offset 0)
   (jtsx-indent-statement-block-regarding-standalone-parent nil)
   (jtsx-jsx-element-move-allow-step-out t)
   (jtsx-enable-jsx-electric-closing-element t)
   (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
   (jtsx-enable-jsx-element-tags-auto-sync t)
   (jtsx-enable-all-syntax-highlighting-features t)
   :config
   (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
    (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
    (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
    (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
    (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
    (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
    (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
    (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
    (define-key mode-map (kbd "C-c j d") 'jtsx-delete-jsx-node))
    
  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))

(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  (add-to-list 'auto-mode-alist '("\\.js\\" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))

(use-package typescript-mode
  :straight t
  :mode ("\\.ts\\'" . typescript-mode)
  :config
  (setq typescript-indent-level 2))

(use-package go-mode
    :straight t
    :hook (go-mode . lsp-deferred)
    :bind (:map go-mode-map
                ("C-c C-g" . gofmt))
    :config
    (require 'lsp-go)
    (setq lsp-go-analyses
            '((fieldalignment . t)
            (nilness . t)
            (unusedwrite . t)
            (unusedparams . t)))

(defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
;;(add-hook 'go-mode-hook #'yas-minor-mode)

;; GOPATH/bin
(add-to-list 'exec-path "~/go/bin")
(setq gofmt-command "goimports"))

(use-package lsp-java
   :straight t
   :config
   (add-hook 'java-mode-hook 'lsp)
   (add-hook 'java-mode-hook 'smartparens-mode)
   (add-hook 'java-mode-hook 'flycheck-mode)
   (require 'dap-java))

(use-package php-mode
   :straight t
   :mode
   ("\\.php'" . php-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)

(use-package web-mode
  :straight t
  :mode (("\\.html\\'" . web-mode)
         ("\\.blade.php\\'" . web-mode)
         ("\\.css\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-identation t)
  (setq web-mode-enable-auto-close t)
  (setq web-mode-enable-auto-pairing t))

(add-hook 'web-mode-hook #'lsp)

(add-hook 'web-mode-before-auto-complete-hooks 'corfu-mode-hook)

(use-package emmet-mode
  :straight t)
