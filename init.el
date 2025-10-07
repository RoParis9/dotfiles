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

;; Bootstrap use-package with straight.el
(straight-use-package 'use-package)

;; Tell use-package to use straight.el by default for all packages
(setq straight-use-package-by-default t)

;; Globally defer use-package unless explicitly demanded
(setq use-package-always-defer t)

;; Restore GC threshold after startup is complete
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 50 1024 1024))))

;; Optional: Native compilation settings (Emacs 28+)
(when (fboundp 'native-compile-available-p)
  (when (native-compile-available-p)
    (setq native-comp-jit-compilation t
          native-comp-async-jobs-number (max 1 (floor (/ (float (cpu-count)) 2)))
          native-comp-speed 2
          native-comp-safety 1
          native-comp-verbose nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (unless (file-exists-p (expand-file-name "init.eln" (car native-comp-eln-load-path)))
                  (native-compile-async user-emacs-directory 'recursively))))))

;; Core Emacs settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)

;; Add unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Project management
(require 'project)

;; Org-mode settings
(setq org-return-follows-link t)

;; Reduce output max for processes
(setq read-process-output-max (* 1024 1024))

(setq custom-safe-themes t)

(setq frame-resize-pixelwise t)

;; Fixing the Scratch buffer
(setq initial-scratch-message "")

;; Clean up *Completions* buffer
(add-hook 'minibuffer-exit-hook
          #'(lambda ()
              (let ((buffer "*Completions*"))
                (and (get-buffer buffer)
                     (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files
(add-hook 'window-setup-hook 'delete-other-windows)

(auto-save-mode t)

;; instead of yes or no i want y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;;highlight current line
(global-hl-line-mode 1)

;;auto-closing tags
(electric-pair-mode 1)

;; delete selected words
(delete-selection-mode 1)

;; Display line numbers only when in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(setq window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Disable line numbers in specific modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                vterm-mode-hook
                eshell-mode-hook))
  (add-hook mode #'(lambda () (display-line-numbers-mode 0))))

;; highlight parentheses
(show-paren-mode 1)

(setq-default fill-column 80)

;; Ensure we have treesit-auto for automatic grammar management
(use-package treesit-auto
  :straight (treesit-auto :type git :host github :repo "renzmann/treesit-auto")
  :init
  ;; Opção de auto-instalação: 'always, 'prompt ou nil
  (setq treesit-auto-install 'prompt)
  (setq treesit-extra-load-path '("~/.emacs.d/tree-sitter"))
  :config
  ;; Adiciona linguagens ao auto-mode-alist
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  ;; Mapeia modos legados para modos baseados em Tree-sitter
  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (python-mode     . python-ts-mode)
          (js-mode         . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode       . json-ts-mode)
          (css-mode        . css-ts-mode)
          (html-mode       . html-ts-mode)
          (yaml-mode       . yaml-ts-mode)
          (bash-mode       . bash-ts-mode)
          (go-mode         . go-ts-mode)
          (rust-mode       . rust-ts-mode)))
  ;; Habilita Tree-sitter em prog-mode
  (add-hook 'prog-mode-hook #'treesit-mode))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(use-package lsp-mode
  :straight t
  :commands lsp
  :init
  (setq lsp-auto-guess-root t)
  :hook
  ((js-ts-mode-hook js-mode-hook) . lsp-deferred)
  ((typescript-mode-hook tsx-mode-hook) . lsp-deferred)
  :config
  (setq lsp-enable-snippet t
        lsp-prefer-flymake nil))

;; Define a function to properly start LSP for JS/TS files
(defun my/lsp-js-ts-setup ()
  "Setup LSP for JavaScript and TypeScript files."
  (when (and (or (derived-mode-p 'js-ts-mode)
                 (derived-mode-p 'typescript-ts-mode)
                 (derived-mode-p 'web-mode))
             (not (lsp-mode)))
    (lsp-deferred)))

;; Hook to start LSP for JS/TS files
(add-hook 'js-ts-mode-hook #'my/lsp-js-ts-setup)
(add-hook 'typescript-ts-mode-hook #'my/lsp-js-ts-setup)
(add-hook 'web-mode-hook #'my/lsp-js-ts-setup)

;; Also hook for other programming modes
(add-hook 'python-ts-mode-hook #'lsp-deferred)
(add-hook 'rust-ts-mode-hook #'lsp-deferred)
(add-hook 'go-ts-mode-hook #'lsp-deferred)

(defun lsp-format-on-save ()
  "Format buffer and organize imports before saving."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'lsp-mode-hook #'lsp-format-on-save)

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-peek-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-delay 0.5
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t
        lsp-ui-flycheck-enable t))

;; Typescript and JS support - FIXED VERSION

;; typescript-mode for .ts and .tsx
(use-package typescript-mode
  :straight t
  :mode ("\\.ts\\'" . typescript-mode)  ;; Changed to typescript-ts-mode
  :hook ((typescript-mode . my/lsp-js-ts-setup)  ;; Use our custom function
         (typescript-mode . lsp)
         (typescript-mode . lsp-format-on-save)))

;; js-mode and js-ts-mode (for plain JS files)
(use-package js
  :mode ("\\.js\\'" . js-ts-mode)  ;; Changed to js-ts-mode
  :hook ((js-ts-mode . my/lsp-js-ts-setup)  ;; Use our custom function
         (js-ts-mode . lsp-format-on-save))
  :config
  (setq js-indent-level 2))

(use-package web-mode
  :straight t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.vue\\'" . web-mode))  ;; optionally
  :hook ((web-mode . prettier-mode))  ;; optional auto-formatting
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package css-mode
  :mode ("\\.css\\'" . css-mode)
  :hook ((css-mode . prettier-mode))
  :config
  (setq css-indent-offset 2))


(use-package jtsx
  :straight t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . (lambda () (hs-minor-mode) (lsp-deferred)))
        (jtsx-tsx-mode . (lambda () (hs-minor-mode) (lsp-deferred))))
  :custom
  ;; Optional customizations
  (js-indent-level 2)
  (typescript-ts-mode-indent-offset 2)
  (jtsx-switch-indent-offset 0)
  (jtsx-indent-statement-block-regarding-standalone-parent nil)
  (jtsx-jsx-element-move-allow-step-out t)
  (jtsx-enable-jsx-electric-closing-element t)
  (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  (jtsx-enable-jsx-element-tags-auto-sync nil)
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
    (define-key mode-map (kbd "C-c j d n") 'jtsx-delete-jsx-node)
    (define-key mode-map (kbd "C-c j d a") 'jtsx-delete-jsx-attribute)
    (define-key mode-map (kbd "C-c j t") 'jtsx-toggle-jsx-attributes-orientation)
    (define-key mode-map (kbd "C-c j h") 'jtsx-rearrange-jsx-attributes-horizontally)
    (define-key mode-map (kbd "C-c j v") 'jtsx-rearrange-jsx-attributes-vertically))
  
  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))
  
  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))

;; Enable prettier
(use-package prettier
  :straight t
  :hook ((typescript-ts-mode . prettier-mode)  ;; Changed to typescript-ts-mode
         (js-ts-mode . prettier-mode)  ;; Changed to js-ts-mode
         (web-mode . prettier-mode)))

;; TypeScript LSP configuration
(with-eval-after-load 'lsp-mode
  (setq lsp-clients-typescript-server "typescript-language-server"
        lsp-clients-typescript-server-args '("--stdio")))

  ;; organize imports on save
  (setq lsp-typescript-preferences-import-module-specifier "non-relative"
        lsp-typescript-implementations-code-lens-enabled t
        lsp-typescript-references-code-lens-enabled t
        lsp-typescript-suggest-auto-imports t
        lsp-typescript-auto-import-on-completion t)

(use-package jest
  :straight t
  :hook ((typescript-ts-mode . jest-minor-mode)  ;; Changed to typescript-ts-mode
         (js-ts-mode . jest-minor-mode)  ;; Changed to js-ts-mode
         (web-mode . jest-minor-mode))
  :bind (("C-c j t" . jest-file)
         ("C-c j s" . jest-file-dwim)
         ("C-c j p" . jest-project)))

;; Alpha background
(add-to-list 'default-frame-alist '(alpha-background . 100))

;; Doom Themes
(use-package doom-themes
  :straight t
  :demand t
  :config
  (doom-themes-visual-bell-config)
  (load-theme 'doom-monokai-pro t))

;; Doom Modeline
(use-package doom-modeline
  :straight t
  :defer t
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

;; All-the-icons
(use-package all-the-icons
  :straight t)

;; All-the-icons on Vertico/Marginalia completion
(use-package all-the-icons-completion
  :straight t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; Font settings
(set-face-attribute
 'default
 nil
 :height 120
 :family "Fira Code Nerd Font"
 :weight 'medium
 :width 'normal)

;; Beacon
(use-package beacon
  :straight t
  :defer t
  :init
  (setq beacon-blink-delay 0.1
        beacon-blink-duration 0.2)
  :config
  (beacon-mode 1))

;; Emacs Dashboard
(use-package dashboard
  :straight t
  :defer t
  :init
  (progn
    (setq dashboard-items '((recents . 5)
                            (bookmarks . 5)
                            (projects . 5)
                            (agenda . 1)))
    (setq dashboard-banner-logo-title "Lock in")
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-startup-banner '1))
  :config
  (dashboard-setup-startup-hook))

;; Centaur Tabs
(use-package centaur-tabs
  :straight t
  :defer t
  :bind
  (("C-c n" . centaur-tabs-forward)
   ("C-c a" . centaur-tabs-backward))
  :config
  (setq centaur-tabs-set-bar 'over
        centaur-tabs-set-icons t
        centaur-tabs-plain-icons t
        centaur-tabs-style 'bar
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-height 30
        centaur-tabs-set-modified-marker t
        centaur-tabs-modifier-marker "")
  (centaur-tabs-mode t))

;; Disable Centaur Tabs on Dashboard
(defun disable-centaur-tabs-on-dashboard ()
  "Disable Centaur Tabs on the dashboard."
  (when (string-equal (buffer-name) "*dashboard*")
    (centaur-tabs-local-mode 1)))
(add-hook 'dashboard-mode-hook 'disable-centaur-tabs-on-dashboard)

(use-package gcmh
  :straight t
  :diminish
  :init
  (setq gc-cons-threshold most-positive-fixnum)
  :hook (emacs-startup . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024)))

(use-package evil
  :straight t
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'message-buffer-mode 'normal))

;; Evil Nerd Commenter
(use-package evil-nerd-commenter
  :straight t
  :defer t
  :bind
  ("C-c c" . evilnc-comment-or-uncomment-lines))

;; Evil Collection
(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :straight t
  :config
  (general-define-key
   :states '(normal motion)
   :prefix "SPC"))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.1)
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  (company-show-quick-access t)
  (company-selection-wrap-around t))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  ;; Optional: Enable icons for all backends including lsp, elisp, etc.
  :config
  ;; Ensure you have all-the-icons installed for best results:
  (unless (require 'all-the-icons nil t)
    (ignore-errors (all-the-icons-install-fonts t))))

;; (use-package corfu
;;   :straight t
;;   :custom
;;   (corfu-cycle t)
;;   (corfu-auto t)
;;   (corfu-auto-prefix 2)
;;   (corfu-auto-delay 0.1)
;;   (corfu-popupinfo-delay '(0.5 . 0.2))
;;   (corfu-preview-current 'insert)
;;   (corfu-preselect 'prompt)
;;   (corfu-on-exact-match nil)
;;   :bind (:map corfu-map
;;               ("M-SPC"   . corfu-insert-separator)
;;               ("TAB"     . corfu-next)
;;               ([tab]     . corfu-next)
;;               ("S-TAB"   . corfu-previous)
;;               ([backtab] . corfu-previous)
;;               ("RET"     . corfu-insert)
;;               ("<return>" . corfu-insert))
;;   :init
;;   (global-corfu-mode)
;;   (corfu-history-mode)
;;   (corfu-popupinfo-mode))

;; (defun corfu-enable-yas ()
;;   "Enable yasnippet integration for corfu."
;;   (setq yas-minor-mode t)
;;   (add-hook 'corfu-mode-hook #'yas-minor-mode))

;; (add-hook 'emacs-startup-hook #'corfu-enable-yas)

;; (defun corfu-yas-expand ()
;;   "Try to expand snippet before using `corfu'."
;;   (interactive)
;;   (unless (yas-expand)
;;     (corfu-complete)))

;; (define-key corfu-map (kbd "TAB") #'corfu-yas-expand)
;; (define-key corfu-map (kbd "<tab>") #'corfu-yas-expand)

(use-package cape
  :straight t
  :defer t
  :bind (("C-c v p" . completion-at-point)
         ("C-c v d" . cape-dabbrev)
         ("C-c v f" . cape-file))
  :config
  (setq completion-at-point-functions
        (list
         #'cape-dabbrev
         #'cape-file
         #'cape-keyword
         #'cape-symbol
         #'cape-history)))

;; (use-package kind-icon
;;   :straight t
;;   :after corfu
;;   :custom
;;   (kind-icon-blend-background t)
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :straight t
  :init
  (savehist-mode))

(use-package vertico
  :straight t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package consult
  :straight t
  :bind(("C-s" . consult-line)
        ("C-c f" . consult-find )
        ("C-c r" . consult-ripgrep)
        ("C-c i" . consult-imenu)
        ("C-c g" . consult-git-grep))
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :straight t
  :after (embark consult))

(use-package rg
  :straight t
  :config
  (rg-enable-default-bindings))

(use-package projectile
  :straight t
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (setq projectile-enable-caching t)   
  (projectile-mode +1)
  (add-hook 'find-file-hook
            (lambda ()
              (when (projectile-project-p)
                (projectile-add-known-project (projectile-project-root))))))

(global-set-key (kbd "C-c p") 'projectile-find-file)

(use-package flycheck
  :straight t
  :init (global-flycheck-mode t)
  :hook ((typescript-ts-mode . flycheck-mode)  ;; Changed to typescript-ts-mode
         (js-ts-mode . flycheck-mode)  ;; Changed to js-ts-mode
         (web-mode . flycheck-mode)))

(use-package which-key
  :straight t
  :init
  (which-key-mode t))

(use-package rainbow-mode
  :straight t
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :straight t
  :config
  (global-rainbow-delimiters-mode))

(defun global-rainbow-delimiters-mode ()
  "Enable `rainbow-delimiters-mode' in all buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (rainbow-delimiters-mode 1)))
  (add-hook 'after-change-major-mode-hook #'rainbow-delimiters-mode))

(use-package org
  :config
  (setq org-agenda-files '("~/Agenda/agenda.org"))
  (setq org-ellipsis " ▾"))

(global-set-key (kbd "C-c o a") #'org-agenda)

(use-package org-roam
  :straight t
  :defer t
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

(use-package org-bullets
  :straight t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package vterm
  :straight t
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "zsh")
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
  :straight t
  :config
  (global-set-key (kbd "C-c v n") 'multi-vterm)
  (global-set-key (kbd "C-c v p") 'multi-vterm-prev)
  (global-set-key (kbd "C-c v c") 'multi-vterm-next))

(use-package magit
  :straight t)

(use-package git-gutter
  :straight t
  :config
  (global-git-gutter-mode 1))

(use-package blamer
  :straight (blamer :type git :host github :repo "artawower/blamer.el")
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 70)
  (blamer-view 'overlay)
  (blamer-max-commit-message-length 100)
  :config
  (global-blamer-mode 1))

(use-package super-save
  :straight t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 2
        auto-save-default nil
        make-backup-files nil))

(use-package rust-ts-mode
  :straight t
  :mode ("\\.rs" . rust-ts-mode)
  :hook ((rust-ts-mode . lsp-deferred)
         (rust-ts-mode . corfu-mode)))

(use-package toml-mode
  :straight t)

(use-package go-mode
  :straight t
  :hook ((go-ts-mode . lsp-deferred))  ;; Changed to go-ts-mode
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

  (add-hook 'go-ts-mode-hook #'lsp-go-install-save-hooks)  ;; Changed to go-ts-mode

  (add-hook 'go-ts-mode-hook #'lsp-deferred)  ;; Changed to go-ts-mode
  (add-hook 'go-ts-mode-hook #'yas-minor-mode)  ;; Changed to go-ts-mode

  (add-to-list 'exec-path "~/go/bin")
  (setq gofmt-command "goimports"))

(use-package solidity-mode
  :straight t
  :mode "\\.sol\\'"
  :hook ((solidity-mode . lsp-deferred))
  :config
  (setq solidity-comment-style 'slash
        solidity-indent-level 2))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(solidity-mode . "solidity"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("solidity-language-server" "--stdio"))
    :major-modes '(solidity-mode)
    :priority -1
    :server-id 'solidity-ls)))

(use-package tuareg
  :straight t
  :mode (("\\.ml\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode)
         ("\\.mlt\\'" . tuareg-mode))
  :hook ((tuareg-mode . lsp-deferred)))

(add-to-list 'load-path "/home/rodrigo/Dev/ocaml/learning_ocaml/_opam/share/emacs/site-lisp")

(use-package ocp-indent
  :straight t
  :hook (tuareg-mode . ocp-setup-indent))

(defun ocp-setup-indent ()
  (when (fboundp 'ocp-indent-mode)
    (ocp-indent-mode 1)))

(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    (setq merlin-command 'opam)))

(with-eval-after-load 'lsp-mode
  (setq lsp-ocaml-lang-server-command '("ocamllsp")))

(use-package emmet-mode
  :straight t
  :hook ((html-mode css-mode web-mode) . emmet-mode)
  :config
  (setq emmet-expand-jsx-className t))

(use-package dockerfile-ts-mode
  :straight (:type built-in)
  :defer t
  :mode (("\\Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.dockerignore\\'" . dockerfile-ts-mode)))

(use-package smartparens
  :straight t
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package dap-mode
  :straight t
  :after lsp-mode
  :commands dap-debug
  :init
  (setq dap-auto-show-output t)
  :config
  (require 'dap-ui)
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  (setq dap-tooltip-mode 1)
  (add-hook 'dap-mode-hook 'dap-tooltip-mode)
  (add-hook 'dap-mode-hook 'tooltip-mode)
  (add-hook 'dap-stopped-hook (lambda (_) (dap-ui-show-many-windows t)))
  (add-hook 'dap-terminated-hook (lambda (_) (dap-ui-hide-many-windows)))
  (add-hook 'dap-stopped-hook (lambda (_) (dap-ui-locals)))

  (add-hook 'dap-stopped-hook (lambda (_) (call-interactively #'dap-hydra)))

  (dap-auto-configure-mode 1)

  (require 'dap-node)
  (dap-register-debug-template "Node::Run Current File"
    (list :type "node"
          :request "launch"
          :name "Node::Run"
          :program "${file}"
          :cwd "${workspaceFolder}"
          :runtimeExecutable "node"
          :console "integratedTerminal"))

  (dap-register-debug-template "Jest::Test File"
    (list :type "node"
          :request "launch"
          :name "Jest::Test File"
          :program "${workspaceFolder}/node_modules/.bin/jest"
          :args '("${file}" "--runInBand")
          :cwd "${workspaceFolder}"
          :runtimeExecutable "node"
          :console "integratedTerminal"
          :internalConsoleOptions "neverOpen"))

  (require 'dap-gdb-lldb)
  (dap-register-debug-template "Rust::Run"
    (list :type "lldb"
          :request "launch"
          :name "Rust::Run"
          :gdbpath "rust-lldb"
          :target nil
          :cwd nil))

  (dap-register-debug-template "Rust::Test"
    (list :type "lldb"
          :request "launch"
          :name "Rust::Test"
          :gdbpath "rust-lldb"
          :program "${workspaceFolder}/target/debug/<test-binary>"
          :args '()
          :cwd "${workspaceFolder}"))

  (require 'dap-go)
  (dap-register-debug-template "Go::Run"
    (list :type "go"
          :request "launch"
          :name "Go::Run"
          :mode "debug"
          :program "${file}"
          :buildFlags ""
          :args []))

  (dap-register-debug-template "Go::Test Current File"
    (list :type "go"
          :request "launch"
          :name "Go::Test"
          :mode "test"
          :program "${file}"))

  (dap-register-debug-template "C/C++::Run"
    (list :type "gdb"
          :request "launch"
          :name "C/C++::Run"
          :gdbpath "gdb"
          :target nil
          :cwd nil))

  (global-set-key (kbd "C-c d h") #'dap-hydra)
  (global-set-key (kbd "C-c d b") #'dap-breakpoint-toggle)
  (global-set-key (kbd "C-c d c") #'dap-breakpoint-condition)
  (global-set-key (kbd "C-c d l") #'dap-breakpoint-log-message)
  (global-set-key (kbd "C-c d t") #'dap-debug)

  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-c d" "DAP Debugger"
      "C-c d h" "Hydra"
      "C-c d b" "Breakpoint"
      "C-c d c" "Conditional Breakpoint"
      "C-c d l" "Log Breakpoint"
      "C-c d t" "Debug Template")))
