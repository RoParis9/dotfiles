(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(package-selected-packages

 (custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )))

 (setq org-return-follows-link t)
 ;; The default is 800 kilobytes.  Measured in bytes.
 (setq gc-cons-threshold (* 50 1000 1000))

(setq exec-path (append exec-path '("~/.asdf/shims")))
 ;; Remove Welcome Screen
 (setq inhibit-startup-message t)

	(setq custom-safe-themes t)


 ;;Fixing the Scratch buffer
 (setq initial-scratch-message "")

;;Removes *scratch* from buffer after the mode has been set.
 ;;  (defun remove-scratch-buffer ()
 ;;    (if (get-buffer "*scratch*")
 ;;        (kill-buffer "*scratch*")))

 ;;  (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

 ;; ;; Removes *messages* from the buffer.
 (setq-default message-log-max nil)
 (kill-buffer "*Messages*")

 ;; Removes *Completions* from buffer after you've opened a file.
 (add-hook 'minibuffer-exit-hook
       '(lambda ()
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

   ;;
   (add-to-list 'load-path "/home/rodrigo/.emacs.d/lisp")

   (setq-default indent-tabs-mode t)
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

   ;;Line numbers
    (global-display-line-numbers-mode 'relative)
    (setq display-line-numbers-type 'relative)

   (dolist (mode '(org-mode-hook
           term-mode-hook
           shell-mode-hook
           treemacs-mode-hook
 			 vterm-mode-hook
           eshell-mode-hook))
     (add-hook mode (lambda () (display-line-numbers-mode 0))))

   ;;highlight parenteses
   (show-paren-mode 1)
   (setq-default fill-column 80)


   ;;font size
   (set-face-attribute
    'default
    nil
    :height 140
    :family "Fira Code"
    :weight 'medium
    :width 'normal)

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

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(add-to-list 'default-frame-alist '(alpha-background . 100))


  (use-package doom-themes
      :straight t
      :config
      (doom-themes-visual-bell-config)
      (doom-themes-treemacs-config)
      (load-theme 'doom-one-light))


      ;;Dark Themes that i like: dracula | gruvbox | molokai | monokai-pro| palenight |
      ;;Light Themes that i like: modus-operandi | doom-one-light

(use-package doom-modeline
    :straight t
    :custom
    (doom-modeline-major-mode-icon t)
    (doom-modeline-major-mode-color-icon t)
    (doom-modeline-time-icon t)
    (doom-modeline-lsp t)
    :init
    (doom-modeline-mode))

      (use-package all-the-icons
      		:straight t
      		:if (display-graphic-p))

      (use-package rainbow-delimiters
      		:straight t
      		:hook
      		(prog-mode . rainbow-delimiters-mode))

(use-package centaur-tabs
    :straight t
    :bind
    ("C-c n" . centaur-tabs-forward)
    ("C-c b" . centaur-tabs-backward)
    :config
    (setq centaur-tabs-set-bar 'over
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-style "bar"
        centaur-tabs-height 24
        centaur-tabs-set-modifier-marker t
        centaur-tabs-modifier-marker "•")
(centaur-tabs-mode t))

(use-package ace-window
		:straight t
		:bind (("C-x o" . ace-window)))

(use-package evil
    :straight t
    :config
    :init
    (setq evil-want-keybinding nil)
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

(use-package smartparens
    :straight t
    :init
    (smartparens-global-mode))

;; Completions for Emacs, on the file buffer, like C-x C-f
(use-package vertico
    :straight t
    :custom
    (vertico-cycle t)
    :init
    (vertico-mode))

;; Searching package 
(use-package consult
    :straight t
    :bind(("C-s" . consult-line)
        ("C-c g" . consult-ripgrep))
    :custom
    (completion-in-region-function #'consult-completion-in-region))

;;you can search in any order 
(use-package orderless
	:straight t
 	:init (setq completion-styles '(orderless)))

;;Save Last Completion
(use-package savehist
    :straight t
    :init
    (savehist-mode))

;;all-the-icons on vertico
(use-package all-the-icons-completion
:straight t
:after (marginalia all-the-icons)
:hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
:init
(all-the-icons-completion-mode))

;;extra info on vertico
(use-package marginalia
    :straight t
    :custom
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light t))
    :init
    (marginalia-mode))

(use-package company
    :straight t
    :bind (:map company-active-map ("<tab>" . company-complete-selection))
    :hook ((emacs-lisp-mode . (lambda ()
                                (setq-local company-backends '(company-elisp))))
            (emacs-lisp-mode . company-mode))
    :config
    (setq company-idle-delay 0.1
                company-minimun-prefix-length 1
      		  company-frontends '(company-preview-frontend)
				company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend))
    :init
    (add-hook 'after-init-hook 'global-company-mode))

(use-package company-box
        :straight t
        :hook (company-mode . company-box-mode))

;; (use-package corfu
;;   :straight t
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   (corfu-separator ?\s)          ;; Orderless field separator
;;   (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   (corfu-preview-current nil)    ;; Disable current candidate preview
;;   (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   (corfu-scroll-margin 5)        ;; Use scroll margin
;;   :init
;;   (global-corfu-mode))

;; (use-package kind-icon
;;   :straight t
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(defun efs/org-mode-setup ()
      (org-indent-mode)
      (variable-pitch-mode 1)
      (auto-fill-mode 0)
      (visual-line-mode 1)
      (setq evil-auto-indent nil))

      (use-package org
      :config
      (setq org-ellipsis " ▾"))

      (defun efs/org-mode-visual-fill ()
      (setq visual-fill-column-width 80
          visual-fill-column-center-text t)
      (visual-fill-column-mode 1))


      (defun efs/org-font-setup ()
      ;; Replace list hyphen with dot
      (font-lock-add-keywords 'org-mode
                  '(("^ *\\([-]\\) "
                      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

      ;; Set faces for heading levels
      (dolist (face '((org-level-1 . 1.2)
              (org-level-2 . 1.1)
              (org-level-3 . 1.05)
              (org-level-4 . 1.0)
              (org-level-5 . 1.1)
              (org-level-6 . 1.1)
              (org-level-7 . 1.1)
              (org-level-8 . 1.1)))
          (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

      ;; Ensure that anything that should be fixed-pitch in Org files appears that way
      (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

      (use-package org-bullets
      :straight t
      :after org
      :config
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
      :custom
      (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
      :init (org-bullets-mode))

      (use-package org-roam
            :straight t
            :init
             (setq org-roam-v2-ack t)
            :custom
            (org-roam-directory "~/OrgNotes")
            (org-roam-completion-everywhere t)
            :bind (("C-c o f"  .  org-roam-node-find)
                          ("C-c o i"  .  org-roam-node-insert)
                          ("C-c o t" .  org-roam-node-buffer-toggle))
            :config
            (org-roam-setup))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package magit
    :straight t)

(use-package git-gutter
    :straight t
    :config
    (global-git-gutter-mode 1))

(use-package projectile
    :straight t
    :config
    (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
    (projectile-mode +1))

(global-set-key (kbd "C-c p") 'projectile-find-file)

(use-package treemacs
    :straight t
    :bind
    (:map global-map
                ("C-\\" . treemacs)
                ("C-0" . treemacs-select-window))
    :config
    (setq treemacs-is-never-other-window t))

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

(use-package lsp-mode
    :straight t
    :commands lsp
    :hook((lsp-java . lsp)
        (lsp-mode . company-mode)
        (php-mode . lsp)
        (web-mode . lsp))
    :custom
    (lsp-completion-provider :none)
    (lsp-prefer-flymake nil)
    (lsp-headerline-breadcrumb-enable nil)
    (lsp-headerline-breadcrumb-enable-symbol-numbers t)
    (lsp-enable-on-type-formatting t)
    (setq lsp-enable-snippet t)
    :bind(:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
    :config
    (lsp-enable-which-key-integration t)
    :init
    (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless)))
    :hook
    (lsp-completion-mode . my/lsp-mode-setup-completion))

(add-hook 'prog-mode-hook #'lsp)

(use-package lsp-ui
    :straight t
    :commands (lsp-ui-mode)
    :custom
    ;;Sideline
    (lsp-ui-sideline-show-diagnostics t)
    (lsp-ui-sidelin-enable nil)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-delay 0)
    (lsp-ui-update-mode 'line)
    (lsp-ui-peek-enable t)
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

(setq lsp-language-id-configuration
           '((javascript-mode .  "javascript")
              (php-mode . "php")
                (typescript-mode . "typescript")))
(setq lsp-log-io t)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(defun rk/rustic-mode-hook ()
    ;; so that run C-c C-c C-r works without having to confirm, but don't try to
    ;; save rust buffers that are not file visiting. Once
    ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
    ;; no longer be necessary.
    (when buffer-file-name
    (setq-local buffer-save-without-query t))
    (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package rustic
    :straight t
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

(use-package go-mode
    :straight t
    :hook ((go-mode . lsp-deferred)
            (go-mode . company-mode))
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
(add-hook 'go-mode-hook #'yas-minor-mode)

;; GOPATH/bin
(add-to-list 'exec-path "~/go/bin")
(setq gofmt-command "goimports"))

(use-package tuareg
        :straight t
        :config
        (add-hook 'tuareg 'lsp)
        (add-hook 'tuareg 'smartparens)
        (add-hook 'tuareg 'flycheck-mode)
        :mode (("\\.ocamlinit\\'" . tuareg-mode)))

    (use-package dune
        :straight t)

    (use-package merlin
        :straight t
        :config
        (add-hook 'tuareg-mode-hook #'merlin-mode)
        (setq merlin-error-after-save nil))

(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
        (when (and opam-share (file-directory-p opam-share))
        ;; Register Merlin
        (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
        (autoload 'merlin-mode "merlin" nil t nil)
        ;; Automatically start it in OCaml buffers
        (add-hook 'tuareg-mode-hook 'merlin-mode t)
        (add-hook 'caml-mode-hook 'merlin-mode t)
        ;; Use opam switch to lookup ocamlmerlin binary
        (setq merlin-command 'opam)))

    (add-to-list 'load-path "/home/rodrigo/.opam/default/share/emacs/site-lisp")
    (require 'ocp-indent)

    (use-package flycheck-ocaml
    :straight t
    :config
    (flycheck-ocaml-setup))

(use-package php-mode
         :straight t
         :mode
         ("\\.php\\'" . php-mode))

(use-package tide
    :straight t
    :after (js2-mode typescript-mode company-mode flycheck)
    :hook ((typescript-mode . tide-setup)
           (typescript-mode . tide-hl-identifier-mode)
           (before-save . tide-format-before-save)))

      (use-package typescript-mode
        :straight t
        :mode "\\.tsx?\\'"
        :config
        (setq typescript-indent-level 2))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

  (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1))

    ;; formats the buffer before saving
    (add-hook 'before-save-hook 'tide-format-before-save)

    ;; if you use typescript-mode
    (add-hook 'typescript-mode-hook #'setup-tide-mode)

    (add-hook 'tsx-ts-mode-hook #'setup-tide-mode)

(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :straight t
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  (add-to-list 'auto-mode-alist '("\\.js\\" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))

(use-package lsp-java
:straight t
:config
(add-hook 'java-mode-hook 'lsp)
(add-hook 'java-mode-hook 'smartparens-mode)
(add-hook 'java-mode-hook 'flycheck-mode)
(require 'dap-java))

(use-package apheleia
    :straight t
    :config
    (apheleia-global-mode +1))

(use-package web-mode
    :straight t
    :mode (
                         ("\\.html\\'" . web-mode))
    :config
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-identation t)
    (setq web-mode-enable-auto-close t)
    (setq web-mode-enable-auto-pairing t))

(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))

(add-hook 'web-mode-hook #'lsp)


(add-hook 'web-mode-before-auto-complete-hooks 'company-mode-hook)

(use-package dockerfile-mode
:straight t
:mode "Dockerfile\\'")

(use-package json-mode
    :mode "\\.json\\'"
    :hook ((json-mode . (lambda ()
                    (when (require 'lsp-json nil t)
                        (lsp))))))

(use-package yaml-mode
      :straight t
      :mode "\\.yml\\'"
      :mode "\\.yaml\\'")

(use-package tree-sitter 
       :straight t 
       :config (global-tree-sitter-mode) 
       (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)) 

(use-package tree-sitter-langs 
       :straight t 
       :after tree-sitter
       (global-tree-sitter-mode) 
       (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package rainbow-delimiters
:straight t
:config
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package flycheck
    :straight t
    :init (global-flycheck-mode t))

(use-package yasnippet   
    :straight t
    :config
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode 1))

(use-package yasnippet-snippets
    :straight t)

(use-package quickrun
    :straight t)

(use-package pdf-tools
    :straight t)

(use-package rainbow-mode
    :straight t)

(use-package vterm
                   :straight t
                   :commands vterm
                   :config
                   (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
                   (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
                   (setq vterm-max-scrollback 10000))

               (use-package vterm-toggle
                  :straight t
                  :bind
                  (("C-`"        . vterm-toggle)
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
