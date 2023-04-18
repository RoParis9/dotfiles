(custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(package-selected-packages
     '(corfu consult marginalia smartparens vertico evil-nerd-commenter evil vterm which-key use-package rainbow-delimiters org-bullets doom-themes doom-modeline beacon all-the-icons)))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )

  ;; The default is 800 kilobytes.  Measured in bytes.
  (setq gc-cons-threshold (* 50 1000 1000))


  ;; Remove Welcome Screen
  (setq inhibit-startup-message t)

  ;;Fixing the Scratch buffer
  (setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
  ;; (defun remove-scratch-buffer ()
  ;;   (if (get-buffer "*scratch*")
  ;;       (kill-buffer "*scratch*")))

  ;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

  ;; Removes *messages* from the buffer.
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

    ;;show recent files
    (recentf-mode 1)

    ;;
    (add-to-list 'load-path "/home/rodrigo/.emacs.d/lisp")

    (setq-default indent-tabs-mode nil)
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
            eshell-mode-hook
            vterm-mode-hook))
      (add-hook mode (lambda () (display-line-numbers-mode 0))))

    ;;highlight parenteses
    (show-paren-mode 1)

    ;;font size
    (set-face-attribute
     'default
     nil
     :height 160
     :family "Fira Code"
     :weight 'medium
     :width 'normal)

(require 'package)
(setq package-enable-at-startup nil);;turn of startup packages

(setq package-archives
   '(("melpa" . "https://melpa.org/packages/")
	 ("org" . "https://orgmode.org/elpa/")
	 ("elpa" . "https://elpa.gnu.org/packages/")
	 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize);;start the packages

(unless (package-installed-p 'use-package) ;; install use-package if it isn`t already
  (package-refresh-contents)
  (package-install 'use-package))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(add-to-list 'default-frame-alist '(alpha-background . 100))



;;theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-material-dark t))

  ;;acario-light the best light theme

;;mode line
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-time-icon t)
  (doom-modeline-lsp t)
  :init
  (doom-modeline-mode))


(use-package all-the-icons
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;Cursor Highlight
(use-package beacon
  :ensure t)

(beacon-mode 1)

;;terminal
  (use-package vterm
    :ensure t
    :commands vterm
    :config
    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
    (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
    (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
   :ensure t
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

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode)
  (evil-set-undo-system 'undo-redo)
  ;; Use visual line motions even outside visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'message-buffer-mode 'normal))

(use-package evil-nerd-commenter
  :ensure t
  :bind
  ("C-c c" . evilnc-comment-or-uncomment-lines))

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode))

;; Completions for Emacs
(use-package vertico
   :ensure t
   :custom
   (vertico-cycle t)
   :init
   (vertico-mode))

;;Save Last Completion
(use-package savehist
   :ensure t
   :init
  (savehist-mode))

;; Searching package
(use-package consult
  :ensure t
  :bind(("C-s" . consult-line)
        ("C-c g" . consult-ripgrep))
  :custom
  (completion-in-region-function #'consult-completion-in-region))

;;you can search in any order 
(use-package orderless
   :ensure t
   :init (setq completion-styles '(orderless)))

;;all-the-icons on vertico
(use-package all-the-icons-completion
 :ensure t
 :after (marginalia all-the-icons)
 :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
 :init
 (all-the-icons-completion-mode))

;;extra info on vertico
(use-package marginalia
   :ensure t
   :custom
   (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
   :init
   (marginalia-mode))

;;completion
(use-package corfu
 :ensure t
 ;; Optional customizations
 :custom
 (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
 (corfu-auto-prefix 1)
 (corfu-auto t)                 ;; Enable auto completion
 (corfu-auto-delay 0.0)
 (corfu-separator ?\s)          ;; Orderless field separator
;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;; (corfu-preview-current nil)    ;; Disable current candidate preview
;; (corfu-preselect 'prompt)      ;; Preselect the prompt
;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
 (corfu-scroll-margin 5)        ;; Use scroll margin
 (tab-always-indent 'complete)

;; Enable Corfu only for certain modes.
;; :hook ((prog-mode . corfu-mode)
;;        (shell-mode . corfu-mode)
;;        (eshell-mode . corfu-mode))

;; Recommended: Enable Corfu globally.
;; This is recommended since Dabbrev can be used globally (M-/).
;; See also `corfu-excluded-modes'.
:init
(global-corfu-mode)
(corfu-history-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
  (setq visual-fill-column-width 100
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
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  :init (org-bullets-mode))

(use-package magit
   :ensure t)

(use-package git-gutter
   :ensure t
   :config
   (global-git-gutter-mode 1))

(use-package projectile
   :ensure t
   :config
   (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
   (projectile-mode +1))

(global-set-key (kbd "C-c p") 'projectile-find-file)

(use-package treemacs
     :ensure t
     :bind
     (:map global-map
              ("<f8>" . treemacs)
              ("C-<f8>" . treemacs-select-window))
     :config
     (setq treemacs-is-never-other-window t))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-all-the-icons
   :ensure t
   :after (treemacs all-the-icons)
   :config
   (treemacs-load-theme "all-the-icons"))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :hook ((lsp-mode . dap-mode)
         (lsp-mode . dap-ui-mode))
  :custom
  (lsp-enable-dap-auto-configure t)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1))

(use-package lsp-mode
     :ensure t
     :commands lsp
     :hook((lsp-java . lsp)
           (lsp-mode . corfu-mode))
     :custom
     (sp-completion-provider :none)
     (lsp-prefer-flymake nil)
     (lsp-headerline-breadcrumb-enable t)
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
     :ensure t
     :commands (lsp-ui-mode)
     :custom
     ;;Sideline
     (lsp-ui-sideline-show-diagnostics t)
     (lsp-ui-sidelin-enable t)
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
   :ensure t
   :commands lsp-treemacs-erros-list)

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
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(use-package lsp-java
   :ensure t
   :config
   (add-hook 'java-mode-hook 'lsp)
   (add-hook 'java-mode-hook 'smartparens-mode)
   (add-hook 'java-mode-hook 'flycheck-mode)
   (require 'dap-java))

(use-package tuareg
     :ensure t
     :mode (("\\.ocamlinit\\'" . tuareg-mode)))

  (use-package dune
     :ensure t)

  (use-package merlin
     :ensure t
     :config
     (add-hook 'tuareg-mode-hook #'merlin-mode)
     (add-hook 'merlin-mode-hook #'corfu-mode)
     (setq merlin-error-after-save nil))

(use-package flycheck-ocaml
  :ensure t
  :config
  (flycheck-ocaml-setup))

(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :ensure t
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

(use-package tide
  :ensure t
  :after (js2-mode typescript-mode corfu flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

    (use-package typescript-mode
      :ensure t
      :mode "\\.ts\\'"
      :config
      (setq typescript-indent-level 2))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package prettier-js
   :ensure t
   :after (js2-mode)
   :hook ((js2-mode . prettier-js-mode)
          (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

(use-package emmet-mode
  :ensure t)

(use-package web-mode
   :ensure t
   :config
   (setq web-mode-enable-auto-closing t)
   (setq web-mode-enable-auto-identation t)
   (setq web-mode-enable-auto-close t)
   (setq web-mode-enable-auto-pairing t))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook #'lsp)


(add-hook 'web-mode-before-auto-complete-hooks 'corfu-mode-hook)

(use-package dockerfile-mode
   :ensure t
   :mode "Dockerfile\\'")

(use-package json-mode
  :mode "\\.json\\'"
  :hook ((json-mode . (lambda ()
                     (when (require 'lsp-json nil t)
                       (lsp))))))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'")

(use-package tree-sitter
   :ensure t
   :config
   (global-tree-sitter-mode)
   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
   :ensure t
   :after tree-sitter)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package flycheck
   :ensure t
   :config
   :commands (global-flycheck-mode))

(use-package yasnippet   
   :ensure t
   :config
   (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
   (yas-global-mode 1))

(use-package quickrun
  :ensure t)

(use-package pdf-tools
   :ensure t)

(use-package rainbow-mode
   :ensure t)

(use-package restclient
   :ensure t)
