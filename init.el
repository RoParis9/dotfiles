;; Remove Welcome Screen
  (setq inhibit-startup-message t)

  ;;Fixing the Scratch buffer
  (setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

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

;;Packages
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
  :config
   (which-key-mode))

;;theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-material-dark t))
  ;;acario-light the best light theme

;;mode line
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-time-icon t)
  (doom-modeline-lsp t))


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

;;emacs dashboard
(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-items '((recents .  1)
			    (projects . 1)))
    (setq dashboard-banner-logo-title "You code for living kekw")
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-startup-banner 'logo)
    (setq dashboard-items '((recents  . 5)
			(bookmarks . 5)
			(projects . 5)
			(agenda . 5)))
    )
  :config
  (dashboard-setup-startup-hook))

;;terminal
   (use-package vterm
     :ensure t
     :commands vterm
     :config
     (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
     (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
     (setq vterm-max-scrollback 10000))


(use-package vterm-toggle
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
  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-nerd-commenter
  :ensure t
  :bind
  ("C-c c" . evilnc-comment-or-uncomment-lines))

(use-package helm
  :ensure t
  :init
  (helm-mode))

(use-package helm-lsp
  :ensure t)

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
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package ace-window
   :ensure t
   :bind(("C-x o" . ace-window)))

;;tabs on top of the buffer
(use-package centaur-tabs
  :ensure t
  :config
  (setq centaur-tabs-set-bar 'over
  centaur-tabs-set-icons t
  centaur-tabs-style 'bar
  centaur-tabs-gray-out-icons 'buffer
  centaur-tabs-height 24
  centaur-tabs-set-modified-marker t
  centaur-tabs-modifier-marker ".")
  (centaur-tabs-mode t))

(use-package treemacs
       :ensure t
       :bind
       (:map global-map
                ([f8] . treemacs)
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

(use-package treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

;;git integration
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

(use-package lsp-mode
     :ensure t
     :commands lsp
     :hook((java-mode typescript-mode js2-mode web-mode rust-mode go-mode) . lsp)
     :hook (lsp-mode . company-mode)
     :bind (:map lsp-mode-map
       ("TAB" . completion-at-point))
     :custom
     (lsp-prefer-flymake nil)
     (lsp-headerline-breadcrumb-enable t)
     :bind(:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
     :config
     (lsp-enable-which-key-integration t))

 (use-package lsp-ui
     :ensure t
     :commands (lsp-ui-mode)
     :custom
     ;;Sideline
     (lsp-ui-sideline-show-diagnostics nil)
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
   :ensure t
   :commands lsp-treemacs-erros-list)

;;Performance
(setq gc-cons-threshold (* 100 1024 1024)
     read-process-output-max(* 1024 1024)
     create-lock-files nil) ;;lock files will kill 'npm start'

(use-package yasnippet
  :ensure t
  :config (yas-global-mode)
  (setq yas-snippet-dirs '/home/rodrigo/.emacs.d/snippets/'))

(use-package rjsx-mode
:mode "\\.jsx\\'"
:mode "components\\/.*\\.js\\'"
:hook ((rjsx-mode . lsp)))

  (add-hook 'rjsx-mode-hook #'lsp)
  (add-hook 'rjsx-mode-hook #'smartparens-mode)
  (add-hook 'rjsx-mode-hook #'tree-sitter-mode)

(use-package rust-mode
    :mode "\\.rs\\'"
    :init (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :config (add-hook 'rust-mode-hook 'cargo-minor-mode))

  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'smartparens-mode)

(defun my/setup-js-mode ()
  (require 'dap-firefox)
  (require 'dap-chrome)
  (setq tab-width 2)
  (when (require 'lsp-javascript nil t)
    (lsp)))

(use-package js2-mode
  :after (lsp-mode dap-mode)
  :mode "\\.js\\'"
  :hook ((js2-mode . my/setup-js-mode)))

(use-package typescript-mode
  :after (lsp-mode dap-mode)
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :hook ((typescript-mode . my/setup-js-mode)))

  (use-package prettier-js
    :hook ((js2-mode . prettier-js-mode)
        (typescript-mode . prettier-js-mode))
    :config
    (setq prettier-js-show-errors nil))

    (add-hook 'js-mode-hook #'lsp)
    (add-hook 'typescript-mode-hook #'lsp)

(use-package yaml-mode
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'"
  :hook ((yaml-mode . (lambda ()
                        (when (require 'lsp-yaml nil t)
                          (lsp))))
         (yaml-mode . yaml-imenu-enable)))
(use-package yaml-imenu
  :after yaml-mode)

(use-package dockerfile-mode
   :ensure t
   :mode "Dockerfile\\'")

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun dw/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
		    (markdown-header-face-2 . 1.1)
		    (markdown-header-face-3 . 1.0)
		    (markdown-header-face-4 . 1.0)
		    (markdown-header-face-5 . 1.0)))
  (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun dw/markdown-mode-hook ()
    (dw/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'dw/markdown-mode-hook))

(use-package lsp-java
   :hook (lsp-mode . lsp-enable-which-key-integratio) 
   :hook (add-hook 'java-mode-hook 'lsp)
   :hook (add-hook 'java-mode-hok 'smartparens-mode)
   :ensure t)

(use-package json-mode
  :mode "\\.json\\'"
  :hook ((json-mode . (lambda ()
                      (when (require 'lsp-json nil t)
                        (lsp))))))

(defun my-web-hook ()
 "Hooks for web mode."
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-autop-pairing t)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-current-column-highlight t))

(use-package web-mode
   :ensure t)

(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(use-package tree-sitter
   :ensure t)

(use-package tree-sitter-langs
   :ensure t)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package dap-mode
   :ensure t
   :after lsp-mode
   :hook ((lsp-mode . dap-mode)
          (lsp-mode . dap-ui-mode))
   :custom
   (lsp-enable-dap-auto-configure nil)
   :config
   (dap-auto-config-mode)
   (dap-ui-mode 1)
   (dap-tooltip-mode 1)
   (require 'dap-node)
   (dap-node-setup))

(use-package company
   :ensure t
   :hook (prog-mode . company-mode)
   :bind(:map company-active-map
           ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
           ("<tab>" . company-indent-or-complete-common))
   :custom
   (company-tooltip-aligh-annotation t)
   (company-require-match nil)
   (company-minimum-prefix-length 1)
   (company-idle-delay 0.0)
   :init (setq company-backends '(company-capf
                               company-elisp
                               company-cmake
                               company-yasnippet
                               company-files
                               company-keywords
                               company-etags
                               company-gtags
                               company-ispell)))

(add-hook 'after-init-hook 'global-company-mode)

(use-package company-box
  :ensure t
  :after company
  :diminish company-box-mode
  :custom
  (company-box-show-single-candidate t)
  (company-box-frame-behaviour 'point)
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-max-candidates 10)
  (company-box-icon-right-margin 0.5)
  :hook (company-mode . company-box-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package smartparens
   :ensure t
   :config
   (smartparens-global-mode t))

(add-hook 'prog-mode 'smartparens-global-mode)

(use-package flycheck
   :ensure t
   :commands (global-flycheck-mode))

(use-package rainbowmode
   :ensure t)
