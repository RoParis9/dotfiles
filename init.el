;; Remove Welcome Screen
  (setq inhibit-startup-message t)


  ;; Remove Menus and Scroll Bar
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)

  ;;show recent files
  (recentf-mode 1)

  ;;
  (add-to-list 'load-path "/home/rodrigo/.emacs.d/lisp")


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


  ;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/Projects/Code/emacs-from-scratch/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

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
  (load-theme 'doom-dracula t))

;;mode line
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

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

;;(setq doom-themes-treemacs-theme "doom-dracula")

;;tabs on top of the buffer
(use-package centaur-tabs
  :ensure t
  :config
  (setq centaur-tabs-set-bar 'over
	centaur-tabs-set-icons t
	centaur-tabs-style "alternate"
	centaur-tabs-gray-out-icons 'buffer
	centaur-tabs-height 24
	centaur-tabs-set-modified-marker t
	centaur-tabs-modifier-marker ".")
  (centaur-tabs-mode t))

;;file tree
(use-package treemacs
  :ensure t
  :bind
  (:map global-map
	([f8] . treemacs)
	("C-<f8>" . treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t)
  (treemacs-git-mode 'deferred)
  (treemacs-filewatch-mode +1)
  (treemacs-indent-guide-mode 'block))

;;icons on the file tree
(use-package treemacs-all-the-icons
  :after treemacs
  :ensure t)

;;show icons on dired
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;;terminal
(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

;;git integration
(use-package magit
  :ensure t)

(use-package projectile
 :ensure t
 :config
 (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
 (projectile-mode +1))

(global-set-key (kbd "C-c p") 'projectile-find-file)

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
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package helm
  :ensure t
  :init
  (helm-mode))

(use-package helm-lsp
  :ensure t)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
   :ensure t
   :commands (lsp lsp-deferred)
   :init
   (setq lsp-keymap-prefix "C-c l")
   :config
   (lsp-enable-which-key-integration t))


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (require 'dap-mode)
  (dap-node-setup))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))



(use-package dap-mode)

;; auto complete
(use-package company
  :ensure t
  :hook ((emacs-lisp-mode . (lambda ()
			      (setq-local company-backends '(company-elisp))))
	 (emacs-lisp-mode . company-mode))

  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	 ("<tab>" . company-complete-selection))
	(:map lsp-mode-map
	 ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
   :ensure t
   :init 
   (add-hook 'emacs-lisp-mode-hook #'smartparens-mode))

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
    (set-face-attribute (car face) nil :font "Fira Mono" :weight 'regular :height (cdr face)))

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
