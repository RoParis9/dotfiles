;; Remove Welcome Screen
(setq inhibit-startup-message t)

(functionp 'module-load t)
;; Remove Menus and Scroll Bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;show recent files
(recentf-mode 1)

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
  :ensure t)

;;theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-material-dark t))

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

(setq doom-themes-treemacs-theme "doom-material-dark")

;; auto complete
(use-package company
  :ensure t
  :hook ((emacs-lisp-mode . (lambda ()
			      (setq-local company-backends '(company-elisp))))
	 (emacs-lisp-mode . company-mode))
  :config
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 1)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

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

(use-package vertico
  :init
  (vertico-mode))
