(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(tree-sitte-langs tree-sitter rainbow-mode evil rainbow-delimiters git-gutter typescript-mode rjsx-mode yaml-mode json-mode ivy yasnippet centaur-tabs dap-mode flycheck lsp-ui lsp-mode projectile dap-node dap-java lsp-node helm beacon treemacs-icons-dired treemacs-magit company treemacs-all-the-icons magit magin vterm dashboard treemacs neotree auto-complete autocomplete all-the-icons doom-modeline doom-themes which-key use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;Remove Welcome Screen
(setq inhibit-startup-message t)

;;Remove Menus and Scroll Bar
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

(add-to-list 'package-archives '(("melpa" . "https://melpa.org/packages")
                                 ("org" . "https://orgmode.org/elpa")
                                 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize);;start the packages

(unless (package-installed-p 'use-package) ;; install use-package if it isn`t already
  (package-refresh-contents)
  (package-install 'use-package))

;;show keys
(use-package which-key
  :ensure t
  :config (which-key-mode))

;;theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t))

;;mode line
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;show icons
(use-package all-the-icons
  :ensure t)

;;git plugin
(use-package git-gutter
  :ensure t)
(global-git-gutter-mode +1)

;;highlight cursor
(use-package beacon
  :ensure t)
(beacon-mode 1)

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

(use-package neotree
  :ensure t)

;;setting the theme 
(setq doom-themes-treemacs-theme "doom-material-dark")

;;terminal
(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

;;git 
(use-package magit
  :ensure t)

(use-package helm
  :ensure t
  :config (helm-mode 1))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1))

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


(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui :ensure t)

(use-package dap-mode :ensure t)

(use-package flycheck :ensure t)

(use-package yasnippet :ensure t)

(use-package dash :ensure t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package json-mode :ensure t)

(use-package yaml-mode :ensure t)

(use-package markdown-mode :ensure t)

(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("pages\\/.*\\.js\\'" . rjsx-mode)))

(use-package typescript-mode
  :ensure t)

(add-hook 'js-mode-hook #'lsp)

(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") nil))

(global-set-key (kbd "C-c p") 'projectile-find-file)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      create-lockfiles nil);;lockfiles will kill npm with node

(with-eval-after-load 'lsp-mode
  (require 'dap-chrome)
  (setq lsp-modeline-diagnostics-enable t)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))

(use-package lsp-treemacs
  :ensure t)

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Org Mode Configuration ------------------------------------------------------

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

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package rainbow-mode
  :ensure t)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo) 
;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

