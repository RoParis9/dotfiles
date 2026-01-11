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
(require 'use-package) 

(setq native-comp-async-report-warnings-errors 'silent)

;; Word Wrapper
(global-visual-line-mode t)

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


(setq initial-scratch-message ";; Bem-vindo ao Emacs!\n")
(setq initial-major-mode 'emacs-lisp-mode)

;; Remove auto-save e backup apenas para o scratch
(add-hook 'emacs-startup-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (setq buffer-auto-save-file-name nil
                    make-backup-files nil))))

;; Core Emacs settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default xref-search-program 'ripgrep)
(setq-default show-trailing-whitespace nil)
(setq indent-line-function 'insert-tab)

(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
;; Only auto-revert in specific modes for performance
(add-hook 'prog-mode-hook 'auto-revert-mode)


(setq create-lockfiles nil)

;; Add unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Project management
(require 'project)

;; Org-mode settings
(setq org-return-follows-link t)

;; Performance optimizations
(setq read-process-output-max (* 1024 1024))
(setq large-file-warning-threshold 100000000)  ; 100MB
(setq vc-follow-symlinks t)
(setq inhibit-compacting-font-caches t)
(setq message-log-max 10000)
(setq ring-bell-function 'ignore)

(setq custom-safe-themes t)

(setq frame-resize-pixelwise t)

(blink-cursor-mode -1)                                
(pixel-scroll-precision-mode) 

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



;;auto save
(auto-save-visited-mode t)
(setq auto-save-visited-interval 1)
(setq auto-save-directory (concat user-emacs-directory "auto-save/"))
(unless (file-exists-p auto-save-directory)
  (make-directory auto-save-directory t))

;; Tell the built-in auto-save feature to use this directory
(setq auto-save-file-name-transforms `((".*" ,auto-save-directory t)))

;; Optimized auto-save settings for performance
(setq auto-save-interval 200)  ; Auto-save every 200 keystrokes (less frequent)
(setq auto-save-timeout 3)     ; Auto-save after 3 seconds of inactivity
(auto-save-mode t)             ; Enable auto-save mode

;; instead of yes or no i want y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;;highlight current line (deferred for performance)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)

;;auto-closing tags
(electric-pair-mode 1)

;;auto save config
(defvar auto-save-directory (concat user-emacs-directory "auto-save/"))

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

(use-package super-save
  :straight t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 1  ; Save after 1 second of inactivity (more reasonable)
        super-save-remote-files nil    ; Don't auto-save remote files
        super-save-hook-trigger-commands '(save-buffer
                                           evil-window-next
                                           evil-window-prev
                                           balance-windows
                                           other-window
                                           next-buffer
                                           previous-buffer
                                           save-some-buffers
                                           evil-write
                                           evil-save-and-close)
        auto-save-default nil
        make-backup-files nil))

;; Alpha background
(add-to-list 'default-frame-alist '(alpha-background . 80))

;; Doom Themes
(use-package doom-themes
  :straight t
  :demand t
  :config
  (doom-themes-visual-bell-config))

(load-theme 'doom-monokai-pro t)


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


;; All-the-icons on Vertico/Marginalia completion
(use-package all-the-icons-completion
  :straight t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode)
  :config
  ;; Ensure icons work properly in completion
  (setq all-the-icons-completion-method 'all-the-icons-completion-method-ivy)
  ;; Force icon refresh
  (add-hook 'after-init-hook
            (lambda ()
              (when (display-graphic-p)
                (all-the-icons-completion-mode 1)))))



;; Font settings
(set-face-attribute
 'default
 nil
 :height 120
 :family "FiraCode Nerd Font"
 :weight 'medium
 :width 'normal)

(use-package all-the-icons
  :straight t)

(use-package nerd-icons
  :straight (nerd-icons
             :type git
             :host github
             :repo "rainstormstudio/nerd-icons.el"
             :files (:defaults "data"))
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "FiraCode Nerd Font"))

(use-package treemacs
  :straight t
  :defer t
  :config
  (setq treemacs-deferred-git-apply-delay      0.2
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 t
        treemacs-file-event-delay              5000
        treemacs-file-follow-delay             0.1
        treemacs-follow-after-init             t
        treemacs-is-never-other-window         t
        treemacs-no-delete-other-windows       t
        treemacs-position                      'left
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              t
        treemacs-silent-refresh                t
        treemacs-collapse-dirs                 0
        treemacs-width                         35)

  ;; Enable auto-follow and refresh
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-git-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always))

(use-package treemacs-nerd-icons
  :straight t
  :after treemacs
  :config
  ;; THIS registers the theme
  (treemacs-load-theme "nerd-icons"))

;; (use-package treemacs-all-the-icons
;;   :straight t
;;   :after treemacs
;;   :config
;;   (treemacs-load-theme "all-the-icons"))

;; (with-eval-after-load 'treemacs
;;   (require 'treemacs-all-the-icons)
;;   (treemacs-load-theme "all-the-icons"))

(with-eval-after-load 'treemacs
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons"))

(add-hook 'projectile-after-switch-project-hook #'treemacs-add-and-display-current-project)

(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

;; 🪞 Match Treemacs window opacity with global frame transparency
(defun my/treemacs-sync-opacity (&rest _)
  "Sync Treemacs window background transparency with current frame."
  (let* ((alpha (or (alist-get 'alpha-background default-frame-alist) 100))
         (bg (face-attribute 'default :background nil 'default)))
    (when (treemacs-get-local-window)
      (with-current-buffer (treemacs-get-local-buffer)
        (face-remap-add-relative 'default `(:background ,bg :inherit default))
        (set-frame-parameter (window-frame (treemacs-get-local-window))
                             'alpha-background alpha)))))

(add-hook 'treemacs-select-hook #'my/treemacs-sync-opacity)
(add-hook 'treemacs-mode-hook #'my/treemacs-sync-opacity)


;; Função para focar o Treemacs
(defun my/focus-treemacs ()
  "Foca a janela do Treemacs."
  (interactive)
  (if (treemacs-get-local-window)
      (select-window (treemacs-get-local-window))
    (treemacs)))

;; Função para voltar para o buffer anterior
(defun my/focus-prev-buffer ()
  "Volta para o buffer anterior."
  (interactive)
  (other-window 1)) ;; ou use outra lógica se quiser voltar para o buffer exato

;; Atalhos no Evil normal mode
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-h") #'my/focus-treemacs)
  (define-key evil-normal-state-map (kbd "C-l") #'my/focus-prev-buffer))


;; Optional integrations (if you use them)
(use-package treemacs-evil
  :straight t
  :after (treemacs evil))

;; Projectile integration
(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile)
  :config
  (treemacs-projectile-mode))

;; Magit/Git integration
;; optional: configure how treemacs reacts to git changes
(use-package treemacs-magit
  :straight t
  :after (treemacs magit))

;; LSP integration
(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :config
  (lsp-treemacs-sync-mode 1))  ;; example function to keep Treemacs in sync with LSP info

(use-package treemacs-icons-dired
  :straight t)

(use-package centaur-tabs
  :straight t
  :bind
  (("C-c n" . centaur-tabs-forward)
   ("C-c b" . centaur-tabs-backward))
  :init
  (setq centaur-tabs-style "alternate"
        centaur-tabs-set-icons t
        centaur-tabs-icon-type 'all-the-icons
        centaur-tabs-set-bar 'over
        centaur-tabs-close-button "X"
        centaur-tabs-modified-marker "*"
        centaur-tabs-change-fonts "Fira Code NerdFont")
  :hook
  (prog-mode . centaur-tabs-mode)
  :config
  ;; Function that decides which buffers should be hidden
  (defun centaur-tabs-hide-tab (x)
    "Do not show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is dedicated
       (window-dedicated-p (selected-window))
       ;; Buffer name blacklist
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*scratch*" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*mybuf" name)
       ;; Magit buffers (but not file-visiting ones)
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name))))))
  ;; Tell centaur-tabs to use the function
  (setq centaur-tabs-hide-function #'centaur-tabs-hide-tab)
  ;; Enable globally
  (centaur-tabs-mode 1))

(use-package general
  :demand t ;; ensures it's loaded immediately
  :config
  ;; Define leader key function early
  (general-create-definer my/leader-keys
    :prefix "SPC"
    :states '(normal visual emacs)))

;; Define the keybinding now that general is guaranteed loaded
(my/leader-keys
  "e" '(treemacs :which-key "Toggle file explorer")
  "RET" '(consult-bookmark :which-key "Consult Bookmarks"))

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
  (evil-define-key 'normal lsp-mode-map "gr" 'lsp-find-references)
  (evil-define-key 'normal lsp-mode-map "gd" 'lsp-find-definition)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'message-buffer-mode 'normal))

(define-key evil-normal-state-map (kbd "Q") (lambda () (interactive) (kill-current-buffer)))

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

(use-package which-key
  :straight t
  :init
  (which-key-mode t))

(use-package avy
  :straight t
  :bind (("M-j" . avy-goto-char-timer)    ; Type a few chars, then jump to one
         ("M-g l" . avy-goto-char-2)
         ("M-g w" . avy-goto-word-1))
  :custom
  (avy-style 'at-full)
  ;; Make hints easier to read (use home row keys for jumping)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; Search through all open windows in the current frame
  (avy-all-windows t)
  ;; Highlight the background to make the hints pop out
  (avy-background t)
  ;; Use a timer (0.5s) for avy-goto-char-timer so it feels responsive
  (avy-timeout-secs 0.3)
  :config
  (avy-setu'p-default))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

;; (use-package company
;;   :straight t
;;   :hook (prog-mode . company-mode)
;;   :custom
;;   (company-minimum-prefix-length 1)  ; Start completion earlier
;;   (company-idle-delay 0.05)  ; Much faster response (50ms)
;;   (company-tooltip-align-annotations t)
;;   (company-require-match nil)
;;   (company-show-quick-access t)
;;   (company-selection-wrap-around t)
;;   (company-backends '(company-capf company-dabbrev company-keywords))  ; Optimized backends
;;   (company-transformers '(company-sort-by-backend-importance))  ; Better sorting
;;   :init
;;   (global-company-mode t))

;; (use-package company-box
;;   :straight t
;;   :hook (company-mode . company-box-mode)
;;   :custom
;;   (company-box-icons-alist 'company-box-icons-all-the-icons)
;;   ;; Optional: Enable icons for all backends including lsp, elisp, etc.
;;   :config
;;   ;; Ensure you have all-the-icons installed for best results:
;;   (unless (require 'all-the-icons nil t)
;;     (ignore-errors (all-the-icons-install-fonts t))))

(use-package corfu
  :straight t
  :hook
  (lsp-after-initialize-hook . corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)  ; Start completion after 1 character
  (corfu-auto-delay 0.0)  ; No delay for auto-completion
  (corfu-popupinfo-delay '(0.1 . 0.1))  ; Faster popup info
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  ;; --- CRITICAL FOR ORDERLESS ---
  (corfu-quit-at-boundary 'separator) ;; Changed from t
  (corfu-quit-no-match 'separator)    ;; Changed from t
  ;; ------------------------------
  (corfu-count 10)  ; Limit completion candidates
  (corfu-max-width 100)  ; Limit popup width
  (corfu-min-width 20)  ; Minimum popup width
  :bind (:map corfu-map
              ("M-SPC" . corfu-insert-separator)
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET" . corfu-insert)
              ("<return>" . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :config
  ;; Disable Corfu in minibuffer to prevent autocomplete in M-x and Vertico
  (defun my/corfu-auto-exclude-predicate ()
    "Exclude minibuffer from Corfu auto-completion."
    (or (minibufferp)
        (eq (current-local-map) minibuffer-local-map)
        (eq (current-local-map) minibuffer-local-ns-map)
        (eq (current-local-map) minibuffer-local-completion-map)
        (eq (current-local-map) minibuffer-local-must-match-map)
        (eq (current-local-map) minibuffer-local-filename-completion-map)))
  
  (setq corfu-auto-exclude-predicate #'my/corfu-auto-exclude-predicate))

(use-package svg-lib :straight t)

(use-package kind-icon
  :straight t
  :demand t  
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background t)
  (kind-icon-blend-fraction 0.08)
  (kind-icon-cache-dir (expand-file-name "cache/kind-icon/" user-emacs-directory))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (require 'kind-icon)
  (kind-icon-reset-cache))

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

(use-package orderless
  :straight t
  :custom
  (setq completion-styles '(orderless basic)
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
  :init (vertico-mode) ; Ativa a UI
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))) ; Atalhos ao estilo Evil/Vim

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package consult
  :straight t
  :bind(("C-s" . consult-line)
        ("C-c s f" . consult-find)
        ("C-c s b" . consult-project-buffer)
        ("C-c s B" . consult-buffer)
        ("C-c s r" . consult-ripgrep)
        ("C-c y" . consult-yank-pop)
        ("C-c s i" . consult-imenu)
        ("C-c s g" . consult-git-grep))
  :config
  (setq consult-preview-key 'any)
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package rg
  :straight t
  :config
  (rg-enable-default-bindings))

(use-package projectile
  :straight t
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-x p t") 'projectile-test-project)
  (define-key projectile-mode-map (kbd "C-x p T") 'projectile-run-project)
  (define-key projectile-mode-map (kbd "C-x p f") 'projectile-find-file)
  (setq projectile-enable-caching t)   
  (setq projectile-project-search-path '("~/orgfile" "~/OrgNotes" "~/Dev"))
  (setq projectile-completion-system 'default)
  (projectile-mode +1))

(use-package rainbow-mode
  :straight t
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (org-mode . rainbow-delimiters-mode))

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
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package vterm
  :straight t
  :commands vterm
  :config
  ;; --- Basic vterm settings ---
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "zsh")
  (setq vterm-copy-mode t)
  (setq vterm-max-scrollback 10000)

  ;; --- Make Ctrl+V and Ctrl+Shift+V paste in vterm ---
  (define-key vterm-mode-map (kbd "C-v") #'vterm-yank)
  (define-key vterm-mode-map (kbd "C-S-v") #'vterm-yank)

  ;; --- Automatically open vterm in current file's directory ---
  (defun my/vterm-project-root-or-default ()
    "Return current buffer's directory or default-directory."
    (or (and buffer-file-name (file-name-directory buffer-file-name))
        default-directory))

  (defun my/vterm-in-current-dir ()
    "Open a vterm in the same directory as the current file."
    (interactive)
    (let ((default-directory (my/vterm-project-root-or-default)))
      (vterm)))

  ;; Optionally rebind the default vterm key
  (global-set-key (kbd "C-c v t") #'my/vterm-in-current-dir))

(use-package vterm-toggle
  :straight t
  :bind
  (("C-`"   . my/vterm-toggle-in-current-directory)
   ("C-c t" . my/vterm-toggle-in-current-directory)
   :map vterm-mode-map
   ("<C-return>" . vterm-toggle-insert-cd))
  :config
  ;; Function: Toggle vterm in current file's directory
  (defun my/vterm-toggle-in-current-directory ()
    "Toggle vterm, and if opening, use the current buffer's directory."
    (interactive)
    (let ((default-directory
           (if (buffer-file-name)
               (file-name-directory (buffer-file-name))
             default-directory)))
      (vterm-toggle)))

  ;; Place vterm at the bottom side window
  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.3)
                 (side . bottom)
                 (slot . 0))))

(use-package multi-vterm
  :straight t
  :config
  ;; Function: Open multi-vterm in current buffer's directory
  (defun my/multi-vterm-in-current-directory ()
    "Open a new multi-vterm in the same directory as the current buffer."
    (interactive)
    (let ((default-directory
           (if (buffer-file-name)
               (file-name-directory (buffer-file-name))
             default-directory)))
      (multi-vterm)))

  ;; Keybindings for multi-vterm
  (global-set-key (kbd "C-c v n") #'my/multi-vterm-in-current-directory)
  (global-set-key (kbd "C-c v p") #'multi-vterm-prev)
  (global-set-key (kbd "C-c v c") #'multi-vterm-next))

(use-package magit
  :straight t)

(use-package diff-hl
  :straight t
  :init
  (global-diff-hl-mode)
  :config
  ;; Highlight changes in the margin if you use a terminal
  (unless (display-graphic-p) (diff-hl-margin-mode))
  ;; Integration with Magit (reverts gutter when you commit)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package git-timemachine
  :straight t
  :bind ("C-c g t" . git-timemachine))

(use-package forge
  :straight t
  :after magit)

(use-package smerge-mode
  :ensure nil ;; Built-in
  :config
  (defun my/smerge-hydra ()
    "A simple way to manage conflicts."
    (interactive)
    (smerge-mode 1)
    ;; You can use M-x smerge-keep-current, etc.
    (message "Smerge active: Use C-c ^ n/p to move, C-c ^ m/t to keep mine/theirs")))

(use-package blamer
  :straight (blamer :type git :host github :repo "artawower/blamer.el")
  :init
  (global-blamer-mode 1)
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 70)
  ;; This ensures it doesn't conflict with other overlays
  (blamer-view 'post-command) 
  (blamer-max-commit-message-length 100)
  (blamer-author-formatter "✎ %s ")
  (blamer-commit-formatter "• %s")
  :config
  ;; Optional: Make the face look a bit more subtle (dimmed)
  (set-face-attribute 'blamer-face nil :foreground "#7a88cf" :italic t))

(use-package smartparens
  :straight t
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

;; Flycheck integration
(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(use-package treesit
  :identity t
  :config
  ;; 1. Define where to find the grammars
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (prisma "https://github.com/victorhqc/tree-sitter-prisma")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; 2. Logic to auto-install missing grammars
  (defun my/setup-install-grammars ()
    "Install all defined tree-sitter grammars if they aren't present."
    (interactive)
    (dolist (grammar treesit-language-source-alist)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; 3. Remap standard modes to TS modes automatically
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (go-mode . go-ts-mode)
          (go-dot-mod-mode . go-mod-ts-mode)
          (js-mode . js-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (css-mode . css-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (js-json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (rust-mode . rust-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode       . yaml-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (terraform-mode  . terraform-ts-mode)
          (toml-mode       . toml-ts-mode)
          (json-mode . json-ts-mode))))

;; 4. Prisma Specific Setup
(use-package prisma-ts-mode
  :straight (:host github :repo "nverno/prisma-ts-mode")
  :mode "\\.prisma\\'"
  :hook (prisma-ts-mode . lsp-deferred)
  :config
  ;; Register the prisma lsp client for the new ts-mode
  (with-eval-after-load 'lsp-mode
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("prisma-language-server" "--stdio"))
                      :major-modes '(prisma-ts-mode prisma-mode)
                      :priority 1
                      :server-id 'prisma-ls))))

(use-package lsp-mode
  :straight t
  :hook((rustic-mode . lsp)
        (python-ts-mode . lsp)
        (typescript-ts-mode . lsp)
        (jtsx-tsx-mode . lsp)
        (go-ts-mode . lsp))
  :bind(:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :custom
  ;; Performance optimizations for faster completion
  (setq lsp-completion-provider :capf)
  (setq lsp-prefer-capf t)
  (setq lsp-enable-completion-at-point t)
  (setq lsp-idle-delay 0.05)  ; Faster response (50ms)
  (setq lsp-log-io nil)  ; Disable LSP logging for performance
  (setq lsp-keep-workspace-alive nil)  ; Don't keep workspace alive unnecessarily
  (setq lsp-enable-snippet nil)  ; Disable snippets for faster completion
  (setq lsp-enable-indentation nil)  ; Disable LSP indentation
  (setq lsp-enable-on-type-formatting nil)  ; Disable on-type formatting
  (setq lsp-enable-text-document-color nil)  ; Disable color provider
  (setq lsp-enable-folding nil)  ; Disable folding
  (setq lsp-enable-dap-auto-configure nil)  ; Disable DAP auto-configure
  (setq lsp-diagnostics-provider :none)  ; Disable LSP diagnostics (use flycheck)
  ;;(setq lsp-enable-symbol-highlighting nil)  ; Disable symbol highlighting
  (setq lsp-lens-enable nil)  ; Disable code lenses
  (setq lsp-signature-auto-activate nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-typescript-format-enable nil)
  (setq lsp-javascript-format-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-headerline-imenu-mode t)
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-headerline-breadcrum-enable t)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-enable-code-actions t)
  (setq lsp-response-timeout 10)  ; Reduce timeout
  (setq lsp-print-io nil)  ; Disable I/O printing
  (setq lsp-trace nil)  ; Disable tracing
  (setq lsp-print-performance nil)  ; Disable performance logging
  (setq lsp-completion-cache-expiry 300)  ; Cache completions for 5 minutes
  (setq lsp-completion-cache-cleanup-interval 600)  ; Clean cache every 10 minutes
  (setq lsp-completion-cache-cleanup-threshold 100)  ; Clean when cache has 100+ items
  ;; Optimize completion sorting
  (setq lsp-completion-sort-function 'lsp-completion-sort-alphabetically)
  ;; Reduce memory usage
  (setq lsp-completion-cache-size 50)  ; Limit cache size
  (setq lsp-completion-cache-cleanup-threshold 25)); Clean more frequently
:config
(add-hook 'lsp-after-initialize-hook
          (lambda ()
            (setq-local before-save-hook (cons 'lsp-format-buffer before-save-hook))))


(use-package lsp-ui
  :straight t
  :commands (lsp-ui-mode)
  :custom
  ;;Sideline
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-delay 0.1)
  (lsp-ui-update-mode 'line)
  ;;Documentantion
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show-with-cursor nil)
  ;; IMenu
  (lsp-ui-imenu-window-width 0)
  (lsp-ui-imenu--custom-mode-line-format nil)
  :hook (lsp-mode . lsp-ui-mode))

;; Additional performance optimizations for LSP
(defun my/lsp-performance-setup ()
  "Setup LSP for maximum performance."
  ;; Disable expensive features
  (setq lsp-enable-folding nil)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-lens-enable nil)
  (setq lsp-signature-auto-activate nil)
  ;; Optimize completion
  (setq lsp-completion-cache-expiry 300)
  (setq lsp-completion-cache-cleanup-interval 600)
  (setq lsp-completion-cache-cleanup-threshold 50)
  (setq lsp-completion-cache-size 25)
  ;; Reduce I/O overhead
  (setq lsp-log-io nil)
  (setq lsp-print-io nil)
  (setq lsp-trace nil)
  (setq lsp-print-performance nil)
  ;; Optimize response times
  (setq lsp-response-timeout 5)
  (setq lsp-idle-delay 0.05))

;; Apply performance settings when LSP starts
(add-hook 'lsp-mode-hook #'my/lsp-performance-setup)

;; Alternative: Use Corfu instead of Company for faster completion
(defun my/use-corfu-instead-of-company ()
  "Switch to Corfu for faster completion."
  (when (bound-and-true-p company-mode)
    (company-mode -1))
  (when (bound-and-true-p corfu-mode)
    (corfu-mode 1)))

;; Uncomment the line below to automatically use Corfu instead of Company
(add-hook 'lsp-mode-hook #'my/use-corfu-instead-of-company)

;; (use-package rustic
;;   :straight t
;;   :hook
;;   (rust-ts-mode-hook . rustic-mode)
;;   :bind (:map rustic-mode-map
;;               ("C-c C-c r" . rustic-run)
;;               ("C-c C-c t" . rustic-test)
;;               ("C-c C-c c" . rustic-compile)
;;               ("C-c C-c l" . rustic-clippy)))


(use-package rustic
  :straight t
  :init
  ;; This is the magic line that makes rustic use tree-sitter
  (setq rustic-treesitter-derive t)
  :bind (:map rustic-mode-map
              ("C-c C-c r" . rustic-run)
              ("C-c C-c t" . rustic-test)
              ("C-c C-c c" . rustic-compile)
              ("C-c C-c l" . rustic-clippy))
  :config
  ;; Ensure rustic uses lsp-mode (it defaults to eglot in some versions)
  (setq rustic-lsp-client 'lsp-mode))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :hook ((go-ts-mode . lsp-deferred)
         (go-ts-mode . lsp-go-install-save-hooks))
  :bind (:map go-ts-mode-map
              ("C-c C-g" . gofmt))
  :custom
  (lsp-go-analyses
   '((nilness . t)
     (unusedwrite . t)
     (unusedparams . t)))
  :config
  (add-to-list 'exec-path "~/go/bin")
  (setq gofmt-command "goimports")
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)))

(use-package typescript-mode
  :straight t
  :hook ((typescript-mode . lsp-deferred)
         (js-mode . lsp-deferred))
  :config
  (defun lsp-typescript-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'typescript-mode-hook #'lsp-typescript-install-save-hooks)
  (setq typescript-indent-level 2))

;; Add specific setup for typescript-ts-mode
(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :hook ((typescript-ts-mode . lsp-deferred))
  :config
  ;; Re-use the function if available, or define it if not.
  ;; Since typescript-mode config defines it, we can just ensure it's loaded or redefine.
  ;; Simpler: just add the hooks directly or make sure the function is visible.
  (add-hook 'typescript-ts-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'lsp-format-buffer t t)
              (add-hook 'before-save-hook #'lsp-organize-imports t t))))

(defun my-react-lsp-setup ()
  "Setup LSP and save hooks for React (JSX/TSX) files."
  (lsp-deferred)
  (corfu-mode)
  (lsp-typescript-install-save-hooks)
  (hs-minor-mode))


(use-package jtsx
  :straight t
  :mode (("\\.jsx\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . my-react-lsp-setup)
         (jtsx-tsx-mode . my-react-lsp-setup))
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

(use-package python
  :straight t
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3"))

;; Optional: format buffer on save (if server supports it)
(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

(use-package solidity-mode
  :straight t
  :config
  (setq solidity-solc-path "/usr/bin/solc")
  (setq solidity-flycheck-solc-checker-active t))

(use-package sol-mode
  :straight t
  :mode "\\.sol\\'")

(defun my/org-format-all-elisp-blocks ()
  "Indent all emacs-lisp source blocks in the current Org buffer."
  (interactive)
  (org-babel-map-src-blocks nil
    (when (string= lang "emacs-lisp")
      (org-babel-do-in-edit-buffer
       (indent-region (point-min) (point-max))))))

(use-package yaml-ts-mode
  :straight t
  :mode ("\\.ya?ml\\'" . yaml-ts-mode)
  :hook (yaml-ts-mode . lsp-deferred))

(use-package dockerfile-ts-mode
  :straight t
  :mode ("Dockerfile\\'" . dockerfile-ts-mode)
  :hook (dockerfile-ts-mode . lsp-deferred))

(use-package terraform-mode
  :straight t
  :mode ("\\.tf\\'" . terraform-mode)
  :hook ((terraform-mode . lsp-deferred)
         (terraform-mode . terraform-format-on-save-mode)))

(use-package toml-ts-mode
  :straight t
  :mode ("\\.toml\\'" . toml-ts-mode)
  :hook (toml-ts-mode . lsp-deferred))

(use-package apheleia
  :straight t
  :config
  (apheleia-global-mode +1))

(with-eval-after-load 'apheleia
  (setf (alist-get 'prettier-tsx apheleia-formatters)
        '(("prettier"
           "--stdin-filepath" filepath
           "--parser" "typescriptreact"))))

(with-eval-after-load 'apheleia
  (setf (alist-get 'jtsx-tsx-mode apheleia-mode-alist) 'prettier-tsx
        (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'js-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'json-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'css-ts-mode apheleia-mode-alist) 'prettier))
