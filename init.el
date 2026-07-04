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

;; `straight-recipe-overrides' is ((PROFILE . RECIPE-LIST) ...); default profile
;; is nil. A bare (nerd-icons ...) at top level is ignored, which caused the
;; “two different recipes” :files warning (MELPA vs your fork).
(let ((cell (assoc nil straight-recipe-overrides))
      (nerd-recipe '(nerd-icons :type git :host github
                                :repo "rainstormstudio/nerd-icons.el"
                                :files (:defaults "data"))))
  (if cell
      (unless (assq 'nerd-icons (cdr cell))
        (setcdr cell (cons nerd-recipe (cdr cell))))
    (push (cons nil (list nerd-recipe)) straight-recipe-overrides)))

;; Prevent straight from shadowing built-in packages that ship with Emacs 29+.
;; Without this, straight pulls an external `project` package that conflicts
;; with the one already compiled into the Emacs binary.
(dolist (pkg '(project xref flymake jsonrpc eldoc seq))
  (straight-use-package `(,pkg :type built-in)))

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

;; Doom Themes
(use-package doom-themes
  :straight t
  :demand t
  :init
  ;; Load theme early to avoid white flash on startup
  (load-theme 'doom-monokai-pro t)

  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects and improves org-mode colors
  (doom-themes-org-config)

  ;; Optional: nicer bold/italic styling
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))




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
        ;; Clear prior remaps: repeated hooks stacked `face-remap-add-relative' and
        ;; broke face inheritance (e.g. gnus-group-news-low cycle with doom-theme).
        (when (fboundp 'face-remap-reset-base)
          (face-remap-reset-base 'default))
        (face-remap-add-relative 'default `(:background ,bg))
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

(use-package treemacs-icons-dired
  :straight t)

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
  (evil-define-key 'normal prog-mode-map "gr" 'xref-find-references)
  (evil-define-key 'normal prog-mode-map "gd" 'xref-find-definitions)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'message-buffer-mode 'normal))

(define-key evil-normal-state-map (kbd "Q") (lambda () (interactive) (kill-current-buffer)))

;; Magit before evil-collection: avoids void `magit-auto-revert-mode' when Evil
;; initializes magit bindings (byte-code can touch that variable).
(use-package magit
  :straight t
  :demand t)

;; Evil Nerd Commenter
(use-package evil-nerd-commenter
  :straight t
  :defer t
  :bind
  ("C-c c" . evilnc-comment-or-uncomment-lines))

;; Evil Collection
(use-package evil-collection
  :straight t
  :after (evil magit)
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

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list (cape-capf-super
                               #'eglot-completion-at-point
                               #'cape-dabbrev
                               #'cape-file)))))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion))
     (eglot (styles orderless flex)))))

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
  :init
  ;; Built-in default `org-modules' includes `ol-gnus'; require fails without Gnus.
  ;; Set before Org loads so `defcustom' does not re-enable it.
  (setq org-modules
        '(ol-doi ol-w3m ol-bbdb ol-bibtex ol-docview ol-info ol-irc ol-mhe ol-rmail ol-eww))
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

(use-package diff-hl
  :init
  (global-diff-hl-mode))

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
  :straight nil
  :demand t
  :config

  ;; Grammar sources
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript"  "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript"  "tsx/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Auto install missing grammars
  (defun my/ensure-treesit-grammars ()
    (dolist (grammar treesit-language-source-alist)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  (add-hook 'emacs-startup-hook #'my/ensure-treesit-grammars)

  ;; Modern TS mode remaps
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (css-mode . css-ts-mode)
          (js-mode . js-ts-mode)
          (js-json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (typescript-tsx-mode . tsx-ts-mode)
          (go-mode . go-ts-mode)
          (go-dot-mod-mode . go-mod-ts-mode)
          (rust-mode . rust-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (json-mode . json-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)))

  ;; Explicit file associations
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode)))

  ;; 4. Prisma Specific Setup
  (use-package prisma-ts-mode
    :straight (:host github :repo "nverno/prisma-ts-mode")
    :mode "\\.prisma\\'"
    :hook (prisma-ts-mode . eglot-ensure))
    ;; Register the prisma lsp client for the new ts-mode

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :custom
  (eldoc-box-max-pixel-height 400)
  (eldoc-box-max-pixel-width 600))

(use-package rust-mode
  :straight t
  :defer t)

(use-package go-mode
  :straight t
  :defer t)

(use-package python-mode
  :straight t
  :defer t)

;; OCaml
(use-package tuareg
  :straight t
  :defer t)

;; TypeScript / JavaScript / JSX / TSX (replaces typescript-mode + js2-mode)
(use-package jtsx
  :straight t
  :defer t
  :mode (("\\.js\\'"  . jtsx-jsx-mode)
         ("\\.jsx\\'" . jtsx-jsx-mode)
         ("\\.ts\\'"  . jtsx-typescript-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode))
  :custom
  (jtsx-switch-ts-backend 'treesit))

;; Solidity

(use-package solidity-mode
  :straight t
  :defer t)

;; C / C++ — built into Emacs via cc-mode, no extra package needed.
;; c-ts-mode and c++-ts-mode are also built-in (Emacs 29+).
;; cmake support
(use-package cmake-mode
  :straight t
  :defer t)

;; Install grammars automatically on first use.
;; Run M-x treesit-install-language-grammar for each language on first setup.
(use-package treesit
  :straight nil
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq treesit-language-source-alist
        '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
          (html       . ("https://github.com/nickel-lang/tree-sitter-html"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))))

  ;; Remap classic major modes to their tree-sitter equivalents.
  ;; This makes Emacs auto-activate -ts-mode when a grammar is available.
  (setq major-mode-remap-alist
        '((python-mode     . python-ts-mode)
          (js-mode         . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (go-mode         . go-ts-mode)
          (rust-mode       . rust-ts-mode)
          (bash-mode       . bash-ts-mode)
          (sh-mode         . bash-ts-mode)
          (css-mode        . css-ts-mode)
          (json-mode       . json-ts-mode)
          (yaml-mode       . yaml-ts-mode)
          (c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode))))

;; tsx files -> tsx-ts-mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; js files -> js-ts-mode (overrides any earlier entry)
(add-to-list 'auto-mode-alist '("\\.js\\'"  . js-ts-mode) t)
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode) t)
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-ts-mode) t)

(use-package eglot
  :straight nil
  :hook ((rust-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (tuareg-mode . eglot-ensure)
         (solidity-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure))
  :config
  ;; Server programs
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(go-ts-mode . ("gopls")))
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode js-ts-mode)
                 . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("basedpyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(tuareg-mode . ("ocamllsp")))
  (add-to-list 'eglot-server-programs
               '(solidity-mode . ("nomicfoundation-solidity-language-server" "--stdio")))
  ;; C / C++
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode) . ("clangd")))
  ;; Performance
  (setq eglot-events-buffer-size 0)
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t)
  ;; Suppress verbose JSON-RPC logging entirely
  (fset #'jsonrpc--log-event #'ignore))
