(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;;(set-face-attribute 'default nil :font "Fira Code Retina" :height 250)

(load-theme 'wombat)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;;(add-to-list 'package-archives
;;             '("melpa" . "https://melpa.org/packages/") t) 

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;; Display line numbers in global
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
		org-mode))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; show a window on right hand of the keys usage binding
(use-package command-log-mode)
(global-command-log-mode t)

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

;; make sure swiper is installed to use in ivy mode
(use-package swiper :ensure t)

;; doom mode need the icons in UI, so we import this package
(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; doom-themes let to find the themes you like best by `counsel-load-themes`
(use-package doom-themes
  :init (load-theme 'doom-acario-dark t))

;; rainbow-delimiters is a "rainbow parentheses"-like mode which
;; highlights delimiters such as parentheses, brackets or braces according to their depth.
;; prog-mode is the basic programming mode of all other languages
;; hook means when run prog-mode, then invoke rainbow-delimiters
;; so, all the programming modes base on pro-mode will run the hook too
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key is a helping key tips package that could check the next step keys
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package counsel
  :bind (("M-x" . 'counsel-M-x)
	 ("C-x b" . 'counsel-ibuffer)
	 ("C-x f" . 'counsel-find-file)   
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

;; Ivy-rich is to display the meta command detail for you not just a name list
(use-package ivy-rich
;;  :after ivy
  :init
  (ivy-rich-mode 1))


;; helpful is helpful, provides much more contextaul information.
;; we can replace the counsel describe to helpful and remap keymappings
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; General is a key-binding packages to maintain keybindings
;; rune/leader-keys is a customized keys namespace
;; we use 't' as the leader letter toggles
(use-package general
   :config
   (general-create-definer rune/leader-keys
     :keymaps '(normal insert visual emacs)
     :prefix "SPC"
     :global-prefix "C-SPC")

   (rune/leader-keys
     "t"  '(:ignore t :which-key "toggles")
     "tt" '(counsel-load-theme :which-key "choose theme")))

;; VIM IN EMACS, EVIL!
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-c-u-scroll t)
  (setq evil-want-c-i-jump nil)
  :config
  (evil-mode 0)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
 :after evil
 :config
 (evil-collection-init))


;; HYDRA LET YOU SCALE YOU TEXT
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))


(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Works/Projects")
    (setq projectile-project-search-path '("~/Works/Projects" "~/Works/Projects/git") )
    )
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))
 
(use-package ccls
  :init (setq ccls-sem-highlight-method 'font-lock)
  :hook ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp)))
  :config (setq ccls-executable "/usr/local/bin/ccls"))

;; https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
