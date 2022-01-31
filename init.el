
;;; UI Customization

;; Disable unneeded UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq use-dialog-box nil ; Don't prompt with dialog boxes
      inhibit-startup-message t) ; Don't show the splash screen

;;; Basic Configuration

;; Keep customization settings out of init.el
(setq custom-file
      (if (boundp 'server-socket-dir)
            (expand-file-name "custom.el" server-socket-dir)
          (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Configure the editor
(hl-line-mode 1)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Org Mode
(defun my/org-buffer-setup ()
  (display-line-numbers-mode -1)
  (org-indent-mode 1)
  (variable-pitch-mode 1))
(add-hook 'org-mode-hook #'my/org-buffer-setup)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Set frame translucency
(set-frame-parameter (selected-frame) 'alpha '(70 . 70))
(add-to-list 'default-frame-alist '(alpha . (70 . 70)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Configure the theme
(setq modus-themes-mode-line '(accented borderless 3d)
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-tabs-accented t
      modus-themes-org-blocks 'tinted-background
      modus-themes-paren-match '(bold intense underline)
      modus-themes-prompts '(intense bold)
      modus-themes-completions 'opinionated
      modus-themes-mixed-fonts t
      modus-themes-scale-headings t
      modus-themes-region '(accented)
      modus-themes-syntax '(faint alt-syntax green-strings yellow-comments))
(setq modus-themes-headings
      '((1 . (rainbow 1.4))
        (2 . (rainbow 1.3))
	(3 . (rainbow 1.2))
        (t . t)))
(load-theme 'modus-vivendi t t)
(enable-theme 'modus-vivendi)

;; Set the font
(set-face-attribute 'default nil :font "JetBrains Mono" :height 200)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 200)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 220)

;; Set up a nicer completion experience
(setq completion-styles '(flex))
(setq max-mini-window-height 7)
(setq resize-mini-windows 'grow-only)
(icomplete-vertical-mode 1)

;; Remember previous completions (recall with M-p)
(setq history-length 25)
(savehist-mode 1)

;; Helpful key bindings
(global-set-key (kbd "M-o") #'other-window)

;; Customize the clock
(display-time-mode 1)

;; Tab bar customization
(setq tab-bar-close-button-show nil
      tab-bar-format '(tab-bar-format-history tab-bar-format-tabs-groups tab-bar-separator tab-bar-format-align-right tab-bar-format-global))
(tab-bar-mode 1)

(setq my/mode-line-prefix (propertize "TOO " 'face '(:inherit 'default :background "LawnGreen" :foreground "Blue")))

(defun my/mode-line-buffer-status ()
  (concat
   (propertize ">" 'face '(:inherit 'default :height 0.8 :background "LawnGreen" :foreground "Black"))
   (propertize ">" 'face '(:inherit 'default :height 0.8 :background "orange1" :foreground "Black"))))

(defun my/mode-line-project()
  (if (project-current)
      (file-name-base (directory-file-name (project-root (project-current))))
    ""))

;; Mode line customization
(setq-default mode-line-format
	      '((:eval (my/mode-line-buffer-status))
		mode-line-modified
		" "
		(:eval (my/mode-line-project))
		" "
		mode-line-buffer-identification
		"   "
		mode-line-position
		(vc-mode vc-mode)
		"  "
		mode-line-modes
		mode-line-misc-info
		mode-line-end-spaces))
