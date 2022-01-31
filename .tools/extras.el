;; What follows is a minimal implementation of tarsius' Keycast package.
;; I'm implementing it here to avoid pulling in an outside package!

(defvar efs/last-command-info "")

(defface efs/show-command-face
  '((t . (:foreground "Black"
       	  :background "LawnGreen"
	        :family "JetBrains Mono"
	        :height 0.9
          :weight bold
          :box (:line-width -3 :style released-button))))
  "Styling for how the last command is shown in the mode line")

(defun efs/pre-command-handler ()
  (setq efs/last-command-info
	(format " %s // %s "
		(ignore-errors
		  (key-description (this-single-command-keys)))
		this-command))
  (force-mode-line-update t))

(defun efs/show-last-command ()
  (concat
   "    "
   (propertize efs/last-command-info 'face 'efs/show-command-face)
   "    "))

(setq global-mode-string '((:eval (efs/show-last-command))))
(add-hook 'pre-command-hook #'efs/pre-command-handler t)
