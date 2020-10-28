(require 'autothemer)

(autothemer-deftheme
 hallo "A theme to set the mood for Halloween"

 ((((class color) (min-colors #xFFFFFF))) ;; We're only concerned with graphical Emacs

  ;; Define our color palette
  (hallo-black      "#000000")
  (hallo-white      "#ffffff")
  (hallo-orange     "orange1")
  (hallo-dk-orange  "#eb6123")
  (hallo-purple     "MediumPurple2")
  (hallo-dk-purple  "MediumPurple4")
  (hallo-green      "LightGreen"))

 ;; Customize faces
 ((default                   (:foreground hallo-white :background hallo-black))
  (cursor                    (:background hallo-dk-orange))
  (region                    (:background hallo-dk-purple))
  (mode-line                 (:background hallo-dk-purple))
  (font-lock-keyword-face    (:foreground hallo-purple))
  (font-lock-constant-face   (:foreground hallo-green))
  (font-lock-string-face     (:foreground hallo-orange))
  (font-lock-builtin-face    (:foreground hallo-green))

  (org-level-1               (:foreground hallo-orange))))

(provide-theme 'hallo)
