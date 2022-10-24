;; I like to keep config for a bunch of themes commented out so I can
;; switch between them.

(use-package base16-theme
  :ensure t
  :config
  ;; (load-theme 'base16-3024 t)
  ;; (load-theme 'base16-apathy t)
  ;; (load-theme 'base16-ashes t)
  ;; (load-theme 'base16-atelier-cave-light t)
  ;; (load-theme 'base16-atelier-cave t)
  ;; (load-theme 'base16-atelier-dune-light t)
  ;; (load-theme 'base16-atelier-dune t)
  ;; (load-theme 'base16-atelier-estuary-light t)
  ;; (load-theme 'base16-atelier-estuary t)
  ;; (load-theme 'base16-atelier-forest-light t)
  ;; (load-theme 'base16-atelier-forest t)
  ;; (load-theme 'base16-atelier-heath-light t)
  ;; (load-theme 'base16-atelier-heath t)
  ;; (load-theme 'base16-atelier-lakeside-light t)
  ;; (load-theme 'base16-atelier-lakeside t)
  ;; (load-theme 'base16-atelier-plateau-light t)
  ;; (load-theme 'base16-atelier-plateau t)
  ;; (load-theme 'base16-atelier-savanna-light t)
  ;; (load-theme 'base16-atelier-savanna t)
  ;; (load-theme 'base16-atelier-seaside-light t)
  ;; (load-theme 'base16-atelier-seaside t)
  ;; (load-theme 'base16-atelier-sulphurpool-light t)
  ;; (load-theme 'base16-atelier-sulphurpool t)
  ;; (load-theme 'base16-atlas t)
  ;; (load-theme 'base16-bespin t)
  ;; (load-theme 'base16-black-metal-bathory t)
  ;; (load-theme 'base16-black-metal-burzum t)
  ;; (load-theme 'base16-black-metal-dark-funeral t)
  ;; (load-theme 'base16-black-metal-gorgoroth t)
  ;; (load-theme 'base16-black-metal-immortal t)
  ;; (load-theme 'base16-black-metal-khold t)
  ;; (load-theme 'base16-black-metal-marduk t)
  ;; (load-theme 'base16-black-metal-mayhem t)
  ;; (load-theme 'base16-black-metal-nile t)
  ;; (load-theme 'base16-black-metal t)
  ;; (load-theme 'base16-black-metal-venom t)
  ;; (load-theme 'base16-brewer t)
  ;; (load-theme 'base16-bright t)
  ;; (load-theme 'base16-brogrammer t)
  ;; (load-theme 'base16-brushtrees-dark t)
  ;; (load-theme 'base16-brushtrees t)
  ;; (load-theme 'base16-chalk t)
  ;; (load-theme 'base16-circus t)
  ;; (load-theme 'base16-classic-dark t)
  ;; (load-theme 'base16-classic-light t)
  ;; (load-theme 'base16-codeschool t)
  ;; (load-theme 'base16-cupcake t)
  ;; (load-theme 'base16-cupertino t)
  ;; (load-theme 'base16-darktooth t)
  ;; (load-theme 'base16-decaf t)
  ;; (load-theme 'base16-default-dark t)
  ;; (load-theme 'base16-default-light t)
  ;; (load-theme 'base16-dracula t)
  ;; (load-theme 'base16-eighties t)
  ;; (load-theme 'base16-embers t)
  ;; (load-theme 'base16-espresso t)
  ;; (load-theme 'base16-flat t)
  ;; (load-theme 'base16-framer t)
  ;; (load-theme 'base16-fruit-soda t)
  ;; (load-theme 'base16-github t)
  ;; (load-theme 'base16-google-dark t)
  ;; (load-theme 'base16-google-light t)
  ;; (load-theme 'base16-grayscale-dark t)
  ;; (load-theme 'base16-grayscale-light t)
  ;; (load-theme 'base16-greenscreen t)
  ;; (load-theme 'base16-gruvbox-dark-hard t)
  (load-theme 'base16-gruvbox-dark-medium t)
  ;; (load-theme 'base16-gruvbox-dark-pale t)
  ;; (load-theme 'base16-gruvbox-dark-soft t)
  ;; (load-theme 'base16-gruvbox-light-hard t)
  ;; (load-theme 'base16-gruvbox-light-medium t)
  ;; (load-theme 'base16-gruvbox-light-soft t)
  ;; (load-theme 'base16-harmonic-dark t)
  ;; (load-theme 'base16-harmonic-light t)
  ;; (load-theme 'base16-heetch-light t)
  ;; (load-theme 'base16-heetch t)
  ;; (load-theme 'base16-helios t)
  ;; (load-theme 'base16-hopscotch t)
  ;; (load-theme 'base16-horizon-dark t)
  ;; (load-theme 'base16-horizon-light t)
  ;; (load-theme 'base16-horizon-terminal-dark t)
  ;; (load-theme 'base16-horizon-terminal-light t)
  ;; (load-theme 'base16-ia-dark t)
  ;; (load-theme 'base16-ia-light t)
  ;; (load-theme 'base16-icy t)
  ;; (load-theme 'base16-irblack t)
  ;; (load-theme 'base16-isotope t)
  ;; (load-theme 'base16-macintosh t)
  ;; (load-theme 'base16-marrakesh t)
  ;; (load-theme 'base16-materia t)
  ;; (load-theme 'base16-material-darker t)
  ;; (load-theme 'base16-material-lighter t)
  ;; (load-theme 'base16-material-palenight t)
  ;; (load-theme 'base16-material t)
  ;; (load-theme 'base16-material-vivid t)
  ;; (load-theme 'base16-mellow-purple t)
  ;; (load-theme 'base16-mexico-light t)
  ;; (load-theme 'base16-mocha t)
  ;; (load-theme 'base16-monokai t)
  ;; (load-theme 'base16-nord t)
  ;; (load-theme 'base16-nova t)
  ;; (load-theme 'base16-ocean t)
  ;; (load-theme 'base16-oceanicnext t)
  ;; (load-theme 'base16-one-light t)
  ;; (load-theme 'base16-onedark t)
  ;; (load-theme 'base16-outrun-dark t)
  ;; (load-theme 'base16-papercolor-dark t)
  ;; (load-theme 'base16-papercolor-light t)
  ;; (load-theme 'base16-paraiso t)
  ;; (load-theme 'base16-phd t)
  ;; (load-theme 'base16-pico t)
  ;; (load-theme 'base16-pop t)
  ;; (load-theme 'base16-porple t)
  ;; (load-theme 'base16-railscasts t)
  ;; (load-theme 'base16-rebecca t)
  ;; (load-theme 'base16-sandcastle t)
  ;; (load-theme 'base16-seti t)
  ;; (load-theme 'base16-shapeshifter t)
  ;; (load-theme 'base16-snazzy t)
  ;; (load-theme 'base16-solarflare t)
  ;; (load-theme 'base16-solarized-dark t)
  ;; (load-theme 'base16-solarized-light t)
  ;; (load-theme 'base16-spacemacs t)
  ;; (load-theme 'base16-summerfruit-dark t)
  ;; (load-theme 'base16-summerfruit-light t)
  ;; (load-theme 'base16-synth-midnight-dark t)
  ;; (load-theme 'base16-tomorrow-night-eighties t)
  ;; (load-theme 'base16-tomorrow-night t)
  ;; (load-theme 'base16-tomorrow t)
  ;; (load-theme 'base16-tube t)
  ;; (load-theme 'base16-twilight t)
  ;; (load-theme 'base16-unikitty-dark t)
  ;; (load-theme 'base16-unikitty-light t)
  ;; (load-theme 'base16-woodland t)
  ;; (load-theme 'base16-xcode-dusk t)
  ;; (load-theme 'base16-zenburn t)
  ;; Set the cursor color based on the evil state
  ;; (defvar my/base16-colors base16-rebecca-colors)
  ;; (defvar my/base16-colors base16- )
  ;; (setq evil-emacs-state-cursor   `(,(plist-get my/base16-colors :base0D) box)
  ;;       evil-insert-state-cursor  `(,(plist-get my/base16-colors :base0D) bar)
  ;;       evil-motion-state-cursor  `(,(plist-get my/base16-colors :base0E) box)
  ;;       evil-normal-state-cursor  `(,(plist-get my/base16-colors :base0B) box)
  ;;       evil-replace-state-cursor `(,(plist-get my/base16-colors :base08) bar)
  ;;       evil-visual-state-cursor  `(,(plist-get my/base16-colors :base09) box))
  )

;; (use-package dracula-theme
;;   :ensure t
;;   :config
;;   (load-theme 'dracula t))

;; (use-package gruvbox-theme
;;   :ensure t
;;   :config
;;   ;; (load-theme 'gruvbox t) ;aka gruvbox-dark-medium
;;   ;; (load-theme 'gruvbox-dark-soft t)
;;   (load-theme 'gruvbox-dark-medium t)
;;   ;; (load-theme 'gruvbox-dark-hard t)
;;   ;; (load-theme 'gruvbox-light-soft t)
;;   ;; (load-theme 'gruvbox-light-medium t)
;;   ;; (load-theme 'gruvbox-light-hard t)
;;   )

;; (use-package moe-theme
;;   :ensure t
;;   :demand t
;;   :after powerline
;;   :init
;;   (setq powerline-image-apple-rgb (eq window-system 'ns))
;;   :config
;;   ;; (load-theme 'moe-light t)
;;   (load-theme 'moe-dark t)
;;   (powerline-moe-theme))

;; (use-package nord-theme
;;   :ensure t
;;   :config
;;   (load-theme 'nord t))

;; (use-package phoenix-dark-pink-theme
;;   :ensure t
;;   :config
;;   (load-theme 'phoenix-dark-pink t))

;; (use-package parchment-theme
;;   :ensure t
;;   :config
;;   (load-theme 'parchment t))

;; (use-package poet-theme
;;   :ensure t
;;   :config
;;   ;; (load-theme 'poet)
;;   (load-theme 'poet-dark))

;; (use-package kaolin-themes
;;   :ensure t
;;   :config
;;   ;; (load-theme 'kaolin-dark t)
;;   ;; (load-theme 'kaolin-aurora t)
;;   ;; (load-theme 'kaolin-bubblegum)
;;   ;; (load-theme 'kaolin-light t)
;;   ;; (load-theme 'kaolin-eclipse t)
;;   ;; (load-theme 'kaolin-ocean t)
;;   ;; (load-theme 'kaolin-galaxy t)
;;   ;; (load-theme 'kaolin-valley-dark t)
;;   ;; (load-theme 'kaolin-temple t)
;;   )


(provide 'my-themes)
