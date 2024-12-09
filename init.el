;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst emacs-start-time (current-time))

;; Reduce startup time. Ripped from
;; https://github.com/jwiegley/dot-emacs

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Configure package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(defun my/package-installed-p (pkg)
  "Check if package is installed. In emacs26 package-installed-p
seems to require package-initialize iff the package is *not*
installed. This prevents calling package-initialized if all
packages are already installed which improves startup time."
  (condition-case nil
      (package-installed-p pkg)
    (error
     (package-initialize)
     (package-installed-p pkg))))

(when (not (my/package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; delight and bind-key are used by use-package

;; Remove modes from minibar
(use-package delight
  :ensure t :demand t
  :config
  (delight '((undo-tree-mode "" undo-tree)
             (eldoc-mode "" eldoc)
             (evil-commentary-mode "" evil-commentary)
             (evil-cleverparens-mode "" evil-cleverparens)
             (auto-revert-mode "" autorevert)
             (smartparens-mode "" smartparens))))

(use-package bind-key :ensure t :demand t)

;;;
;;;
;;; Basic look and feel

(eval-after-load "startup" '(fset 'display-startup-echo-area-message (lambda ())))
(setq inhibit-startup-screen t)
(blink-cursor-mode 0)
(tool-bar-mode -1)
(menu-bar-mode 1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
;; (setq visible-bell t)
(setq ring-bell-function (lambda ()))

;;;
;;;
;;; Emacs file handling

;; Keep emacs "custom" settings in separate file and load it
(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; Don't litter the filesystem with backups
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;;;
;;;
;;; Frame and font settings

(setq default-frame-alist
      '((top . 0) (left . 259)          ;pixels
        (width . 100) (height . 54)     ;characters
        (font . "Input Mono 16")
        (ns-transparent-titlebar . t)
        (ns-appearance . dark)
        ))

;;;
;;;
;;; Global keybindings

(global-set-key (kbd "<f7>") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "<f8>") (lambda () (interactive) (find-file "~/.emacs.d/lisp/my-themes.el")))
(global-set-key (kbd "<f9>") (lambda () (interactive) (find-file "~/.emacs.d/lisp/local-init.el")))
(global-set-key (kbd "s-\\")
                (lambda ()
                  (interactive)
                  (other-window 1)
                  (delete-window)))

(defun my/clear-other-window-output ()
  (interactive)
  (let ((window (cl-find-if (lambda (w)
                              (with-current-buffer (window-buffer w)
                                (or (bound-and-true-p eshell-mode)
                                    (bound-and-true-p cider-mode))))
                            (window-list))))
    (with-current-buffer (window-buffer window)
      (my/clear-eshell-or-cider-output))))

(global-set-key (kbd "s-|") 'my/clear-other-window-output)

;;; Windows support

;; Make left windows key act as super
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key

;;;
;;;
;;; Evil mode config

(use-package evil
  :ensure t :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-minibuffer nil)
  :config
  (evil-mode 1)
  (add-hook 'view-mode-hook 'evil-motion-state)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle))

(use-package evil-cleverparens
  :after evil :ensure t :demand t
  :init
  (require 'evil-cleverparens-text-objects)
  (add-hook 'emacs-lisp-mode-hook 'evil-cleverparens-mode)
  (add-hook 'lisp-mode-hook 'evil-cleverparens-mode)
  (add-hook 'clojure-mode-hook 'evil-cleverparens-mode)
  (add-hook 'clojurescript-mode-hook 'evil-cleverparens-mode)
  (add-hook 'cider-repl-mode-hook 'evil-cleverparens-mode))

(use-package evil-collection
  :after evil
  :ensure t
  :demand t
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil :ensure t :demand t
  :config
  (evil-commentary-mode))

(use-package evil-extra-operator
  :after evil :ensure t :demand t
  :init
  (setq evil-extra-operator-eval-key (kbd "ge"))
  :config
  (add-to-list #'evil-extra-operator-eval-modes-alist
               '(clojurescript-mode cider-eval-region))
  (add-to-list #'evil-extra-operator-eval-modes-alist
               '(clojurec-mode cider-eval-region))
  (global-evil-extra-operator-mode))

(use-package evil-leader
  :after evil :ensure t :demand t
  :init
  (setq evil-leader/in-all-states 1)
  :config
  (evil-leader/set-leader ",")
  (evil-leader-mode 1)          ;evil-leader/set-leader kills the mode
  (global-evil-leader-mode 1)
  (define-key evil-normal-state-map (kbd "\\") 'evil-repeat-find-char-reverse)

  ;; Random custom functions (TODO: find a better place to put them)
  (defun my/close-other-byffers ()
    (interactive)
    (let ((cbuf (current-buffer)))
      (mapc 'kill-buffer
            (seq-filter (lambda (buf) (not (eq buf cbuf)))
                        (buffer-list)))))

  (defun my/indent-buffer ()
    (interactive)
    (save-excursion
      (mark-whole-buffer)
      (indent-for-tab-command)))

  (defun my/find-projects-dir ()
    (interactive)
    (dired "~/git/projects"))

  (evil-leader/set-key
    "w"   'save-buffer
    "c b" 'my/close-other-byffers
    "k"   'kill-buffer
    "P l" 'my/find-projects-dir
    "P f" 'my/find-projects-file
    "f r" 'raise-sexp
    "i"   'my/indent-buffer))

;;;
;;;
;;; Random packages

;; Never use tabs and don't add unneeded space when aligning code
(use-package align
  :config
  (defadvice align-regexp (around align-regexp-with-spaces activate)
    (let ((indent-tabs-mode nil))
      ad-do-it)))

(use-package cider
  :ensure t
  :defer nil
  :after exec-path-from-shell
  ;; :commands (cider-jack-in cider-jack-in-clj cider-jack-in-cljs cider-connect)
  :init

  (defun my/cider-jack-in-shadow ()
    (interactive)
    (let ((cider-preferred-build-tool 'shadow-cljs))
      (cider-jack-in-cljs '())))

  (defun my/integrant-reset ()
    (interactive)
    (cider-interactive-eval "(integrant.repl/reset)"
                            nil
                            nil
                            (cider--nrepl-pr-request-map)))

  (defun my/clear-eshell-or-cider-output ()
    (interactive)
    (if (bound-and-true-p eshell-mode)
        (eshell/clear-scrollback)
      (cider-find-and-clear-repl-output t)))

  (evil-leader/set-key
    "c j" 'cider-jack-in
    ;; "c J" 'my/cider-jack-in-shadow
    "c J" 'cider-jack-in-cljs
    "c c" 'coder-connect
    "c q" 'cider-quit
    "c k" 'my/clear-eshell-or-cider-output
    ;; "c k" 'cider-repl-clear-buffer
    "c r" 'my/integrant-reset
    "c e" 'cider-eval-buffer))

(use-package clojure-mode :ensure t :defer t
  ;; :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'" "\\.tmpl\\'" "\\.edn\\'")
  :config
  (defconst my/clojure-indentations
    '(

      (apply . 0)
      (map . 0)
      (str . 0)
      (union . 0)
      (intersection . 0)
      (recur . 0)
      (comp . 0)

      (not= . 0)
      (= . 0)
      (println . 0)
      (format . 0)

      ;; other
      (update-package-json . 0)
      (get . 0)

      ;; core
      (merge-with . 1)
      (merge . 0)

      ;; re-frame
      (reg-event-fx . 1)
      (reg-event-db . 1)
      (reg-sub . 1)

      ;; tmp/custom
      (get-ident . 1)
      (get-query . 1)
      (get-initial-state . 1)

      ;; Manifold
      (let-flow . 1)

      ;; Core
      (mod . 0)
      (/ . 0)
      (assoc . 1)
      (into . 1)
      (add-watch . 2)
      (or . 0)
      (mapv . 0)
      (filterv . 0)
      (filter . 0)
      (.. . 0)
      (join . 0)
      (set . 0)
      (-> . 0)
      (->> . 0)
      (some-> . 0)
      (some->> . 0)
      (and . 0)
      (ns . defn)
      (+ . 0)
      (* . 0)
      (reduce . 0)
      (mapcat . 0)

      ;; HTML
      (article . defn)
      (button . 1)
      (div . defn)
      (figure . 1)
      (h1 . defn)
      (h2 . defn)
      (h3 . defn)
      (h4 . defn)
      (h5 . defn)
      (h6 . defn)
      (header . defn)
      (section . defn)
      (form . defn)
      (nav . defn)
      (a . defn)
      (ul . defn)
      (li . defn)
      (input . defn)

      ;; Fulcro/Om.next
      (render . 1)
      (transact! . 1)
      (action . 1)
      (add-form-config . 1)
      (load . 2)
      (route-to! . 2)
      (start! . 2)
      (begin! . 3)

      ;; Fulcro - common conventions and names for mutations
      (file-upload . 1)

      ;; Compojure
      (GET . 2)
      (context . 2)

      ))

  (setq clojure-indent-style 'always-indent)
  (dolist (item my/clojure-indentations)
    (put-clojure-indent (car item) (cdr item))))

;;; completion

;; hooks into normal emacs completion fns to provide better UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (define-key minibuffer-local-map (kbd "C-j") 'next-line)
  (define-key minibuffer-local-map (kbd "C-k") 'previous-line))

;; allows space separated fuzzy matches
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :ensure t
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.5)
  :init
  (global-corfu-mode))

;;; project and code navigation

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :commands (consult-find consult-grep)
  :init
  (setq consult-async-refresh-delay 0.1)
  (evil-leader/set-key
    "p f" 'consult-find
    "p g" 'consult-grep)
  :config
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?"))
  )

;;; code quality and formatting

(use-package apheleia
  :ensure t
  :delight
  :config
  (apheleia-global-mode +1))

(use-package dockerfile-mode :ensure t :defer t)

(use-package eshell
  :init
  (defun directory-name-base (dirpath)
    (file-name-nondirectory (directory-file-name dirpath)))

  (defun my/shell-name (&optional suffix)
    (format "*eshell*<%s%s>" (directory-name-base default-directory) (if suffix suffix "")))

  (defun my/buffer-open? (name)
    (seq-reduce (lambda (res buf) (or res (string= name (buffer-name buf))))
                (buffer-list)
                nil))

  (defun my/start-shell (&optional create-new? n)
    "Start a shell named after the current buffer."
    (interactive "P")
    (let ((eshell-buffer-name (if create-new? (my/shell-name (or n 1)) (my/shell-name))))
      (when (and (memq window-system '(ns))
                 (not (string-match-p "/usr/local/bin" eshell-path-env)))
        (exec-path-from-shell-initialize-safely))
      (if (not (my/buffer-open? eshell-buffer-name))
          (eshell)
        (if (not create-new?)
            (pop-to-buffer eshell-buffer-name)
          (my/start-shell t (1+ (or n 1)))))))

  (evil-leader/set-key "s" 'my/start-shell)

  )

;; Emacs init.el profiling
(use-package esup
  :ensure t :defer t
  :commands (esup))

(use-package exec-path-from-shell
  :ensure t :defer 1
  :commands (exec-path-from-shell-initialize)
  :init
  (defun exec-path-from-shell-initialize-safely ()
    (interactive)
    (when (memq window-system '(ns x))
      (exec-path-from-shell-initialize)
      (setq-default eshell-path-env (string-join exec-path ":"))))
  :config
  (dolist (var '("HOMEBREW_GITHUB_API_TOKEN"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize-safely))

(use-package groovy-mode :ensure t :mode  "(\\.groovy\\|\\.gradle)\\'")

(use-package linum
  :config
  (global-linum-mode 1))

;; Fix whitespace on save and always use spaces
(use-package files
  :init
  (setq mode-require-final-newline t)
  (setq-default indent-tabs-mode nil)
  :config
  (add-hook 'before-save-hook
            (lambda ()
              (when (not indent-tabs-mode)
                (untabify (point-min) (point-max)))
              (delete-trailing-whitespace))))

(use-package guix
  :ensure t
  :defer t
  :commands (guix guix-all-packages)
  :init
  (evil-leader/set-key
    "G" 'guix))

(use-package emacs
  :init
  (evil-leader/set-key
    "e" 'find-file))

;; Best git porcelain EVER!!!!!!!!!
(use-package magit
  :after evil-leader :ensure t :defer t
  :commands (magit-status magit-clone)
  :init
  (evil-leader/set-key
    "g"   'magit-status
    "m s" 'magit-status
    "m c" 'magit-clone))

(use-package markdown-mode :ensure t :mode "\\.md\\'")

(use-package org-bullets
  :ensure t :defer t
  :commands (org-bullets-mode)
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; (use-package org-tree-slide :ensure t :defer t
;;   :bind
;;   (("<f8>" . org-tree-slide-mode)
;;    ("<f9>" . org-tree-slide-move-previous-tree)
;;    ("<f10>" . org-tree-slide-move-next-tree)))

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package restclient :ensure t :mode ("\\.restclient\\'" . restclient-mode))

(use-package smartparens
  :ensure t :demand t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1))

(use-package terraform-mode :ensure t :mode "\\.tf\\'")

;; Python pipenv uses toml for config ಠ_ಠ
(use-package toml-mode :ensure t :commands (toml-mode))

(use-package yasnippet :ensure t
  :init
  (add-hook 'mhtml-mode-hook (lambda () (yas-minor-mode 1)))
  (add-hook 'web-mode-hook (lambda () (yas-minor-mode 1)))
  )

(use-package yaml-mode :ensure t :mode "(\\.yaml\\|\\.yml)\\'")

(use-package envrc
  :ensure t
  :init
  (evil-leader/set-key
    "E r" 'envrc-reload
    "E a" 'envrc-allow))

;;; eye candy

(use-package beacon
  :ensure t
  :delight
  :demand t
  :config
  (beacon-mode 1))

(use-package minimap
  :ensure t
  :init
  (setq minimap-window-location 'right)
  (setq minimap-update-delay 0.1)
  (custom-set-faces
   '(minimap-active-region-background
     ((((background dark)) (:background "SlateBlue1" :extend t))
      (t (:background "#D3D3D3222222" :extend t)))
     "Face for the active region in the minimap.
By default, this is only a different background color."
     :group 'minimap))
  (evil-leader/set-key
    "d m" 'minimap-mode))

(use-package moody
  :ensure t
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :ensure t
  :after moody
  :config
  (minions-mode 1))

(use-package neotree
  :ensure t
  :init
  (evil-leader/set-key
    "d t" 'neotree))

(require 'my-themes)

;; Support local config untracked by git
(if (file-exists-p "~/.emacs.d/lisp/local-init.el")
    (load "local-init")
  (message "local-init.el does not exist"))

(exec-path-from-shell-initialize-safely)
(envrc-global-mode)

;; Start server to support emacsclient
(server-start)

;;; Finalization

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

;;; init.el ends here
