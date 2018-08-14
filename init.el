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

;; Disable "For information about GNU Emacs and the GNU system, type
;; C-h C-a." message on startup
(setq inhibit-startup-echo-area-message "jed")

;; Configure package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

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
  :init
  ;; (use-package face-remap :delight (text-scale-mode))
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

(setq inhibit-startup-screen t)
(blink-cursor-mode 0)
(tool-bar-mode -1)
(menu-bar-mode 1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

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
        ))

;;;
;;;
;;; Global keybindings

(global-set-key (kbd "<f7>") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "s-\\")
                (lambda ()
                  (interactive)
                  (other-window 1)
                  (delete-window)))

;;;
;;;
;;; Evil mode config

(use-package evil
  :ensure t :demand t
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode 1)
  (add-hook 'view-mode-hook 'evil-motion-state))

(use-package evil-cleverparens
  :after evil :ensure t :demand t
  :init
  (add-hook 'emacs-lisp-mode-hook 'evil-cleverparens-mode)
  (add-hook 'lisp-mode-hook 'evil-cleverparens-mode)
  )

(use-package evil-collection
  :after evil :ensure t :demand t
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
  (evil-leader/set-key
    "s" 'save-buffer
    "b" 'switch-to-buffer
    "k" 'kill-buffer))

(use-package evil-magit
  :after (evil magit) :ensure t :demand t :defer t)

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
  :ensure t :defer t
  :commands (cider-jack-in cider-connect)
  :init
  (evil-leader/set-key
    "c j" 'cider-jack-in
    "c c" 'coder-connect))

(use-package clojure-mode :ensure t :defer t)

;; Emacs init.el profiling
(use-package esup
  :ensure t :defer t
  :commands (esup))

(use-package exec-path-from-shell
  :ensure t :defer 5
  :commands (exec-path-from-shell-initialize)
  :init
  (defun exec-path-from-shell-initialize-safely ()
    (interactive)
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))
  :config
  (exec-path-from-shell-initialize-safely))

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

(use-package helm
  :ensure t :defer t :delight helm-mode
  :commands (helm-find-files)
  :init
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (evil-leader/set-key
    "e" 'helm-find-files)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :config
  (require 'helm-config)
  (helm-mode 1))

;; (use-package highlight-parentheses
;;   :ensure t
;;   :demand t
;;   :config
;;   (global-highlight-parentheses-mode))

;; Best git porcelain EVER!!!!!!!!!
(use-package magit
  :after evil-leader :ensure t :defer t
  :commands (magit-status magit-clone)
  :init
  (evil-leader/set-key
    "g"   'magit-status
    "m s" 'magit-status)
  :config
  (require 'evil-magit))

(use-package projectile :ensure t :defer t)

(use-package helm-projectile
  :ensure t :defer t
  :commands (helm-projectile-find-file)
  :init
  (evil-leader/set-key
    "p f" 'helm-projectile-find-file))

;; Manage parens so I don't have to
(use-package smartparens
  :ensure t :demand t
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode 1))

;; Custom mode line
(use-package spaceline
  :ensure t :demand t
  :init
  (setq powerline-image-apple-rgb t)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(use-package terraform-mode :ensure t :mode "\\.tf\\'")

;;; Finalization

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

;;; init.el ends here
