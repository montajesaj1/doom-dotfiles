;; User info init
(setq user-full-name "AJ Montajes"
      user-mail-address "montajes@student.ubc.ca")

;; UI Configs
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)
(setq visible-bell t)
(auto-save-mode)
(setq default-frame-alist '((undecorated . t)))
(setq-default line-spacing 2)


(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(column-number-mode)
(global-display-line-numbers-mode t)
(global-set-key (kbd "C-x l") 'menu-bar--display-line-numbers-mode-relative)
(global-set-key (kbd "C-x L") 'menu-bar--display-line-numbers-mode-absolute)

(use-package marginalia)
(marginalia-mode)

(use-package vertico-posframe)
(vertico-posframe-mode 1)
(setq vertico-posframe-parameters
      '((left-fringe . 10)
        (right-fringe . 10)))
(setq vertico-posframe-border-width 5)
(setq vertico-posframe--overlays-back nil)
(setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)

;; Disable line numbers for certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq
 org-tags-column 0
 org-modern-horizontal-rule "──────────")

;; Org-Tufte configuration
(use-package org-tufte
  :ensure nil
  :init (add-to-list 'load-path "/Users/user/custom_pkg/org-tufte.el")
  :config
  (require 'org-tufte)
  (setq org-tufte-htmlize-code t))

;; Org-Timeblock configuration
(use-package org-timeblock)
(use-package org-hyperscheduler)

;; Font Configuration
(setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'medium))
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :weight 'medium)
(set-face-attribute 'variable-pitch nil :font "Iosevka" :weight 'light :slant 'italic)

;; Theme configuration
(use-package gruvbox)
(setq doom-theme 'gruvbox)

;; Doom modeline configuration
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-height 20)
           (doom-modeline-icon t)
           (doom-modeline-bar-width 4)
           (doom-modeline-window-width-limit 85)
           (display-time-mode 1)
           (display-battery-mode 1)))

(setq doom-modeline-time-icon t)
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-rust t)
(setq doom-modeline-env-load-string "...")
(setq doom-modeline-unicode-fallback nil)

;; Line numbers display style
(setq display-line-numbers-type t)

;; Org-mode configurations
(setq org-directory "~/Sync/")
(blink-cursor-mode 1)
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-mode-hook #'org-bullets-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; Org Bullets configuration
(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "->" "○" "●" "○" "->" "●"))
  (org-ellipsis "⤵")
  :hook (org-mode . org-bullets-mode))

;; Visual Fill Column configuration
(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; Neotree configuration
(use-package neotree)
(global-set-key (kbd "C-c TAB") 'neotree-toggle)

;; Org modern mode
(use-package org-modern
  :custom
  (org-modern-))
(global-set-key [f7] 'org-modern-mode)

;; CDLaTeX mode configuration
(use-package cdlatex)
(add-hook 'org-mode-hook #'turn-on-org-cdlatex)
(global-set-key [f9] 'cdlatex-mode)

;; Minimap configuration
(use-package minimap)
(setq minimap-window-location 'right)
(setq minimap-numlines t)
(global-set-key (kbd "C-c C-c") #'minimap-mode)
(set-frame-parameter nil 'alpha-background 70)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; Transparency toggle function
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Anki editor configuration
(use-package anki-editor)
(setq anki-editor-create-decks t)

;; Coding + Writing modes
(defun writing-mode ()
  (interactive)
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (setq display-line-numbers nil)
  (org-modern-mode)
  (org-bullets-mode)
  (+org-pretty-mode)
  (load-theme 'doom-flatwhite)
  (justify-current-line )
  (visual-fill-column-mode 1))

(use-package zenburn-theme)
(use-package gruvbox-theme)
(defun coding-mode ()
  (interactive)
  (setq display-line-numbers t)
  (setq visual-fill-column-width 300
        visual-fill-column-center-text t)
  (load-theme 'gruvbox-dark-medium)
  (visual-fill-column-mode nil))

(global-set-key (kbd "C-x w m") 'writing-mode)
(global-set-key (kbd "C-x w c") 'coding-mode)
(global-set-key (kbd "C-x t DEL") 'kill-current-buffer)
;; (global-set-key (kbd "s-f") 'swiper-isearch)
(global-set-key (kbd "s-]") 'evil-window-next)
(global-set-key (kbd "s-[") 'evil-window-prev)
(global-set-key (kbd "s-;") 'split-window-horizontally)
(global-set-key (kbd "s-'") 'split-window-vertically)

;; (global-set-key (kbd "s-{}") '+workspace:switch-previous)

;; ;; Space leader bindings
;; ;; Use `ivy` for buffer and file navigation with space as a leader key in normal mode
;; (require 'evil)
;; (require 'ivy)

;; Define a global prefix map for the space key
;; (defvar my-space-map (make-sparse-keymap)
  ;; "My custom keymap for space key prefix.")

;; Define the desired key sequences using the new prefix map
;; (evil-define-key 'normal global-map (kbd "SPC") my-space-map)

;; (evil-define-key 'normal my-space-map (kbd "b") #'ivy-switch-buffer)
;; (evil-define-key 'normal my-space-map (kbd "f") #'find-file)

;; For the "SPC f r" sequence, define 'f' as a prefix within the space map
;; (define-prefix-command 'my-space-f-map)
;; (evil-define-key 'normal my-space-map (kbd "f") 'my-space-f-map)
;; (evil-define-key 'normal my-space-f-map (kbd "r") #'ivy-recentf)
;; (evil-define-key 'normal my-space-map (kbd "M-f") #'swiper)
;; Add more keybindings as desired, following the same pattern:
;; (evil-define-key 'normal my-space-map (kbd "x y") #'some-command)

(use-package org-download
  :after org
  :defer nil
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-image-actual-width 500)
  (org-download-screenshot-method "/usr/local/bin/pngpaste %s")
  :bind
  ("C-M-y" . org-download-screenshot)
  :config
  (require 'org-download))

(global-set-key (kbd "s-D") 'dictionary-search)

;; Ensure Emacs is initialized properly with the given configurations
(when (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (load-theme 'gruvbox-dark-medium t)))))
