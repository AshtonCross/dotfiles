;;
;; package managers
;;

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

(package-initialize)
(package-refresh-contents)

;; Use package

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; dired tweak (stop unnessisary buffers)
(put 'dired-find-alternate-file 'disabled nil)

;; mostly just for i3. not very good looking outside of tiling wm's
;; (set-frame-parameter nil 'alpha-background 95)
;; (add-to-list 'default-frame-alist '(alpha-background . 95))

;; move annoying *~ files to better location.
(setq make-backup-files nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(cond
 ((string-equal system-type "windows-nt")
  (progn
    (message "Microsoft Windows"))
  (set-face-attribute 'default nil :height 120)) ; emacs is small in win
 ((string-equal system-type "darwin") ; macosx
  (progn
    (message "Mac OS X"))
  (setq mac-command-modifier 'control)
  (set-face-attribute 'default nil :height 150)) ; emacs is so tiny in mac
 ((string-equal system-type "gnu/linux")
  (progn
    (message "Linux")
    )))

;; custom.el 
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; org mode conf
(add-hook 'org-mode-hook 'org-indent-mode)

;; display line numbers 
(when (version<= "26.0.50" emacs-version )
;;  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode))

;; tweaks to ui

(display-battery-mode 1)
(display-time-mode 1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(blink-cursor-mode 0)
(menu-bar-mode -1)

;; other tweaks
(setq scroll-conservatively 100)
(global-prettify-symbols-mode 1)

;; weird theme mix 


(use-package gruvbox-theme
  :ensure t)
(load-theme 'gruvbox-dark-hard)
;;(load-theme 'gruvbox-light-hard)

(use-package almost-mono-themes
  :ensure t)
(load-theme 'almost-mono-gray)

;; you can load these two at the same time for a kinda cool theme

;; cursor
(setq-default cursor-type 'box)
(set-cursor-color "#fff000")
(global-hl-line-mode t)

(setq default-frame-alist '((font . "Iosevka-12")))

(message "Config Successfully Loaded")
(print "config sucessfully loaded")

;; TODO: LSP Support 
