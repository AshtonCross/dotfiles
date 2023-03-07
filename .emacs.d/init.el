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

;; general settings

;; dired conf
(put 'dired-find-alternate-file 'disabled nil)

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
(global-prettify-symbols-mode 1)
(blink-cursor-mode 0)

;; gruvbox

(use-package gruvbox-theme
  :ensure t)
(load-theme 'gruvbox-dark-hard)
;;(load-theme 'gruvbox-light-hard)

;; curosr
(setq-default cursor-type 'bar)
(set-cursor-color "#fff000")

;;
;; elfeed
;;

(use-package elfeed
  :ensure t)

(global-set-key (kbd "C-x w") 'elfeed) ; nice bind

;; G - Fetch feed updates
;; g - refresh view of feed listing
;; s - update serach filter
;; c - clear search filter

(setq elfeed-feeds
      '(
	;; feeds go in here
	;; format:
	;; ("https://path.to/feed.xml" tag1 tag2 tag3 etc)
	("https://lukesmith.xyz/lindy.xml" reading store philosophy)
	("https://denshi.org/index.xml" computers)
	("https://lukesmith.xyz/index.xml" computers philosophy)
	("https://notrelated.xyz/rss" philosophy podcast)
	))

;;
;; behavior
;;

;; move those annoying ***~ files to a nice spot
(setq make-backup-files nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; resizing window keybinds (from emacs wiki)
;; frame workflow is better; don't be "faaking idiot"

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; auto-fill-mode
(add-hook 'prog-mode-hook 'auto-fill-mode)
(setq comment-auto-fill-only-comments t)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; emoji
(setf use-default-font-for-symbols nil)
(set-fontset-font t 'unicode "Noto Emoji" nil 'append)

;; auctex
;;(use-package auctex
;;  :ensure t)

;;;
;;; bindings
;;;

;;(use-package evil :ensure t) ; vi keybindings
;;(evil-mode)

;; (use-package ergoemacs-mode :ensure t) ; ergo-macs
;; (use-package xah-fly-keys :ensure t) ; xah fly keys (only for genuises with 9k iq)

;;
;; version specific settings (adjustments to font and window size)
;;

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
    ;;(set-frame-font "Iosevka-15" nil t)
    ;;(set-frame-font "Terminus (ttf) Medium 15" nil t) 
    ;;(set-face-attribute 'default nil :height 150) ; emacs is so tiny
    ;;in mac
    )))
