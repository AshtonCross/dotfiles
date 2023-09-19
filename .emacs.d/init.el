;;; package --- Summary

;;; Commentary:
;;; generated  using https://emacs.amodernist.com/ with tweaks from my
;;; old config.

;;; Personal configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.

;; Add the NonGNU ELPA package archive
(require 'package)
(add-to-list 'package-archives  '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(unless package-archive-contents  (package-refresh-contents))

;; Load a custom theme
(load-theme 'deeper-blue t)

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable splash screen
(setq inhibit-startup-screen t)

;; Enable line numbering in `prog-mode'
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; LSP Support
(unless (package-installed-p 'eglot)
  (package-install 'eglot))

;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'eglot-ensure)

;;; Inline static analysis

;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)

;; Display messages when idle, without prompting
(setq help-at-pt-display-when-idle t)

;;; Pop-up completion
(unless (package-installed-p 'corfu)
  (package-install 'corfu))

;; Enable autocompletion by default in programming buffers
(add-hook 'prog-mode-hook #'corfu-mode)

;;; Git client
(unless (package-installed-p 'magit)
  (package-install 'magit))

;; Bind the `magit-status' command to a convenient key.
(global-set-key (kbd "C-c g") #'magit-status)

;;; Indication of local VCS changes
(unless (package-installed-p 'diff-hl)
  (package-install 'diff-hl))

;; Enable `diff-hl' support by default in programming buffers
(add-hook 'prog-mode-hook #'diff-hl-mode)

;;; Clojure Support
(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))

;;; Haskell Support
(unless (package-installed-p 'haskell-mode)
  (package-install 'haskell-mode))

;;; Lua Support
(unless (package-installed-p 'lua-mode)
  (package-install 'lua-mode))

;;; Additional Lisp support
(unless (package-installed-p 'sly)
  (package-install 'sly))

;;; LaTeX support
(unless (package-installed-p 'auctex)
  (package-install 'auctex))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Enable LaTeX math support
(add-hook 'LaTeX-mode-map #'LaTeX-math-mode)

;; Enable reference mangment
(add-hook 'LaTeX-mode-map #'reftex-mode)

;;; Markdown support
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;;; Outline-based notes management and organizer

;;; Additional Org-mode related functionality
(unless (package-installed-p 'org-contrib)
  (package-install 'org-contrib))

;;; IRC Client

;; Connect to Librea
(setq rcirc-server-alist
      '(("irc.libera.chat" :channels ("#emacs")
         :port 6697 :encryption tls)))

;; Set your IRC nick
(setq rcirc-default-nick "nil")
(add-hook 'rcirc-mode-hook #'rcirc-track-minor-mode)
(add-hook 'rcirc-mode-hook #'rcirc-omit-mode)

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


(load (expand-file-name "~/.quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "sbcl")

;;
;; legacy config stuff
;;

(display-battery-mode 1)
(display-time-mode 1)
(toggle-scroll-bar -1)
(blink-cursor-mode 0)
(setq scroll-conservatively 100)
(global-prettify-symbols-mode 1)

;; curosr
(setq-default cursor-type 'box)
(set-cursor-color "#fff000")
(global-hl-line-mode t)

;; elfeed

(unless (package-installed-p 'elfeed)
  (package-install 'elfeed))

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

;; org mode conf
(add-hook 'org-mode-hook 'org-indent-mode)

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
    )))

(provide 'init)
;;; init.el ends here
