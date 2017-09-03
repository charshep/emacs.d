;;; init.el -- Modified from https://github.com/tonini/emacs.d/blob/master/init.el

;;; Debugging
(setq message-log-max 10000)

;; Please don't load outdated byte code
(setq load-prefer-newer t)

;; Bootstrap `use-package'
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/")
			 '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Customization - for package cus-edit if I ever use it
;(defconst my-custom-file (locate-user-emacs-file "customize.el")
;  "File used to store settings from Customization UI.")

(setq temporary-file-directory (expand-file-name "~/.emacs.d/tmp"))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Load my own lisp and keybindings
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-utils)
(require 'my-keybindings)


;;;
;; User interface

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;(when (and (eq system-type 'darwin) (fboundp 'menu-bar-mode))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-default 'truncate-lines t)

(delete-selection-mode 1)
(transient-mark-mode 1)

;; No blinking and beeping, no startup screen, no scratch message and short
;; Yes/No questions.
(blink-cursor-mode nil)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      echo-keystrokes 0.1
      linum-format " %d")
      ;initial-scratch-message "Howdy Sam!\n")
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
;(fset 'display-startup-echo-area-message #'ignore)

;; For global-hl-line-mode
;(set-face-background 'hl-line "#350")
(global-linum-mode)


;; Full screen
(set-frame-parameter nil 'fullscreen 'fullboth) ; no longer works in emacs 23
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Scroll behavior
(setq scroll-preserve-screen-position t
	  scroll-step 1)

;; Mode line stuff
(setq column-number-mode t)
(setq display-time-and-date t)
(setq display-time-day-and-date t)
(display-time)

;; utf-8 all the things
;(set-terminal-coding-system 'utf-8)
;(set-keyboard-coding-system 'utf-8)
;(set-selection-coding-system 'utf-8)
;(prefer-coding-system 'utf-8)




;;;
;; System setup

;; `gc-cons-threshold'

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html
;;
;; I have a modern machine ;)
;;
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(setq gc-cons-threshold 20000000)

(server-start)


;;;
;; Packages

(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-mode 1))

(use-package hl-line
  :init (global-hl-line-mode 1))

(use-package company						; COMP-lete ANY-ware
  :ensure t
  :init (global-company-mode) ; why is this in init and not in config?
  :config
  (progn
    ;(delete 'company-dabbrev company-backends)
    (setq company-tooltip-align-annotations t
	  company-tooltip-minimum-width 27
	  company-idle-delay 0.3
	  company-tooltip-limit 10
	  company-minimum-prefix-length 2
	  company-tooltip-flip-when-above t))
  :bind (:map company-active-map
              ("M-k" . company-select-next)
              ("M-i" . company-select-previous)
              ("TAB" . company-complete-selection))
  ;:diminish company-mode)
  )

(use-package eldoc)						; lisp info in echo area

(use-package desktop					; Save buffers, windows and frames
  ;:disabled
  :init (desktop-save-mode)
  :config
  ;; Save desktops a minute after Emacs was idle.
  (setq desktop-auto-save-timeout 60)

  ;; ;; Don't save Magit and Git related buffers
  ;; (dolist (mode '(magit-mode magit-log-mode))
  ;;   (add-to-list 'desktop-modes-not-to-save mode))
  ;; (add-to-list 'desktop-files-not-to-save (rx bos "COMMIT_EDITMSG")))
  )

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (setq yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode 1)
  ;:diminish (yas-minor-mode . " YS"))
  )

;;; Environment fixup

(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
    (when (string-match-p "/bash$" (getenv "SHELL"))
      ;; Use a non-interactive login shell.  A login shell, because my
      ;; environment variables are mostly set in `.bash_profile'.
      (setq exec-path-from-shell-arguments '("-l")))

    ;(dolist (var '("EMAIL" "INFOPATH" "JAVA_OPTS"))
    ;  (add-to-list 'exec-path-from-shell-variables var))

    (exec-path-from-shell-initialize)

    ;; (setq user-mail-address (getenv "EMAIL"))

    ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
    ;; already initializes info, we need to explicitly add the $INFOPATH
    ;; directories to `Info-directory-list'.  We reverse the list of info paths
    ;; to prepend them in proper order subsequently
    ;(with-eval-after-load 'info
    ;  (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
    ;    (when dir
    ;      (add-to-list 'Info-directory-list dir))))))
    ))

(use-package restclient
  :ensure t
  :defer t
  :mode ("\\.\\(http\\|rest\\)$" . restclient-mode)
  :config
  (progn
    (add-hook 'restclient-response-loaded-hook
	      (defun pulse-entire-buffer ()
		(save-excursion
		  (goto-char (point-min))
		  (pulse-momentary-highlight-region (point-min) (point-max)))))))
	      
	
(provide 'init)


;;;
; Generated by emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(debug-on-error t)
 '(history-delete-duplicates t)
 '(history-length t)
 '(hscroll-margin 20)
 '(hscroll-step 1)
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(ns-function-modifier (quote hyper))
 '(package-selected-packages
   (quote
    (exec-path-from-shell yasnippet use-package swift-mode company-sourcekit calmer-forest-theme)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
