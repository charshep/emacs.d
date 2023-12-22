;;; init.el -- Modified from https://github.com/tonini/emacs.d/blob/master/init.el

;;; Debugging
(setq message-log-max 10000)

;; Please don't load outdated byte code
(setq load-prefer-newer t)

;; Bootstrap `use-package'
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'bind-key)

(use-package diminish :ensure t)

;; Customization
(defconst my-custom-file (locate-user-emacs-file "customize.el")
  "File used to store settings from Customization UI.")

(setq temporary-file-directory (expand-file-name "~/.emacs.d/tmp"))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Load my own lisp and keybindings
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-utils)
(require 'my-keybindings)

;; gc tweaks for minibuffer
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 100000000)) ; upped further for lsp

;; lsp, see lsp-doctor output or https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;; User interface

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (eq system-type 'gnu/linux) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-default 'truncate-lines t)
(set-default 'indent-tabs-mode nil)

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

(global-linum-mode)
(display-time)

; TODO investigate conifuring these
;; original
;;(add-to-list 'default-frame-alist '(font . "-*-courier new-medium-r-normal--0-0-0-0-m-0-iso10646-1"))
;;(modify-frame-parameters (selected-frame) '((font . "-*-courier new-medium-r-normal--0-0-0-0-m-0-iso10646-1")))
;; Tonini's updated (looked ugly in dired)
;; (set-face-attribute 'default nil
;;                     :family "Source Code Pro" :height 160)
;; (set-face-attribute 'variable-pitch nil
;;                     :family "Fira Sans" :height 140 :weight 'regular)

(set-frame-parameter nil 'fullscreen 'fullboth)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(load-theme 'calmer-forest t nil)

;; utf-8 all the things
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; System setup

;; `gc-cons-threshold'

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html
;;
;; I have a modern machine ;)
;;
(setq gc-cons-threshold 20000000)

;; not sure yet whether I want all these
(setq delete-old-versions t
;      make-backup-files nil
;      create-lockfiles nil
      ring-bell-function 'ignore
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(server-start)

;; Fix for Symbol's value as variable is void: shell-mode-map when loading helm below
; not sure if still needed
;(require 'shell)


;;;
;; Packages

;;; Manage Emacs Customize
(use-package cus-edit
  :defer t
  :config
  (setq custom-file my-custom-file
        custom-buffer-done-kill nil            ; Kill when existing
        custom-buffer-verbose-help nil         ; Remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil)
  :init (load my-custom-file 'no-error 'no-message))

;;; OS X support
(use-package ns-win                     ; OS X window support
  :defer t
  :if (eq system-type 'darwin)
  :config
  (setq ns-pop-up-frames nil            ; Don't pop up new frames from the
                                        ; workspace
        mac-option-modifier 'super      ; Nees super for lsp keyboard prefix
        mac-command-modifier 'meta      ; Option is simply the natural
        mac-right-command-modifier 'left
        mac-right-option-modifier 'none ; Keep right option for accented input
        ;; Just in case we ever need these keys
        mac-function-modifier 'hyper))

(use-package exec-path-from-shell
  :ensure t
  :config
  (progn
    (exec-path-from-shell-initialize)
    ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
    ;; already initializes info, we need to explicitly add the $INFOPATH
    ;; directories to `Info-directory-list'.  We reverse the list of info paths
    ;; to prepend them in proper order subsequently
    (with-eval-after-load 'info
      (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
        (when dir
          (add-to-list 'Info-directory-list dir))))))

;;; Helm
(use-package helm
  :ensure t
  :config
  (progn
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
		  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
		  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
		  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
		  helm-ff-file-name-history-use-recentf t
		  helm-echo-input-in-header-line t)
										; skipping for now helm-hide-minibuffer-maybe but it would go here
    (setq helm-autoresize-max-height 40)
    (setq helm-autoresize-min-height 0)
    (helm-autoresize-mode 1)
    (setq helm-buffers-fuzzy-matching t)
    (helm-mode 1))
  :bind (("M-x" . helm-M-x)
		 ("C-x b" . helm-mini)
		 ("C-x C-f" . helm-find-files))
  :bind	 (:map helm-map
			   ("TAB" . helm-execute-persistent-action)
			   ("C-i" . helm-execute-persistent-action) ; make TAB work in terminal
			   ("C-z" . helm-select-action))
  :bind (:map shell-mode-map
			  ("C-c C-l" . helm-comint-input-ring))
  :bind (:map minibuffer-local-map
			  ("C-c C-l" . helm-minibuffer-history)))

;;; Hilight current line
 (use-package hl-line
   :init (global-hl-line-mode 1))

;;; improved paren management
(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (dolist (hook '(inferior-emacs-lisp-mode-hook
                  emacs-lisp-mode-hook)))
    ;(add-hook hook #'smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always)
  :bind
  (:map smartparens-mode-map
	("C-c s u" . sp-unwrap-sexp)
	("C-c s w" . sp-rewrap-sexp))
  :diminish (smartparens-mode))

(use-package magit
  :ensure t
  :defer 2
  :bind (("C-x g" . magit-status))
  ; :config
  ; i like to use vc-diff as a quick way to view changes without navigating to the magit buffer
  ;(progn
  ;  (delete 'Git vc-handled-backends)))
)

;;; Swift Mode
;(use-package swift-mode :ensure t)

;;; Projectile
(use-package projectile
  :ensure t
  :pin melpa
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'helm)
  (projectile-global-mode)
  (helm-projectile-on)
  (setq projectile-enable-caching nil)
  :diminish (projectile-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;;; COMPlete ANYwhere
(use-package company						; COMP-lete ANY-ware
  :ensure t
  :init (global-company-mode) ; why is this in init and not in config?
  ;:init
  ;(setq company-backends '((company-files company-keywords company-capf company-dabbrev-code company-etags company-dabbrev)))
  :config
  (progn
    ;(delete 'company-dabbrev company-backends)
    (setq company-tooltip-align-annotations t
	  company-tooltip-minimum-width 27
	  company-idle-delay 0.3
	  company-tooltip-limit 10
	  company-minimum-prefix-length 2
	  company-tooltip-flip-when-above t))
	;(global-company-mode))
  :bind (:map company-active-map
              ("M-k" . company-select-next)
              ("M-i" . company-select-previous)
              ("TAB" . company-complete-selection))
  :diminish company-mode)
  ;)

;;; LSP
;; https://emacs-lsp.github.io/lsp-mode/page/installation/
;; see lsp-sourcekit below for objc integration
(use-package lsp-mode
  :hook
  (objc-mode . (lambda () (lsp-deferred) (electric-indent-local-mode -1))) ; electric overrides clang-format
  (swift-mode . (lambda () (lsp-deferred) (electric-indent-local-mode 1))) ; lsp does not support formatting for swift so use swift-mode/electric-mode instead
  :commands (lsp-deferred))

;;; LSP protocol implementation for Swift/Objc
(use-package lsp-sourcekit
  :after lsp-mode
  :config
  ;;(setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain") ; guessing this from executable path returned from xcrun --find sourcekit-lsp
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Applications/Xcode-15.0.1.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain")
  ;;(setq lsp-sourcekit-executable (expand-file-name "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))
  ;;(setq lsp-sourcekit-executable (expand-file-name "/Applications/Xcode-14.1.0.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))
  (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp")))
  (setq lsp-clients-clangd-executable (string-trim (shell-command-to-string "xcrun --find clangd")))
  ;;(setq lsp-clients-clangd-executable (expand-file-name "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clangd"))
) ; TODO run xcrun --find clangd
;;TODO lsp-clients-clangd-executable OR keep relying on path in $PATH

(use-package winner                     ; Undo and redo window configurations
  :init (winner-mode))

(use-package desktop                    ; Save buffers, windows and frames
  ;;:disabled t
  :init (desktop-save-mode)
  :config
  ;; Save desktops a minute after Emacs was idle.
  (setq desktop-auto-save-timeout 60)

  ;; Don't save Magit and Git related buffers
  (dolist (mode '(magit-mode magit-log-mode))
    (add-to-list 'desktop-modes-not-to-save mode)))
  ;(add-to-list 'desktop-files-not-to-save (rx bos "COMMIT_EDITMSG"))) compile error

;; TODO multiple-cursors

(use-package yasnippet
  :ensure t
  :defer t
  :config
  ;(setq yas-snippet-dirs "~/.emacs.d/snippets") yasnippet no longer includes default snippets, installed as separate packages
  (yas-global-mode 1)
  :diminish (yas-minor-mode . " YS"))

;; Rest
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
(put 'dired-find-alternate-file 'disabled nil) ; note, this line is autogenerated after opting in to disabling
