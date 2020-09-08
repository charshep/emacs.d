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

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(require 'cl) ; for search function

; fix for broken cert authorities on OS X version of emacs: https://blog.vifortech.com/posts/emacs-tls-fix/
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem")

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

;; Fix for Symbol's value as variable is void: shell-mode-map when loading helm below
(require 'shell)


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
(electric-pair-mode 1)

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
(add-to-list 'default-frame-alist '(font . "-*-courier new-medium-r-normal--0-0-0-0-m-0-iso10646-1"))
(modify-frame-parameters (selected-frame) '((font . "-*-courier new-medium-r-normal--0-0-0-0-m-0-iso10646-1")))
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

(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(server-start)


;;;
;; Packages

(use-package ido
  :disabled ; switching to helm
  :config
  (setq ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-mode 1))

; from tuhdo.github.io/helm-intro.html
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

(use-package hl-line
  :init (global-hl-line-mode 1))

(use-package company						; COMP-lete ANY-ware
  :ensure t
  ;:init (global-company-mode) ; why is this in init and not in config?
  :init
  (setq company-backends '((company-files company-keywords company-capf company-dabbrev-code company-etags company-dabbrev)))
  :config
  (progn
    ;(delete 'company-dabbrev company-backends)
    (setq company-tooltip-align-annotations t
	  company-tooltip-minimum-width 27
	  company-idle-delay 0.3
	  company-tooltip-limit 10
	  company-minimum-prefix-length 2
	  company-tooltip-flip-when-above t)
	(global-company-mode))
  :bind (:map company-active-map
              ("M-k" . company-select-next)
              ("M-i" . company-select-previous)
              ("TAB" . company-complete-selection))
  ;:diminish company-mode)
  )

;; https://emacs-lsp.github.io/lsp-mode/page/installation/
;; see lsp-sourcekit below for objc integration
(use-package lsp-mode
  :hook (objc-mode . lsp-deferred)
  :commands (lsp-deferred)
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
  ;(setq yas-snippet-dirs "~/.emacs.d/snippets") yasnippet no longer includes default snippets, installed as separate packages
  (yas-global-mode 1)
  ;:diminish (yas-minor-mode . " YS"))
  )

(use-package projectile
  :ensure t
  :pin melpa
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain") ; guessing this from executable path returned from xcrun --find sourcekit-lsp
  (setq lsp-sourcekit-executable (expand-file-name "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))) ; TODO run xcrun --find sourcekit-lsp directly
;;TODO lsp-clients-clangd-executable OR keep relying on path in $PATH

;; BUILDING json-compilation-database for clangd:
;; 1. install via brew xcpretty
;; 2. In Xcode: build project
;; 3. In Xcode: export build log (TODO perfrom this via command line? may be stuck using UI or building on command line and piping to xcpretty: http://joemburgess.com/2014/10/04/diving-into-xcode-logs/)
;; 4. cat xcodebuild.log |xcpretty -r json-compilation-database --output compile_commands.json
;; TODO make alias or script and link to emacs?

;; NOTE for CRM need to disable module debugging in project file (or optionally disable on command line if building on CL?)
;; from https://github.com/MaskRay/ccls/issues/330
;; I believe it’s the “-gmodules” flag in your command string in compile_commands.json. If you look at the clang documentation for “-gmodules”, it states:
;; “This option transparently switches the Clang module format to object file containers...”
;; I had the same issue that was resolved with removing that flag.
;; It can be removed in Xcode, under build settings, with the “Enable clang module debugging” option set to “NO”

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
 '(auto-revert-check-vc-info nil)
 '(blink-cursor-mode nil)
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "cc-mode"))))
 '(calendar-latitude 37.690175)
 '(calendar-longitude -121.895285)
 '(completion-ignored-extensions
   (quote
    (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".dex")))
 '(custom-safe-themes
   (quote
    ("8dc7f4a05c53572d03f161d82158728618fb306636ddeec4cce204578432a06d" default)))
 '(debug-on-error t)
 '(global-auto-revert-mode t)
 '(global-magit-file-mode t)
 '(helm-adaptive-mode t nil (helm-adaptive))
 '(helm-boring-file-regexp-list
   (quote
    ("\\.o$" "~$" "\\.bin$" "\\.lbin$" "\\.so$" "\\.a$" "\\.ln$" "\\.blg$" "\\.bbl$" "\\.elc$" "\\.lof$" "\\.glo$" "\\.idx$" "\\.lot$" "\\.svn\\(/\\|$\\)" "\\.hg\\(/\\|$\\)" "\\.git\\(/\\|$\\)" "\\.bzr\\(/\\|$\\)" "CVS\\(/\\|$\\)" "_darcs\\(/\\|$\\)" "_MTN\\(/\\|$\\)" "\\.fmt$" "\\.tfm$" "\\.class$" "\\.fas$" "\\.lib$" "\\.mem$" "\\.x86f$" "\\.sparcf$" "\\.dfsl$" "\\.pfsl$" "\\.d64fsl$" "\\.p64fsl$" "\\.lx64fsl$" "\\.lx32fsl$" "\\.dx64fsl$" "\\.dx32fsl$" "\\.fx64fsl$" "\\.fx32fsl$" "\\.sx64fsl$" "\\.sx32fsl$" "\\.wx64fsl$" "\\.wx32fsl$" "\\.fasl$" "\\.ufsl$" "\\.fsl$" "\\.dxl$" "\\.lo$" "\\.la$" "\\.gmo$" "\\.mo$" "\\.toc$" "\\.aux$" "\\.cp$" "\\.fn$" "\\.ky$" "\\.pg$" "\\.tp$" "\\.vr$" "\\.cps$" "\\.fns$" "\\.kys$" "\\.pgs$" "\\.tps$" "\\.vrs$" "\\.pyc$" "\\.pyo$" "\\.dex$")))
 '(helm-buffer-max-length nil)
 '(helm-ff-skip-boring-files t)
 '(helm-grep-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.dex")))
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(history-delete-duplicates t)
 '(history-length t)
 '(hscroll-margin 20)
 '(hscroll-step 1)
 '(indent-tabs-mode nil)
 '(magit-diff-refine-hunk t)
 '(magit-dispatch-arguments nil)
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(ns-function-modifier (quote hyper))
 '(package-selected-packages
   (quote
    (magit-gitflow lsp-sourcekit yasnippet-classic-snippets kotlin-mode visual-fill-column helm-projectile projectile company-restclient magit magit-popup matlab-mode vlf web-server magit helm exec-path-from-shell yasnippet use-package swift-mode company-sourcekit calmer-forest-theme)))
 '(projectile-sort-order (quote recently-active))
 '(projectile-use-git-grep t)
 '(show-paren-mode t)
 '(tab-width 4)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-highlight-prompt ((t nil))))
(put 'dired-find-alternate-file 'disabled nil)
