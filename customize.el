(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-remote-files t)
 '(blink-cursor-mode nil)
 '(c-default-style
   '((objc-mode . "cc-mode")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "cc-mode")))
 '(column-number-mode t)
 '(custom-safe-themes
   '("143d897548e5a7efb5cf92c35bd39fe7c90cbd28f9236225ad3e80e1b79cef8a" default))
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(gc-cons-threshold 100000000)
 '(helm-buffer-max-length nil)
 '(lsp-after-apply-edits-hook '((lambda (op) (move-end-of-line nil))))
 '(lsp-clients-clangd-args
   '("--header-insertion-decorators=0" "--header-insertion=never"))
 '(lsp-enable-file-watchers t)
 '(lsp-java-vmargs
   '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx4G" "-Xms100m"))
 '(magit-diff-refine-hunk t)
 '(package-selected-packages
   '(string-inflection csv-mode restclient-jq restclient-helm restclient lsp-java lsp-sourcekit lsp-mode geiser-mit yasnippet-snippets yasnippet-classic-snippets smartparens yasnippet exec-path-from-shell company helm-projectile projectile swift-mode magit helm use-package diminish calmer-forest-theme))
 '(projectile-enable-caching nil)
 '(reb-re-syntax 'string)
 '(scroll-conservatively 100)
 '(scroll-preserve-screen-position nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(transient-default-level 7))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray12" :foreground "green" :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "nil" :family "PT Mono")))))
