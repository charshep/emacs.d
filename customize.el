(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-remote-files t)
 '(blink-cursor-mode nil)
 '(c-default-style
   '((objc-mode . "cc-mode") (java-mode . "java") (awk-mode . "awk")
     (other . "cc-mode")))
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(custom-safe-themes
   '("143d897548e5a7efb5cf92c35bd39fe7c90cbd28f9236225ad3e80e1b79cef8a"
     default))
 '(debug-on-quit nil)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(gc-cons-threshold 100000000)
 '(global-auto-revert-mode t)
 '(helm-buffer-max-length nil)
 '(lsp-after-apply-edits-hook '((lambda (op) (move-end-of-line nil))))
 '(lsp-clients-clangd-args
   '("--header-insertion-decorators=0" "--header-insertion=never"))
 '(lsp-enable-file-watchers nil)
 '(lsp-java-format-on-type-enabled t)
 '(lsp-java-import-maven-enabled nil)
 '(lsp-java-java-path
   "/Library/Java/JavaVirtualMachines/amazon-corretto-17.jdk/Contents/Home/bin/java")
 '(lsp-java-show-build-status-on-start-enabled t)
 '(lsp-java-vmargs
   '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4"
     "-XX:AdaptiveSizePolicyWeight=90"
     "-Dsun.zip.disableMemoryMapping=true" "-Xmx8G" "-Xms4G"
     "-javaagent:/Users/CharlieSheppard/.m2/repository/org/projectlombok/lombok/1.18.22/lombok-1.18.22.jar"))
 '(lsp-log-io nil)
 '(magit-diff-refine-hunk t)
 '(mode-line-compact 'long)
 '(package-archive-priorities '(("melpa" . 1)))
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("elpa" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(async avy calmer-forest-theme company compat csv-mode dap-mode dash
           diminish exec-path-from-shell expand-region f geiser
           geiser-mit helm helm-core helm-projectile ht hydra jq-mode
           lsp-docker lsp-java lsp-mode lsp-sourcekit lsp-treemacs
           magit magit-section markdown-mode popup posframe projectile
           request restclient restclient-helm restclient-jq
           smartparens string-inflection swift-mode transient treemacs
           use-package with-editor yasnippet
           yasnippet-classic-snippets yasnippet-snippets))
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
 '(default ((t (:family "PT Mono" :foundry "nil" :slant normal :weight regular :height 180 :width normal)))))
