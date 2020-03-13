(defun scroll-down-keep-cursor ()
   (interactive)
   (scroll-down 1))

(defun scroll-up-keep-cursor ()
   (interactive)
   (scroll-up 1))


(defun select-current-word ()
  "Select the word under cursor.
“word” here is considered any alphanumeric sequence with “_” or “-”."
  (interactive)
  (let (pt)
	(skip-chars-backward "-_A-Za-z0-9")
	(setq pt (point))
	(skip-chars-forward "-_A-Za-z0-9")
	(set-mark pt)
	))

(defun delete-enclosed-text ()
  "Delete texts between any pair of delimiters."
  (interactive)
  (save-excursion
    (let (p1 p2)
      (skip-chars-backward "^(<[“") (setq p1 (point))
      (skip-chars-forward "^)>]”") (setq p2 (point))
      (delete-region p1 p2))))

(defun delete-current-file ()
  "Delete the file associated with the current buffer. Delete the
current buffer too."
  (interactive)
  (let (currentFile)
    (setq currentFile (buffer-file-name))
    (when (yes-or-no-p (concat "Delete file: " currentFile))
      (kill-buffer (current-buffer))
      (delete-file currentFile)
      (message (concat "Deleted file: " currentFile)))))

(defun insert-date ()
  "Insert date at point."
  (interactive)
  ;(insert (format-time-string "%a %b %e, %Y %l:%M %p")))
  (insert (format-time-string "%m/%d/%Y")))

;; Handy tea timer
(defun tea-is-done ()
  (switch-to-buffer "*Tea Timer*")
  (insert "Tea is ready")
)

(defun tea-timer ()
  "Display 'Tea is ready' in buffer '*Tea Timer*' after 2 minutes have elapsed."
  (interactive)
  (run-at-time "2 min" nil 'tea-is-done)
)

;; From www.emacswiki.org/cgi-bin/wiki/IncrementNumber
;; use built in 1+ instead
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun my-choose-header-mode ()
  (interactive)
  (if (string-equal (substring (buffer-file-name) -2) ".h")
      (progn
        ;; OK, we got a .h file, if a .m file exists we'll assume it's
        ; an objective c file. Otherwise, we'll look for a .cpp file.
        (let ((dot-m-file (concat (substring (buffer-file-name) 0 -1) "m"))
              (dot-cpp-file (concat (substring (buffer-file-name) 0 -1) "cpp")))
          (if (file-exists-p dot-m-file)
              (progn
                (objc-mode)
                )
            (if (file-exists-p dot-cpp-file)
                (c++-mode)
              )
            )
          )
        )
    )
  )

(add-hook 'find-file-hook 'my-choose-header-mode)

;;;
;; Mode hooks

;; Swift
(add-hook 'swift-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-sourcekit)))

(provide 'my-utils)
