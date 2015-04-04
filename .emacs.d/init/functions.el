(defun system-is-linux()
  (string-equal system-type "gnu/linux"))

(defun system-is-windows()
  (string-equal system-type "windows-nt"))

(defun copy-buffer-file-name-as-kill (choice)
  "Copyies the buffer {name/mode}, file {name/full path/directory} to the kill-ring."
  (interactive "cCopy (b) buffer name, (m) buffer major mode, (f) full buffer-file path, (d) buffer-file directory, (n) buffer-file basename")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          ((eq choice ?b)
           (setq new-kill-string (buffer-name)))
          ((eq choice ?m)
           (setq new-kill-string (format "%s" major-mode)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

(provide-me)
;;; functions.el ends here
