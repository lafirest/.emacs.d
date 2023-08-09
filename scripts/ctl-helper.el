(require 'tramp)
(require 'tramp-container)

(defconst ctl-cmd-port "59998")

(eval-after-load
    'tramp
  (lambda ()
    ;; speed up
    (setq vc-ignore-dir-regexp
          (format "\\(%s\\)\\|\\(%s\\)"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp))
    ;; speed up the tramp-find-foreign-file-name-handler
    (setq
     tramp-foreign-file-name-handler-alist
     (list (assoc 'identity tramp-foreign-file-name-handler-alist)))

    (setq remote-file-name-inhibit-cache nil) ;; if need disable

    ;; use rsync to send file
    (setq tramp-connection-properties
          (append
           tramp-connection-properties
           (mapcar (lambda (kv)
                     (cons (regexp-quote "/docker:") kv))
                   '(("copy-program" "docker-tramp-cp")))))
                    ;; ("keep-date" t)
                    ;; ("keep-tmpfile" t)
                    ;;  ("copy-recursive" t)))))

    ;; avoid to hang
    (setenv "SHELL" "/bin/bash")

    (setq tramp-shell-prompt-pattern
          "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

    ;; bin path
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

    ;; using erlfmt with after-save-hook will slow down even hanging the tramp
    ;; Debug
    ;;(setq tramp-debug-buffer t)
    ;;(setq tramp-debug-to-file t)
    (setq tramp-verbose 0)
    ))

;; avoid tramp to hang
(defun ctl/vc-off-if-remote ()
  (if (file-remote-p (buffer-file-name))
      (setq-local vc-handled-backends nil)))

(defun open-container ()
  (interactive)
  (let* ((benko (completing-read
                 "Elektu unu laborbenkon: "
                 (split-string
                  (shell-command-to-string "docker container ls --format '{{.Names}}'")
                  "\n")))
         (projekto (completing-read
                    "Elektu unu projekto"
                    (split-string
                     (shell-command-to-string (format "docker exec %s ls ~" benko))
                     "\n"))))
    (find-file
     (format "/docker:firest@%s:/home/firest/%s" benko projekto))))

(defmacro with-container-cmd-channel (container &rest body)
  `(let* ((channel-name (cl-gentemp))
          (channel
           (open-network-stream
            (symbol-name channel-name)
            nil
            ,container
            ctl-cmd-port)))
     (cl-flet ((send (lambda (cmd)
                       (process-send-string channel
                                            (format "%s\n" cmd)))))
       ,@body)
     (delete-process channel)))

(defmacro with-current-container (&rest body)
  `(let ((buf (current-buffer)))
     (when (and
            (buffer-local-boundp 'benko buf)
            (buffer-local-boundp 'namo buf))
       (let ((home (file-name-concat "/home/firest" namo)))
         (with-container-cmd-channel benko
                                     ,@body)))))

(add-hook 'find-file-hook 'ctl/vc-off-if-remote)

(provide 'ctl-helper)
