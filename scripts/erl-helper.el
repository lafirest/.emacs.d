(defun erl/make-on-container ()
  (interactive)
  (with-current-container
   (send (concat "cd " home))
   (send "make")))

(defun erl/make-run-on-container ()
  (interactive)
  (with-current-container
   (send (concat "cd " home))
   (send "make run")))

(defun erl/close-emqx-on-container ()
  (interactive)
  (with-current-container
   (send "emqx_machine:brutal_shutdown().")))

(defun erl/shelll-on-container ()
  (interactive)
  (with-current-container
   (send "rebar shell")))

(defun erl/halt-on-container ()
  (interactive)
  (with-current-container
   (send "halt().")))

(defun on-erlang-mode ()
  (eglot-ensure)
  (let ((str (buffer-file-name)))
    (when (string-match "/docker:firest@\\(.*\\):/home/firest/\\(.*?\\)/.*" str)
      (setq-local benko (match-string 1 str)
                  namo (match-string 2 str)))
    (local-set-key (kbd "C-C l m") 'erl/make-on-container)
    (local-set-key (kbd "C-C l r") 'erl/make-run-on-container)
    (local-set-key (kbd "C-C l c") 'erl/stop-emqx-on-container)
    (local-set-key (kbd "C-C l s") 'erl/shelll-on-container)
    (local-set-key (kbd "C-C l h") 'erl/halt-on-container)))

(add-hook 'erlang-mode-hook 'on-erlang-mode)

(provide 'erl-helper)
