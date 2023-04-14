(cl-defmacro emacsclient/with-frame-maximized (&body body)
  `(progn
     (toggle-frame-maximized)
     ,@body
     (select-frame-set-input-focus (selected-frame))))

(defun emacsclient/open-dashboard ()
  (emacsclient/with-frame-maximized
   (dashboard-open)))

(defun emacsclient/open-with-file (file)
  (emacsclient/with-frame-maximized
   (find-file file)))

(defun emacsclient/vterm ()
  (vterm)
  (select-frame-set-input-focus (selected-frame))
  (frame-helper/move-frame-to-center))

(defun emacsclient/emacs-everywhere-set-frame-position ()
  "Set the size and position of the emacs-everywhere frame."
  (cl-destructuring-bind (x . y) (mouse-pixel-position)
    (set-frame-position (selected-frame)
                        x
                        y)
    (frame-helper/move-frame-to-center)))

(defun emacsclient/emacs-everywhere-init ()
  (emacsclient/emacs-everywhere-set-frame-position)
  (setq mode-line-format nil)
  ;; make sure the mode line is hidden
  (add-variable-watcher mode-line-format
                        (lambda (sym new op where)
                          (setq mode-line-format nil))))

(defun emacsclient/vterm-toggle (vterm-name)
  (cl-flet* ((create-frame (name)
               "create a new frame"
               (let ((left 0.5)
                     (current-frame (selected-frame)))
                 (when current-frame
                   (cl-destructuring-bind (x . y) (frame-position)
                     (setq left (if (< x 0) 0.5 -0.5))))
                 (make-frame `((name . ,name)
                               (height . 80)
                               (width . 120)
                               (user-position . t)
                               (top . 0.5)
                               (left . ,left)))))
             (bind-frame (name)
               "create a new frame with a new vterm"
               (with-selected-frame (create-frame name)
                 (multi-vterm name)))
             (toggle-frame (buffer)
               "if frame is exists, toggle the frame, or create a new frame and switch to the vterm buffer "
               (let* ((name (buffer-name buffer))
                      (frame (emacsclient/find-frame-by-name name)))
                 (if (frame-live-p frame)
                     (if (frame-visible-p frame)
                         (make-frame-invisible frame)
                       (make-frame-visible frame))
                   (progn
                     (setq frame (create-frame name))
                     (select-frame frame)
                     (switch-to-buffer buffer))))))
    (let ((buffer (get-buffer vterm-name)))
      (if (buffer-live-p buffer)
          (toggle-frame buffer)
        (bind-frame vterm-name)))))

(cl-defun emacsclient/find-frame-by-name (name)
  (dolist (f (frame-list))
    (when (equal name (alist-get 'name (frame-parameters f)))
      (cl-return-from emacsclient/find-frame-by-name f)))
  nil)

(provide 'emacsclient-helper)
