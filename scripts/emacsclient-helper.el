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
                 (setq current-frame
                       (make-frame `((name . ,name)
                                     (height . 80)
                                     (width . 120)
                                     (user-position . t)
                                     (top . 0.5)
                                     (left . ,left))))
                 (select-frame-set-input-focus current-frame)
                 current-frame))
             (bind-frame (name)
               "create a new frame with a new vterm"
               (with-selected-frame (create-frame name)
                 (multi-vterm name)))
             (show-frame (frame)
               "make the frame to be visible and be the focus"
               (make-frame-visible frame)
               (select-frame-set-input-focus frame))
             (hide-frame (frame)
               "hide the frame and focus on the next if it exists"
               (make-frame-invisible frame)
               (when-let ((next (car (visible-frame-list))))
                 (select-frame-set-input-focus next)))
             (may-hide-frame (frame)
               "hide the frame if it is focused, otherwise refocus it"
               (if (eq frame (selected-frame))
                   (hide-frame frame)
                 (select-frame-set-input-focus frame)))
             (toggle-frame (buffer)
               "if frame is exists, toggle the frame, or create a new frame and switch to the vterm buffer "
               (let* ((name (buffer-name buffer))
                      (frame (emacsclient/find-frame-by-name name)))
                 (if (frame-live-p frame)
                     (if (frame-visible-p frame)
                         (may-hide-frame frame)
                       (show-frame frame))
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
