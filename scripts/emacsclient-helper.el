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

(provide 'emacsclient-helper)
