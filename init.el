;; 添加路径
(defmacro add-to-load-path (path)
  `(let ((default-directory ,path))
     (normal-top-level-add-subdirs-to-load-path)))


(setq max-lisp-eval-depth 10000)
(add-to-load-path "~/.emacs.d/mia/")

(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("org" . "http://elpa.emacs-china.org/org/")))


(package-initialize)
(package-refresh-contents)

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;(require 'dashboard)
;(dashboard-setup-startup-hook)
;; Or if you use use-package
;(use-package dashboard
;  :config
;  (dashboard-setup-startup-hook))

(use-package ivy
  :hook (after-init . ivy-mode))

(use-package company
  :hook (after-init . global-company-mode))

(use-package org
  :mode ("\\.org\\'" . org-mode))

(use-package org-bullets
    :hook (org-mode . org-bullets-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package lsp-mode
  :hook (prog-mode . lsp-mode)
  :commands lsp)

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  :hook (lsp-mode . aggressive-indent-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-doc-enable t))

(use-package lsp-ivy
  :hook (lsp-mode . lsp-ivy-workspace-symbol) 
  :config (lsp-treemacs-sync-mode 1))

(use-package lsp-treemacs
  :hook (lsp-mode . lsp-treemacs-errors-list))

(use-package flycheck)

(use-package projectile
  :init (projectile-mode +1)
  :bind
  ("C-c p" . projectile-command-map)
  :config (setq projectile-completion-system 'ivy))

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package visual-regexp
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)))

(use-package goto-line-preview)

(use-package avy
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("C-\"" . avy-goto-char-timer)
   ("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)
   ("M-g h" . avy-org-goto-heading-timer)))


(use-package treemacs
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-expand-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-litter-directories            '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)
 
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t))
  ;:bind
  ;("C-x C-left" . centaur-tabs-backward)
  ;("C-x C-right" . centaur-tabs-forward))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (csharp-mode lsp-ui lsp-mode helm-lsp yasnippet-snippets xr visual-regexp treemacs-projectile treemacs-magit smart-mode-line sly orgtbl-show-header org-roam org-bullets move-text magit-todos lsp-treemacs lsp-ivy goto-line-preview focus dashboard company common-lisp-snippets centaur-tabs avy-flycheck auto-package-update all-the-icons aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
