;; add path to load path
;;(defmacro add-to-load-path (path)
;;  `(let ((default-directory ,path))
;;     (normal-top-level-add-subdirs-to-load-path)))

(setq max-lisp-eval-depth 10000)

(require 'package)

(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(setq quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      quelpa-self-upgrade-p nil)

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(when (eq system-type 'darwin)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(require 'dashboard)
(dashboard-setup-startup-hook)

(use-package ivy
  :hook (after-init . ivy-mode))

(use-package company
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-backends '((:separate company-capf
                                      company-dabbrev-code
                                      company-yasnippet))))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-use-sub-superscripts '{}
        org-export-with-sub-superscripts '{}
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-image-actual-width nil
        org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))

        org-log-into-drawer "LOGBOOK"
        org-clock-in-switch-to-state "ACTIVE"
        org-clock-out-when-done '("DONE" "CANCELED")

        org-todo-keywords '((sequence "TODO(t!)" "ACTIVE(a!)" "Suspended(s@)"
                                      "BLOCKED(b@)" "DONE(d!)" "CANCELED(c@)"))

        org-todo-keyword-faces '(("TODO" . "red") ("ACTIVE" . "yellow") ("Suspended" . "peru")
                                 ("BLOCKED" . "brown") ("DONE" . "green") ("CANCELED" . "cyan"))


        org-latex-listings-langs (quote ((emacs-lisp "Lisp") (lisp "Lisp") (clojure "Lisp") (c "C") (cc "C++") (fortran "fortran") (perl "Perl") (cperl "Perl") (python "Python") (ruby "Ruby") (html "HTML") (xml "XML") (tex "TeX") (latex "[LaTeX]TeX") (shell-script "bash") (gnuplot "Gnuplot") (ocaml "Caml") (caml "Caml") (sql "SQL") (sqlite "sql") (R-mode "R") (csharp "csharp")))

        org-latex-minted-options
        '(("frame" "lines") ("linenos=true"))

        org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode %f"
                                "xelatex -shell-escape  -interaction nonstopmode %f")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-hide-stars 'nil)
  ;; TODO org-modern only support "TODO" and "DONE" keywords, this should be fork and fixup
  (setq org-modern-todo 'nil))

(use-package org-roam
  :ensure nil
  :quelpa (org-roam :fetcher github-ssh
                    :repo "lafirest/org-roam"
                    :files ("extensions/*"))
  :after org
  :bind
  ("C-c s i" . org-id-get-create)
  ("C-c s f" . org-roam-node-find)
  ("C-c s n" . org-roam-node-insert)
  ("C-c s a" . org-roam-alias-add)
  ("C-c s A" . org-roam-alias-remove)
  ("C-c s t" . org-roam-tag-add)
  ("C-c s T" . org-roam-tag-remove)
  ("C-c s r" . org-roam-ref-add)
  ("C-c s R" . org-roam-ref-remove)
  ("C-c s l" . insert-node-and-tag)
  :hook (org-mode . check-is-in-roam-dir)
  :config
  (setq org-roam-directory (file-truename "~/sciobazo"))
  (setq org-roam-db-location
        (expand-file-name "org-roam.db" org-roam-directory))
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-db-update-on-save t)

  (defun check-is-in-roam-dir ()
    (when (and buffer-file-name
               (string-match-p org-roam-directory buffer-file-name))
      ;;(org-roam-db-autosync-mode)
      (add-hook 'after-save-hook 'org-roam-db-sync)
      (org-roam-timestamps-mode)))

  (setq toggle-auto-insert-tag nil)
  (add-hook 'org-roam-post-node-insert-hook
            (lambda (id description)
              (when toggle-auto-insert-tag
                (directly-tag-add
                 (list (replace-regexp-in-string
                        " "
                        "_"
                        (downcase description))))
                (setq toggle-auto-insert-tag nil))))

  (defun insert-node-and-tag ()
    "insert node link and convert node name to tag to add"
    (interactive)
    (setq toggle-auto-insert-tag t)
    (org-roam-node-insert))

  (defun directly-tag-add (tags)
    "copy from org-mode, add tags to node"
    (let ((node (org-roam-node-at-point 'assert)))
      (save-excursion
        (goto-char (org-roam-node-point node))
        (if (= (org-outline-level) 0)
            (let ((current-tags (split-string
                                 (or
                                  (cadr (assoc
                                         "FILETAGS"
                                         (org-collect-keywords '("filetags"))))
                                  "")
                                 ":" 'omit-nulls)))
              (org-roam-set-keyword
               "filetags"
               (org-make-tag-string (seq-uniq (append tags current-tags)))))
          (org-set-tags (seq-uniq (append tags (org-get-tags)))))))))

(use-package org-roam-timestamps
  :config
  (setq org-roam-timestamps-parent-file t
        org-roam-timestamps-remember-timestamps t
        org-roam-timestamps-minimum-gap 60))

(use-package org-roam-ui
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox
  :config
  (setq org-hugo-base-dir "~/labori/lafirest.github.io/"))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package lsp-mode
  :config
  (setq lsp-erlang-server-path "~/.local/bin/erlang_ls")
  (setq lsp-lens-place-position 'above-line)
  (setq lsp-completion-provider :none)

  ;;(setq lsp-log-io t)
  :hook ((prog-mode . lsp-mode)
         (lsp-mode . lsp-lens-mode)
         (erlang-mode . lsp)
         (haskell-mode . lsp)
         (csharp-mode . lsp)
         (haskell-literate-mode-hook . lsp)))

(use-package lsp-haskell
  :config
  (setq lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-wrapper"))

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
  (setq lsp-ui-doc-enable t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;;(use-package lsp-ivy
;;  :hook (lsp-mode . lsp-ivy-workspace-symbol))

(use-package lsp-treemacs
                                        ;:config (lsp-treemacs-sync-mode 1)
  :bind
  ("C-c l e" . lsp-treemacs-errors-list)
  ("C-c l s" . lsp-treemacs-symbols)
  ("C-c l r" . lsp-treemacs-references)
  ("C-c l i" . lsp-treemacs-implementations)
  ("C-c l h" . lsp-treemacs-type-hierarchy))

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

(use-package goto-line-preview
  :bind
  ("M-g g" . goto-line-preview))

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
  (centaur-tabs-headline-match)
  (setq centaur-tabs-style "box")
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'over)
  (centaur-tabs-mode t)
  :bind
  ("C-x t p" . centaur-tabs-backward)
  ("C-x t n" . centaur-tabs-forward))

(use-package all-the-icons)

(use-package which-key
  :config
  (progn
    (which-key-mode)
    (which-key-setup-side-window-bottom)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package plantuml-mode
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :config
  (setq plantuml-jar-path  "~/.local/lib/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-output-type "txt")
  (setq org-plantuml-jar-path (expand-file-name "~/.local/lib/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t)
                                 (csharp . t))))

(use-package org-static-blog
  :config
  (setq org-static-blog-publish-title "Cogito, ergo sum")
  (setq org-static-blog-publish-url "https://lafirest.github.io/")
  (setq org-static-blog-publish-directory "~/labori/lafirest.github.io/docs")
  (setq org-static-blog-posts-directory "~/labori/lafirest.github.io/posts")
  (setq org-static-blog-drafts-directory "~/labori/lafirest.github.io/drafts")
  (setq org-static-blog-enable-tags t)
  (setq org-export-with-toc t)
  (setq org-export-with-section-numbers t)
  (setq org-static-blog-use-preview t)

  ;; This header is inserted into the <head> section of every page:
  ;;   (you will need to create the style sheet at
  ;;    ~/projects/blog/static/style.css
  ;;    and the favicon at
  ;;    ~/projects/blog/static/favicon.ico)
  (setq org-static-blog-page-header
        "<meta name=\"author\" content=\"firest\">
<meta name=\"referrer\" content=\"no-referrer\">
<link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">"))

(use-package telephone-line
  :config
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t)
  (telephone-line-mode 1))

(use-package switch-window
  :config
  (global-set-key (kbd "C-x o") 'switch-window)
  (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
  (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
  (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
  (global-set-key (kbd "C-x 0") 'switch-window-then-delete)

  (global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
  (global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
  (global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
  (global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)

  (global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
  (global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)

  (global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer))

(use-package beacon
  :config
  (setq beacon-color "yellow")
  (setq beacon-size 80)
  (setq beacon-blink-when-point-moves-vertically 10)
  (beacon-mode 1))

(use-package pangu-spacing
  :config
  (setq pangu-spacing-real-insert-separtor t)
  :hook (org-mode . pangu-spacing-mode))

(use-package undo-tree
  :config
  (setq undo-tree-visualizer-diff t))

(defun prog-face ()
  (face-remap-add-relative 'hl-line
                           :background "forest green")

  (face-remap-add-relative 'font-lock-function-name-face
                           :height 160
                           :underline t)

  (face-remap-add-relative 'lsp-face-highlight-textual
                           :background "dark cyan"))

(defun prog-hook ()
  (prog-face)
  (show-paren-mode))

(add-hook 'prog-mode-hook #'prog-hook)
(menu-bar-mode 0)
(tool-bar-mode 0)
(global-display-line-numbers-mode)
(global-hl-line-mode 1)
(global-undo-tree-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq create-lockfiles nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/autosaves/\\1" t)))

(load-theme 'zenburn t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   '("b77a00d5be78f21e46c80ce450e5821bdc4368abf4ffe2b77c5a66de1b648f10" "79586dc4eb374231af28bbc36ba0880ed8e270249b07f814b0e6555bdcb71fab" "33ea268218b70aa106ba51a85fe976bfae9cf6931b18ceaf57159c558bbcd1e6" "a37d20710ab581792b7c9f8a075fcbb775d4ffa6c8bce9137c84951b1b453016" "c8e076f0e2df414c02fdb46b09b735628e73c73f72f9d78392edf99de7d86977" "d2e0c53dbc47b35815315fae5f352afd2c56fa8e69752090990563200daae434" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" "549ccbd11c125a4e671a1e8d3609063a91228e918ffb269e57bd2cd2c0a6f1c6" default))
 '(fci-rule-color "#383838")
 '(ispell-dictionary nil)
 '(line-number-mode nil)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-agenda-files nil)
 '(package-selected-packages
   '(ox-hugo use-package-always-ensure use-package-ensure quelpa-use-package quelpa org-roam-timestamps org-roam-ui pangu-spacing magit-git org-modern htmlize bnf-mode ox-jira org-jira org-superstar dedicated fzf ztree omnisharp exec-path-from-shell lsp-haskell anti-zenburn-theme hc-zenburn-theme sly-quicklisp beacon telephone-line vterm multiple-cursors undo-tree org-preview-html yaml-mode plantuml-mode rainbow-delimiters which-key solo-jazz-theme darktooth-theme ample-theme zenburn-theme dracula-theme erlang xwwp-follow-link-ivy flycheck csharp-mode lsp-ui helm-lsp yasnippet-snippets xr visual-regexp treemacs-projectile treemacs-magit smart-mode-line sly orgtbl-show-header org-roam org-bullets move-text magit-todos lsp-treemacs lsp-ivy goto-line-preview focus dashboard company common-lisp-snippets centaur-tabs avy-flycheck auto-package-update all-the-icons aggressive-indent))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight semi-bold :height 140 :width normal :foundry "nil" :family "JuliaMono")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
