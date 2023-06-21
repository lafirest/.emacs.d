(add-to-list 'load-path "~/.emacs.d/scripts")
(setq max-lisp-eval-depth 10000)

(require 'straight-helper)
(require 'frame-helper)
(require 'emacsclient-helper)

(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize)
  ;; I don't know why this env can't be load auto
  (setenv "DOTNET_ROOT" "/opt/dotnet-sdk-bin-6.0"))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook))

(use-package ivy
  :straight t
  :hook (after-init . ivy-mode))

(use-package company
  :straight t
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-backends '((:separate company-capf
                                      company-dabbrev-code
                                      company-yasnippet))))

(use-package org
  :straight (:type built-in) ;; compile upstream repo sometime will fails
  :mode ("\\.org\\'" . org-mode)
  :bind
  ("C-c c" . (lambda () (interactive) (org-capture nil)))
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


        org-latex-listings-langs (quote ((emacs-lisp "Lisp")
                                         (lisp "Lisp")
                                         (clojure "Lisp")
                                         (c "C")
                                         (cc "C++")
                                         (fortran "fortran")
                                         (perl "Perl")
                                         (cperl "Perl")
                                         (python "Python")
                                         (ruby "Ruby")
                                         (html "HTML")
                                         (xml "XML")
                                         (tex "TeX")
                                         (latex "[LaTeX]TeX")
                                         (shell-script "bash")
                                         (gnuplot "Gnuplot") (ocaml "Caml") (caml "Caml") (sql "SQL") (sqlite "sql") (R-mode "R") (csharp "csharp")))

        org-latex-minted-options
        '(("frame" "lines") ("linenos=true"))

        org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode %f"
                                "xelatex -shell-escape  -interaction nonstopmode %f")
        org-capture-templates
        '(("t" "Todo" entry (file+headline "~/.emacs.d/org/todo.org" "Tasks")
           "* TODO %?\n %U\n")
          ("d" "Todo for Today" entry (file+datetree "~/.emacs.d/org/todo_for_day.org")
           "* TODO %?\n %U\n")
          ("n" "Thought or Note" entry (file+headline "~/.emacs.d/org/thought.org" "Thought")
           "* Thought %?\n %U\n"))))

(use-package org-modern
  :straight t
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-hide-stars " "))

(use-package ox-hugo
  :straight t
  :after ox
  :config
  (setq org-hugo-base-dir "~/labori/lafirest.github.io/"))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t)

;; (use-package lsp-mode
;;   :straight t
;;   :config
;;   (setq lsp-erlang-server-path "~/.local/bin/erlang_ls")
;;   (setq lsp-lens-place-position 'above-line)
;;   (setq lsp-completion-provider :none)
;;   (setq lsp-log-io t))
;;   :hook ((prog-mode . lsp-mode)
;;          (lsp-mode . lsp-lens-mode)
;;          (erlang-mode . lsp)
;;          (haskell-mode . lsp)
;;          (csharp-mode . lsp) csharp-ls
;;          (haskell-literate-mode-hook . lsp)
;;          need install ccls (not emacs-ccls ?)
;;          ((c-mode c++-mode) . (lambda () (require 'ccls) (lsp)))))

;; (use-package lsp-haskell
;;   :straight t
;;   :config
;;   (setq lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-wrapper"))

;; (use-package lsp-ui
;;   :straight t
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config
;;   (setq lsp-ui-doc-enable t
;;         lsp-ui-sideline-show-diagnostics t
;;         lsp-ui-sideline-show-hover t
;;         lsp-ui-sideline-show-code-actions t
;;         lsp-ui-doc-show-with-mouse nil
;;         lsp-ui-doc-show-with-cursor t
;;         lsp-ui-doc-doc-header t
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-doc-max-width 60
;;         lsp-ui-doc-max-height 60
;;         )

;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; (use-package lsp-treemacs
;;   :straight t
;;   ;;:config (lsp-treemacs-sync-mode 1)
;;   :bind
;;   ("C-c l e" . lsp-treemacs-errors-list)
;;   ("C-c l s" . lsp-treemacs-symbols)
;;   ("C-c l r" . lsp-treemacs-references)
;;   ("C-c l i" . lsp-treemacs-implementations)
;;   ("C-c l h" . lsp-treemacs-type-hierarchy))

(use-package flycheck
  :straight t)

(use-package projectile
  :straight t
  :init (projectile-mode +1)
  :bind
  ("C-c p" . projectile-command-map)
  :config (setq projectile-completion-system 'ivy))

(use-package move-text
  :straight t
  :config
  (move-text-default-bindings))

(use-package visual-regexp
  :straight t
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)))

(use-package goto-line-preview
  :straight t
  :bind
  ("M-g g" . goto-line-preview))

(use-package avy
  :straight t
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("C-\"" . avy-goto-char-timer)
   ("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)
   ("M-g h" . avy-org-goto-heading-timer)))

(use-package treemacs
  :straight t
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
  :straight t
  :after (treemacs projectile))

(use-package treemacs-magit
  :straight t
  :after (treemacs magit))

(use-package all-the-icons
  :straight t)

(use-package which-key
  :straight t
  :config
  (progn
    (which-key-mode)
    (which-key-setup-side-window-bottom)))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package emacs-everywhere
  :straight t
  :hook
  (after-init . (lambda ()
                  (setq emacs-everywhere-init-hooks
                        (cons 'emacsclient/emacs-everywhere-init
                              (remove
                               'emacs-everywhere-set-frame-position
                               emacs-everywhere-init-hooks))))))

(use-package plantuml-mode
  :straight t
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :config
  (setq plantuml-jar-path  "~/.local/lib/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-output-type "txt")
  (setq org-plantuml-jar-path (expand-file-name "~/.local/lib/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t)
                                 (csharp . t)
                                 (lisp . t))))

(use-package org-static-blog
  :straight t
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

(use-package switch-window
  :straight t
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
  :straight t
  :config
  (setq beacon-color "yellow")
  (setq beacon-size 80)
  (setq beacon-blink-when-point-moves-vertically 10)
  (beacon-mode 1))

(use-package pangu-spacing
  :straight t
  :config
  (setq pangu-spacing-real-insert-separtor t)
  :hook (org-mode . pangu-spacing-mode))

(use-package undo-tree
  :straight t
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package ligature
  :straight t
  :load-path "/home/firest/github/ligature.el"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; (use-package rime
;;   :straight t
;;   :config
;;   (setq rime-posframe-properties
;;         (list :background-color "#333333"
;;               :foreground-color "#dcdccc"
;;               :internal-border-width 10))

;;   (setq  rime-show-candidate 'posframe)

;;   (setq rime-disable-predicates
;;         '(rime-predicate-after-alphabet-char-p
;;           rime-predicate-prog-in-code-p
;;           rime-predicate-in-code-string-p
;;           rime-predicate-space-after-cc-p))

;;   (define-key rime-active-mode-map (kbd "M-j") 'rime-inline-ascii)

;;   :custom
;;   (default-input-method "rime"))

(require 'tramp)
(require 'tramp-container)

(eval-after-load
    'tramp
    (lambda ()
      ;; speed up
      (setq-default tramp-default-method "scp")
      (setq remote-file-name-inhibit-cache nil) ;; if need disable

      ;; avoid to hang
      (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

      (setq tramp-shell-prompt-pattern
            "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
      (setq vc-ignore-dir-regexp
            (format "\\(%s\\)\\|\\(%s\\)"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))

      ;; using erlfmt with after-save-hook will slow down even hanging the tramp
      ;; Debug
      ;;(setq tramp-debug-buffer t)
      ;;(setq tramp-verbose 10)
      ))

;; avoid tramp to hang
(defun vc-off-if-remote ()
  (if (file-remote-p (buffer-file-name))
      (setq-local vc-handled-backends nil)))

(defun open-container (benko namo)
  (interactive "sBenko:\nsNamo:")
  (find-file
   (read-file-name
    "Open Container file: "
    (format "/docker:firest@%s:/home/firest/%s" benko namo))))

(use-package eglot
  :straight t
  :config
  (add-to-list 'eglot-server-programs
               '(erlang-mode . ("erlang_ls"))))

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
  (show-paren-mode)
  (set-fill-column-indicator))


(defun set-fill-column-indicator ()
  (when (boundp 'display-fill-column-indicator)
    (display-fill-column-indicator-mode)
    (setq-default indicate-buffer-boundaries 'left)
    (setq-default display-fill-column-indicator-column 100)
    (setq-default display-fill-column-indicator-character ?\u254e)
    (face-remap-add-relative 'fill-column-indicator
                             :foreground "chocolate1")))

;; hooks
(add-hook 'prog-mode-hook 'prog-hook)
(add-hook 'find-file-hook 'vc-off-if-remote)
(add-hook 'erlang-mode-hook 'eglot-ensure)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
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

;; Set default font
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 140
                    :weight 'normal
                    :width 'normal)

(use-package zenburn-theme
  :straight t
  :config
  (load-theme 'zenburn t))
