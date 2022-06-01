;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Nick Silverman"
      user-mail-address "nick.silverman11@gmail.com")

;; Viz settings =================================================
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code Retina" :size 14))
(setq doom-variable-pitch-font (font-spec :family "Cantarell" :size 14 :weight 'regular))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-wilmersdorf)
;; (setq doom-theme 'doom-moonlight)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-xcode)
;; (setq doom-theme 'doom-solarized-dark)
;; (setq doom-theme 'doom-badger)

;; For Spacemacs theme; unecessary for other themes
(setq doom-theme 'spacemacs-dark)
(custom-set-faces!
  '(doom-dashboard-banner :inherit default)
  '(doom-dashboard-loaded :inherit default))

(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
  (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
(add-hook 'org-mode-hook #'my/org-mode-hook)

;; Beacon flashes cursor line
(use-package beacon
  :custom
  (beacon-push-mark 10)
  (beacon-color "#cc342b")
  (beacon-blink-delay 0.3)
  (beacon-blink-duration 0.3)
  :config
  (beacon-mode)
  (global-hl-line-mode 1))

;; Modeline stuff (see https://github.com/seagle0128/doom-modeline)
(setq doom-modeline-height 25)
(setq doom-modeline-bar-width 3)
(setq doom-modeline-window-width-limit fill-column)
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-buffer-modification-icon t)
(setq doom-modeline-unicode-fallback nil)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-enable-word-count nil)
(setq doom-modeline-buffer-encoding t)
(setq doom-modeline-indent-info nil)
(setq doom-modeline-checker-simple-format t)
(setq doom-modeline-number-limit 99)
(setq doom-modeline-vcs-max-length 12)
(setq doom-modeline-workspace-name t)
(setq doom-modeline-lsp t)
(setq doom-modeline-modal-icon t)

;; General preferences =========================================
;; Word wrap
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
"Takes a multi-line paragraph and makes it into a single line of text."
(interactive (progn (barf-if-buffer-read-only) '(t)))
(let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
(fill-paragraph nil region)))

;;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; This is to use pdf-tools instead of doc-viewer
(use-package pdf-tools
  :config
  (pdf-tools-install)
  ;; This means that pdfs are fitted to width by default when you open them
  (setq-default pdf-view-display-size 'fit-width))

;; Terminal settings
;;; Vterm
(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "bash")
  (setq vterm-max-scrollback 10000))

;; Encryption =================================================
(require 'epa-file)
(epa-file-enable)

;; LSP-mode ====================================================
(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :custom (lsp-headerline-breadcrumb-enable t)
  :commands (lsp lsp-deferred))

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))  ; or lsp-deferred

;; ESS settings ================================================
(use-package ess-r-mode
  :bind
  (:map ess-r-mode-map
        ("_" . ess-insert-assign))
  (:map inferior-ess-r-mode-map
        ("_" . ess-insert-assign)))

;;; Org-mode settings ==========================================
(setq org-directory "~/Dropbox/org/projects/")
(setq org-todo-file "~/Dropbox/org/projects/actions.org")

;; Set TODO keywords
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE")))

;; Allow ordered lists to include A. B. C. etc.
(setq org-list-allow-alphabetical t)

;; Set archive file
(setq org-archive-location
      (concat org-directory
              "/archive/%s::datetree/"))

;; Set org agenda files
;; (setq org-agenda-files (list org-todo-file
;;                              "~/Dropbox/org/projects/someday.org"))
(setq org-agenda-files (list org-directory))

;; Include timestamp when closing a todo
(setq org-log-done 'time)

;; Create list of common tags
(setq org-tag-alist '(("@errand" . ?E)
                      ("@tumbleleaf" . ?T)
                      ("@burlington" . ?B)
                      ("@home" . ?H)
                      ("@work" . ?W)
                      ("@ocean" . ?O)
                      ("@river" . ?R)
                      ("@mountain" . ?M)
                      ("paddling" . ?p)
                      ("hunting" . ?h)
                      ("fishing" . ?f)
                      ("surfing" . ?s)
                      ("cycling" . ?c)
                      ("climbing" . ?m)
                      ("running" . ?r)
                      ("hiking" . ?i)))

;; Capture
(after! org
  (setq org-default-notes-file org-todo-file)
  (setq org-capture-templates
        '(("t" "TODO" entry
           (file+headline org-default-notes-file "Inbox")
           "* TODO [#B] %?\n %i\n %a"
           :kill-buffer t)

        ("n" "Note" entry
          (file+headline org-default-notes-file "Inbox")
          "* %?\n %i\n %a"
          :kill-buffer t))))

;; Org-wild-notifier
(use-package org-wild-notifier
  :defer t
  :init
  (org-wild-notifier-mode)
  :custom
  (org-wild-notifier-alert-time '(30 5)))

(use-package alert
  :defer t
  :commands (alert)
  :custom
  (alert-default-style 'libnotify))

;; Deft settings ==============================================
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Dropbox/org/projects/"))

;; This speeds up deft...but limits the amount of files you see
;; Overwrite `deft-current-files` for the `deft-buffer-setup` and
;; limit it to 50 entries
(defun anks-deft-limiting-fn (orig-fun &rest args)
  (let
      ((deft-current-files (-take 50 deft-current-files)))
    (apply orig-fun args)))

(advice-add 'deft-buffer-setup :around #'anks-deft-limiting-fn)

;; Org-journal settings =======================================
(setq org-journal-dir "~/Dropbox/org/journal/"
      org-journal-file-type 'monthly
      org-journal-file-format "%m%Y.org"
      org-journal-date-prefix "* "
      org-journal-date-format "%A, %B %d %Y"
      org-journal-file-header "#+TITLE: %B %Y Journal\n\n")

;; Org-roam settings ===========================================
;;; taken from jethro doom config:
;;; https://github.com/jethrokuan/dots/blob/master/.doom.d/config.el
(use-package! org-roam
  :after org
  :init
  (setq org-roam-directory "~/Dropbox/org/notes/")
  :config
  (org-roam-db-autosync-mode +1)
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .27 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .27 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode))

;; This changes the file name and template during note capture
(setq org-roam-capture-templates
      `(("d" "default" plain "%?"
         :target (file+head "${slug}.org"
                            "#+TITLE: ${title}\n")
         :unnarrowed t)
         ("r" "reference" plain "%?"
          :target (file+head "${citekey}.org"
                             ,(concat
                              "#+TITLE:  ${title}\n"
                              "#+AUTHOR: ${author-or-editor}\n"
                              "#+filetags: :Reference:\n\n"
                              "* ${title}\n"
                              "  :PROPERTIES:\n"
                              "  :Custom_ID: ${citekey}\n"
                              "  :URL: ${url}\n"
                              "  :NOTER_DOCUMENT: ${file}\n"
                              "  :NOTER_PAGE: \n"
                              "  :END:\n"))
          :unnarrowed t)
         )
      )

;; Helm-bibtex ========================================================
(after! org-ref
  (setq bibtex-completion-pdf-field "file"
        bibtex-completion-bibliography "~/Dropbox/bibTex/zotbibs.bib"
        bibtex-completion-notes-path "~/Dropbox/org/notes/"
        bibtex-completion-notes-template-multiple-files
        (concat
         "#+TITLE: ${title}\n"
         "#+ROAM_KEY: cite:${=key=}\n"
         "#+ROAM_TAGS: ${keywords}\n"
         "* Notes\n"
         ":PROPERTIES:\n"
         ":Custom_ID: ${=key=}\n"
         ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
         ":AUTHOR: ${author-abbrev}\n"
         ":JOURNAL: ${journaltitle}\n"
         ":DATE: ${date}\n"
         ":YEAR: ${year}\n"
         ":DOI: ${doi}\n"
         ":URL: ${url}\n"
         ":END:\n\n"
         )
        )
  )
(map! :leader "n h b" #'helm-bibtex)

;; Org-ref ============================================================
(use-package org-ref
  :after org-roam
  :config
  (setq
   ;; org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   org-ref-default-bibliography (list "~/Dropbox/bibTex/zotbibs.bib")
   org-ref-bibliography-notes "~/Dropbox/org/notes/bibnotes.org"
   org-ref-note-title-format (concat
                              "* %y - %t\n"
                              "  :PROPERTIES:\n"
                              "  :Custom_ID: %k\n"
                              "  :NOTER_DOCUMENT: %F\n"
                              "  :ROAM_KEY: cite:%k\n"
                              "  :AUTHOR: ${author-or-editor}\n"
                              "  :JOURNAL: %j\n"
                              "  :YEAR: %y\n"
                              "  :VOLUME: %v\n"
                              "  :PAGES: %p\n"
                              "  :DOI: %D\n"
                              "  :URL: ${url}\n"
                              "  :END:\n\n")
   org-ref-notes-directory "~/Dropbox/org/notes/"
   org-ref-pdf-directory "~/Dropbox/zotero/"
   org-ref-notes-function 'orb-edit-notes
   )
)

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
	 (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
	(find-file pdf-file) ; original in org-ref-help,
                         ; opens external viewer (org-open-file pdf-file)
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function #'my/org-ref-open-pdf-at-point)
(map! :leader "n r t" #'orb-insert-non-ref)
(map! :leader "n r a" #'orb-note-actions)

;; Org-roam-bibtex
(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref)
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "file" "author-or-editor" "keywords")
        orb-process-file-keyword t
        orb-file-field-extensions '("pdf"))
  )

(after! org-roam
  (org-roam-bibtex-mode))

(after! bibtex-completion
  (setq bibtex-completion-bibliography '("~/Dropbox/bibTex/zotbibs.bib")))

;; Org-noter settings ============================================
(use-package org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list org-roam-directory)
   )
  )

;; Org-tree-slide (presentation mode in org) ====================================
(map! "<f8>" #'org-tree-slide-mode)
(map! "S-<f8>" #'org-tree-slide-skip-done-toggle)

;; These functions change the text scaling in presenation and make images inline
(defun presentation-setup ()
  (setq text-scale-mode-amount 3)
  (org-display-inline-images)
  (text-scale-mode 1)
  (beacon-mode 0))

(defun presentation-end ()
  (text-scale-mode 0)
  (beacon-mode 1))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . presentation-setup)
         (org-tree-slide-stop . presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (org-tree-slide-header t)
  (org-image-actual-width nil))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
