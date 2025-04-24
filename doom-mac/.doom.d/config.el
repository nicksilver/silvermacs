;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq user-full-name "Nick Silverman"
user-mail-address "nick.silverman11@gmail.com")

;; Viz settings
;;; Fonts
;; (setq doom-font (font-spec :family "Fira Code Retina" :size 14)) ;
;; (setq doom-variable-pitch-font (font-spec :family "Cantarell" :size 14 :weight 'regular))

(setq doom-font (font-spec :family "JetBrains Mono" :size 14))
(setq doom-variable-pitch-font (font-spec :family "Cantarell" :size 14 :weight 'regular))

;;; Theme
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-badger)
;; (setq doom-theme 'doom-dark+)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'modus-vivendi)
(load-theme 'spacemacs-dark t)

;;; Modeline stuff (see https://github.com/seagle0128/doom-modeline)
(setq doom-modeline-height 35)
(setq doom-modeline-bar-width 4)
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
(setq doom-modeline-indent-info t)
(setq doom-modeline-checker-simple-format t)
(setq doom-modeline-number-limit 99)
(setq doom-modeline-vcs-max-length 12)
(setq doom-modeline-workspace-name t)
(setq doom-modeline-lsp t)
(setq doom-modeline-modal-icon t)

;; Load my-functions.el ========================================
(load! "my-functions")

;; General preferences ==========================================
;;; Word wrap
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Turn off line numbers
(setq display-line-numbers-type nil)

;;; This is to use pdf-tools instead of doc-viewer
(use-package pdf-tools
  :config
  (pdf-tools-install)
  ;; This means that pdfs are fitted to width by default when you open them
  (setq-default pdf-view-display-size 'fit-width))

;; Vterm =======================================================
(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "bash")
  (setq vterm-max-scrollback 10000))

;; ESS settings =================================================
(use-package ess-r-mode
  :bind
  (:map ess-r-mode-map
        ("_" . ess-insert-assign))
  (:map inferior-ess-r-mode-map
        ("_" . ess-insert-assign)))

;; Denote ======================================================
;; This removes the automatic #+title when creating new org file
(set-file-template! "\\([0-9T]+?--.*__.*\\)"
  :trigger "__denote.org"
  :mode 'org-mode)

(require 'denote)

;; Remember to check the doc strings of those variables.
(setq denote-directory (expand-file-name "~/org/denote/"))
(setq denote-known-keywords '("admin" "tech" "project" "research" "idea"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type nil) ; Org is the default, set others here
(setq denote-prompts '(title keywords))
(setq denote-excluded-directories-regexp nil)
(setq denote-excluded-keywords-regexp nil)

;; Denote journal
;; (require 'denote-journal-extras)
;; (setq denote-journal-extras-directory "~/org/denote/journal")
;; (setq denote-journal-extras-title-format nil) ; always prompt for title
;; (setq denote-journal-extras-keyword "journal")

;; Pick dates, where relevant, with Org's advanced interface:
(setq denote-date-prompt-use-org-read-date t)

;; Read this manual for how to specify `denote-templates'.  We do not
;; include an example here to avoid potential confusion.

(setq denote-date-format nil) ; read doc string

;; By default, we do not show the context of links.  We just display
;; file names.  This provides a more informative view.
(setq denote-backlinks-show-context t)

;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
;; advanced.

;; If you use Markdown or plain text files (Org renders links as buttons
;; right away)
(add-hook 'find-file-hook #'denote-fontify-links-mode)
;; (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

;; We use different ways to specify a path for demo purposes.
(setq denote-dired-directories
      (list denote-directory
            (thread-last denote-directory (expand-file-name "attachments"))
            (expand-file-name "~/Documents/books")))

;; Generic (great if you rename files Denote-style in lots of places):
;; (add-hook 'dired-mode-hook #'denote-dired-mode)
;;
;; OR if only want it in `denote-dired-directories':
(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
(denote-rename-buffer-mode 1)

;; Denote DOES NOT define any key bindings.  This is for the user to
;; decide.  For example:
(let ((map global-map))
  (define-key map (kbd "C-c n n") #'denote-open-or-create)
  (define-key map (kbd "C-c n c") #'denote-region) ; "contents" mnemonic
  (define-key map (kbd "C-c n N") #'denote-type)
  (define-key map (kbd "C-c n d") #'denote-date)
  (define-key map (kbd "C-c n z") #'denote-signature) ; "zettelkasten" mnemonic
  (define-key map (kbd "C-c n s") #'denote-subdirectory)
  (define-key map (kbd "C-c n t") #'denote-template)
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
  (define-key map (kbd "C-c n I") #'denote-add-links)
  (define-key map (kbd "C-c n b") #'denote-backlinks)
  (define-key map (kbd "C-c n f f") #'denote-find-link)
  (define-key map (kbd "C-c n f b") #'denote-find-backlink)
  ;; Note that `denote-rename-file' can work from any context, not just
  ;; Dired bufffers.  That is why we bind it here to the `global-map'.
  (define-key map (kbd "C-c n r") #'denote-rename-file)
  (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

;; Key bindings specifically for Dired.
(let ((map dired-mode-map))
  (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
  (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-files)
  (define-key map (kbd "C-c C-d C-k") #'denote-dired-rename-marked-files-with-keywords)
  (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))

;; Also check the commands `denote-link-after-creating',
;; `denote-link-or-create'.  You may want to bind them to keys as well.

;; If you want to have Denote commands available via a right click
;; context menu, use the following and then enable
;; `context-menu-mode'.
(add-hook 'context-menu-functions #'denote-context-menu)

;; Citar ======================================================
(use-package citar
  :custom
  (citar-bibliography '("~/Zotero/better-bibtex/my-library.bib" ))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

;;; Adds contextual access actions in the minibuffer and at-point
(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;;; Settings to integrate with denote
(use-package citar-denote
  :custom
  ;; Allow multiple notes per bibliographic entry
  (citar-open-always-create-notes nil)
  ;; Use package defaults
  (citar-denote-file-type 'org)
  (citar-denote-subdir nil)
  (citar-denote-signature nil)
  (citar-denote-template nil)
  (citar-denote-keyword "bib")
  (citar-denote-use-bib-keywords nil)
  (citar-denote-title-format "title")
  (citar-denote-title-format-authors 1)
  (citar-denote-title-format-andstr "and")
  :init
  (citar-denote-mode)
  ;; Bind all available commands
  :bind (("C-c w c" . citar-create-note)
         ("C-c w n" . citar-denote-open-note)
         ("C-c w d" . citar-denote-dwim)
         ("C-c w e" . citar-denote-open-reference-entry)
         ("C-c w a" . citar-denote-add-citekey)
         ("C-c w k" . citar-denote-remove-citekey)
         ("C-c w r" . citar-denote-find-reference)
         ("C-c w l" . citar-denote-link-reference)
         ("C-c w f" . citar-denote-find-citation)
         ("C-c w x" . citar-denote-nocite)
         ("C-c w y" . citar-denote-cite-nocite)))

;; Org-mode ====================================================
(setq org-directory "~/org/gtd/")
(setq org-todo-file "~/org/gtd/actions.org")

;; This logs org nodes into drawer under heading
(setq org-log-into-drawer t)

;;; Set TODO keywords
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE")))

;;; Allow ordered lists to include A. B. C. etc.
(setq org-list-allow-alphabetical t)

;;; Set archive file
(setq org-archive-location
      (concat org-directory
              "/archive/%s::datetree/"))

;;; Set org agenda files
(setq org-agenda-files (list org-directory denote-directory))

;;; Include timestamp when closing a todo
(setq org-log-done 'time)

;;; Create list of common tags
(setq org-tag-alist '(("@home" . ?H)
                      ("@mitre" . ?M)
                      ("project" . ?p)
                      ("research" . ?r)
                      ("idea" . ?i)))

;;; Capture
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
          :kill-buffer t)

        ("d" "Denote" plain
         (file denote-last-path)
         #'denote-org-capture
         :no-save t
         :immediate-finish nil
         :kill-buffer t
         :jump-to-captured t))))

;;; Add strike through for DONE tasks
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "dim gray"
                 :weight normal
                 :strike-through t))))
 '(org-headline-done
            ((((class color) (min-colors 16) (background dark))
               (:foreground "dim gray" :strike-through t)))))
