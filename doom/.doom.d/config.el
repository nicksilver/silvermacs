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
;; (setq doom-theme 'doom-dark+)
;; (setq doom-theme 'doom-nord)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'tsdh-dark)
;; (setq doom-theme 'doom-challenger-deep)
(setq doom-theme 'modus-vivendi)

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

;; General preferences ==========================================
;;; Word wrap
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Turn off line numbers
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
(setq denote-directory (expand-file-name "~/Dropbox/org/denote/"))
(setq denote-known-keywords '("emacs" "philosophy" "politics" "economics"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type nil) ; Org is the default, set others here
(setq denote-prompts '(title keywords))
(setq denote-excluded-directories-regexp nil)
(setq denote-excluded-keywords-regexp nil)

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
(add-hook 'find-file-hook #'denote-link-buttonize-buffer)

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
  (define-key map (kbd "C-c n n") #'denote)
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

(with-eval-after-load 'org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("n" "New note (with denote.el)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

;; Also check the commands `denote-link-after-creating',
;; `denote-link-or-create'.  You may want to bind them to keys as well.

;; If you want to have Denote commands available via a right click
;; context menu, use the following and then enable
;; `context-menu-mode'.
(add-hook 'context-menu-functions #'denote-context-menu)

;; Org-mode ====================================================
(setq org-directory "~/Dropbox/org/projects/")
(setq org-todo-file "~/Dropbox/org/projects/actions.org")

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
(setq org-agenda-files (list org-directory))

;;; Include timestamp when closing a todo
(setq org-log-done 'time)

;;; Create list of common tags
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
          :kill-buffer t))))

;;; Add strike through for DONE tasks
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "dim gray"
                 :weight normal
                 :strike-through t))))
 '(org-headline-done
            ((((class color) (min-colors 16) (background dark))
               (:foreground "dim gray" :strike-through t)))))

;;; Org-journal settings
(setq org-journal-dir "~/Dropbox/org/journal/"
      org-journal-file-type 'monthly
      org-journal-file-format "%m%Y.org"
      org-journal-date-prefix "* "
      org-journal-date-format "%A, %B %d %Y"
      org-journal-file-header "#+TITLE: %B %Y Journal\n\n")

;;; Org-roam
;;;; taken from jethro doom config:
;;;; https://github.com/jethrokuan/dots/blob/master/.doom.d/config.el
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

;;;; This changes the file name and template during note capture
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

;; Export org-roam files to Hugo markdown
(defun nicksilver/org-roam-export-all ()
  "Re-exports all Org-roam files to Hugo markdown."
  (interactive)
  (dolist (f (org-roam-list-files))
    (with-current-buffer (find-file f)
      (when (s-contains? "HUGO_BASE_DIR" (buffer-string))
        (org-hugo-export-wim-to-md)))))
