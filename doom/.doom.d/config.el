;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Nick Silverman"
      user-mail-address "nick.silverman11@gmail.com")

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
(setq doom-font (font-spec :family "monospace" :size 14))
;; (setq doom-font (font-spec :family "Ubuntu Mono" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-dark)
;; (setq doom-theme 'doom-vibrant)
;; (setq doom-theme 'doom-solarized-light)
;; (setq doom-theme 'doom-oceanic-next)
;; (setq doom-theme 'doom-sourcerer)
;; (setq doom-theme 'doom-wilmersdorf)

;;; Org-mode settings ==========================================
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/projects/")
(setq org-todo-file "~/Dropbox/org/projects/actions.org")

;; These are Nick's personal org settings
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
(setq org-agenda-files (list org-todo-file
                             "~/Dropbox/org/projects/someday.org"))

;; Include timestamp when closing a todo
(setq org-log-done 'time)

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

;; Org-roam settings ===========================================
(setq org-roam-directory "~/Dropbox/org/notes/")

;; Deft
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

;; This speeds up deft...but limits the amount of files you see
;; Overwrite `deft-current-files` for the `deft-buffer-setup` and limit it to 50 entries
(defun anks-deft-limiting-fn (orig-fun &rest args)
  (let
      ((deft-current-files (-take 50 deft-current-files)))
    (apply orig-fun args)))

(advice-add 'deft-buffer-setup :around #'anks-deft-limiting-fn)

;; Org-roam-bibtex settings =============================================
;; Based on https://rgoswami.me/posts/org-note-workflow/#search
(setq zot-bib-file "~/Dropbox/bibTex/zotbibs.bib")
(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t))))

;; Helm-bibtex
(after! org-ref
  (setq bibtex-completion-notes-path org-roam-directory
        bibtex-completion-bibliography zot-bib-file
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-template-multiple-files
        (concat
         "#+TITLE: ${title}\n"
         "#+ROAM_KEY: cite:${=key=}\n"
         "* TODO Notes\n"
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
)))

;; Org-ref
(use-package org-ref
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list zot-bib-file)
         ;;org-ref-bibliography-notes "/home/haozeke/Git/Gitlab/Mine/Notes/bibnotes.org"
         org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory org-roam-directory
         org-ref-notes-function 'orb-edit-notes
         ))

;; Org-noter
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


;; General preferences =========================================
;; Word wrap
;; (setq visual-fill-column 80)
;; enable word-wrap (almost) everywhere
(global-visual-line-mode t)
(+global-word-wrap-mode +1)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


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
