;; Nick's emacs configuration 04/15/2017

;; Install packages ---------------------------------------------
(require 'package)

;;; Code:
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(spacemacs-theme
    solarized-theme
    company
    csv-mode
    spaceline
    powerline
    ivy
    swiper
    counsel
    drag-stuff
    auto-package-update
    org-alert
    org-bullets
    multiple-cursors
    expand-region
    auctex
    exec-path-from-shell))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; This will check for package updates every 7 days
(require 'auto-package-update)
(auto-package-update-maybe)
(auto-package-update-at-time "03:00")
;; (setq auto-package-update-interval 14)
(setq auto-package-update-prompt-before-update t)


;; Theme settings -----------------------------------------------

;; Load spacemacs
(load-theme 'spacemacs-dark t)
(require 'spaceline-config)
(spaceline-emacs-theme)
(setq powerline-height 26)
(setq spaceline-workspace-numbers-unicode t)
(setq spaceline-window-numbers-unicode t)

;; Load solarized theme
;; (load-theme 'solarized-dark t)
;; (setq x-underline-at-descent-line t)
;; (require 'spaceline-config)
;; (spaceline-emacs-theme)
;; (setq powerline-height 26)
;; (setq powerline-default-separator 'bar)
;; (setq spaceline-workspace-numbers-unicode t)
;; (setq spaceline-window-numbers-unicode t)
;; (spaceline-compile)

;; Basic settings -----------------------------------------------
;; Hide startup message
(setq inhibit-startup-message t)

;; Starting directory
(find-file "~/Dropbox/OrgMode/")

;; Turn off bell sound
(setq visible-bell 1)

;; Delete text that is selected when typing
(delete-selection-mode 1)

;; Get rid of scratch message
(setq initial-scratch-message "")

;; Disable toolbar
(tool-bar-mode -1)

;; Put backups in separate folder (and other settings)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)  ; can change this if it is too slow
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Unfill paragraph
(define-key global-map "\M-Q" 'unfill-paragraph)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Automatically match parantheses
(electric-pair-mode 1)

;; Turn on visual line mode (word wrapping)
(global-visual-line-mode t)

;; Remove C-xz suspend function key
(global-set-key (kbd "C-x C-z") nil)

;; Change ctrl-z to undo
(global-set-key (kbd "C-z") 'undo)

;; Cycle buffers
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "C-<next>") 'next-buffer)

;; Set global font
;; (set-face-attribute 'default nil :font "Inconsolata-10")
(set-face-attribute 'default nil :font "Medium")  ; use 120 font
;; (set-face-attribute 'default nil :font "Hack")  ; use 110 font

;; Set font size (divide height by 10 to get pt font size)
(set-face-attribute 'default nil :height 120)

;; Bind keys to moving windows
(global-set-key [S-M-left] 'windmove-left)    ; move to left windnow
(global-set-key [S-M-right] 'windmove-right)  ; move to right window
(global-set-key [S-M-up] 'windmove-up)        ; move to upper window
(global-set-key [S-M-down] 'windmove-down)    ; move to downer window

;; Cycle through history in shell using up and down arrows
(require 'comint)
(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)

;; Exec-path-from-shell package ---------------------------------
(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-copy-env "PATH") ; need this when using anaconda
(exec-path-from-shell-copy-env "GDAL_DATA")

;; Ivy ----------------------------------------------------------
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-s") 'swiper)


;; Drag stuff (move text/regions) -------------------------------
;; Default keybindings are M-up, M-down, M-left, M-right
(require 'drag-stuff)
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)
(add-hook 'org-mode-hook (lambda () (drag-stuff-mode -1)))

;; Company mode autocomplete ------------------------------------
(add-hook 'after-init-hook 'global-company-mode)

;; Multiple cursors ---------------------------------------------
(require 'multiple-cursors)
(global-set-key (kbd "C-c M-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-.") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-,") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-c C->") 'mc/edit-ends-of-lines)

;; Expand region ------------------------------------------------
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;; Org-mode configuration ---------------------------------------
;; Standard settings
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

;; Set org-directory
(setq org-directory "~/Dropbox/OrgMode")

;; Include timestamp when closing a todo
(setq org-log-done 'time)

;; Turn on auto-fill (line wrapping)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Open pdf links in evince rather than docview
;; To open in docview use C-u C-u C-c C-o
(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))

;; Set TODO keywords
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE")))

;; Set archive file
(setq org-archive-location
      (concat org-directory
              "/archive/%s::datetree/"))

;; Set org agenda files
(setq org-agenda-files (list "~/Dropbox/OrgMode/actions.org"
			     "~/Dropbox/OrgMode/someday.org"))

;; Org-alert package
(require 'org-alert)
(setq alert-default-style 'libnotify)

;; Org-bullets package
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Turn on indent of text
(setq org-startup-indented t)

;; Auctex -------------------------------------------------------
;; Standard settings
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Turn on flyspell spell checker
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)

;; Turn on refTex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Pdf settings
(setq TeX-PDF-mode t) 

;; Set Evince as pdf viewer
(setq TeX-view-program-list
      '(("Evince" "evince --page-index=%(outpage) %o")))
(setq TeX-view-program-selection '((output-pdf "Evince")))
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-start-server t)

;; Add syntax highlighting
(setq font-latex-match-reference-keywords
      '(("citet" "[{")
	("citep" "[{")))

;; Custom settings ----------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet spacemacs-theme spaceline solarized-theme org-bullets org-alert nlinum multiple-cursors jupyter json-mode highlight-indentation find-file-in-project expand-region exec-path-from-shell ess drag-stuff csv-mode counsel company autothemer auto-package-update auctex all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
