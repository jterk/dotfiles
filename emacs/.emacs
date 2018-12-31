;;; package --- Summary -*- lexical-binding: t -*-
;;
;; File: .emacs
;;
;;; Commentary:
;;; Code:

;; Remove startup wait time.
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; Use more RAM etc, particularly for LSP mode
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Don't show the startup message.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message "jterk"
      initial-scratch-message nil)

;; Turn off the GUI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Bootstrap `use-package'
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Set up the load path
(defvar my-home (concat (getenv "HOME") "/") "Home is where the heart is.")

(add-to-list 'load-path (concat my-home "emacs"))
(add-to-list 'load-path (concat my-home "emacs-private"))

(defvar jterk/syncdir)

(defun jterk/dir-or-nil (dir)
  "Return DIR if DIR exists and is a directory."
  (if (file-directory-p dir) dir nil))

(setq jterk/syncdir
      (or (jterk/dir-or-nil (concat my-home "Dropbox (Personal)"))
          (jterk/dir-or-nil (concat my-home "Dropbox"))))

;; Configure PATH related vars from
(use-package exec-path-from-shell
  :ensure t
  :config
  (add-to-list `exec-path-from-shell-variables "GOPATH")
  (add-to-list `exec-path-from-shell-variables "VIRTUALENVWRAPPER_PYTHON")
  (add-to-list `exec-path-from-shell-variables "PROJECT_HOME")
  (exec-path-from-shell-initialize))

(setenv "LC_ALL" "en_US.UTF-8")
(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")

;; effectively disable customization by shunting it to an unused file
(if (not (file-directory-p "~/tmp"))
    (make-directory "~/tmp"))
(setq custom-file "~/tmp/custom.el")

;; org
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(use-package org
  :ensure org-plus-contrib
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
  :config
  (defvar jterk/org-refile-target)
  (setq jterk/org-refile-target (concat jterk/syncdir "/org/refile.org"))

  (setq org-agenda-files (list (concat jterk/syncdir "/org")))
  (setq org-default-notes-file jterk/org-refile-target)
  (setq org-log-done t)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))

  (defvar org-mobile-inbox-for-pull)
  (defvar org-mobile-directory)
  (setq org-directory (concat jterk/syncdir "/org"))
  (setq org-mobile-inbox-for-pull (concat jterk/syncdir "/org/mobile-inbox.org"))
  (setq org-mobile-directory (concat jterk/syncdir "/Apps/MobileOrg"))

  ;; Don't convert to super/subscript unless an explicit '{' and '}' pair is present
  (setq org-use-sub-superscripts '{})
  (defvar org-export-with-sub-superscripts)
  (setq org-export-with-sub-superscripts '{})

  (defvar org-capture-templates)
  ;; (setq org-capture-templates
  ;;       '(("t" "TODO" entry (file jterk/org-refile-target)
  ;;          "* TODO %?\n  %i\n  %a")))

  ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
  (setq org-capture-templates
        `(("t" "todo" entry (file jterk/org-refile-target)
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("r" "respond" entry (file jterk/org-refile-target)
           "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
          ("n" "note" entry (file jterk/org-refile-target)
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("m" "Meeting" entry (file jterk/org-refile-target)
           "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
          ("p" "Phone call" entry (file jterk/org-refile-target)
           "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
          ("h" "Habit" entry (file jterk/org-refile-target)
           "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

  ;; Ripped from http://doc.norang.ca/org-mode.html#Archiving
  (defun bh/skip-non-archivable-tasks ()
    "Skip trees that are not available for archiving."
    (save-restriction
      (widen)
      ;; Consider only tasks with done todo headings as archivable candidates
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
            (subtree-end (save-excursion (org-end-of-subtree t))))
        (if (member (org-get-todo-state) org-todo-keywords-1)
            (if (member (org-get-todo-state) org-done-keywords)
                (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                       (a-month-ago (* 60 60 24 (+ daynr 1)))
                       (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                       (this-month (format-time-string "%Y-%m-" (current-time)))
                       (subtree-is-current (save-excursion
                                             (forward-line 1)
                                             (and (< (point) subtree-end)
                                                  (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                  (if subtree-is-current
                      subtree-end ; Has a date in this month or last month, skip it
                    nil))  ; available to archive
              (or subtree-end (point-max)))
          next-headline))))

  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODOs"
           ((agenda "")
            (alltodo "")
            (tags "-REFILE/"
                  ((org-agenda-overriding-header "Tasks to Archive")
                   (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                   (org-tags-match-list-sublevels nil))))))))

(use-package org-velocity
  :after org)

(use-package org-journal
  :ensure t
  :after org
  :config
  (custom-set-variables
   '(org-journal-dir (concat jterk/syncdir "/org/journal"))
   '(org-journal-date-format "%A %F")))

;; functions
(defun jterk/concat-with-separator (sequence &optional separator)
  "Concatenate all of the elements of SEQUENCE as strings.
SEPARATOR, or ' ' will be used to separate the entries in
SEQUENCE."
  (mapconcat 'identity sequence (or separator " ")))

;; mu4e comes from Homebrew
(use-package mu4e
  :config
  (setq sendmail-program (concat my-home "bin/msmtpq"))
  (setq message-sendmail-f-is-evil 't)
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))

  ;; mu4e settings
  (setq mu4e-maildir "~/Maildir"
        mu4e-attachment-dir  "~/Downloads"
        send-mail-function 'sendmail-send-it
        message-send-mail-function 'sendmail-send-it
        mu4e-html2text-command 'mu4e-shr2text

        ;; Use `jterk/mu4e-update-timer-fn'
        mu4e-update-interval nil
        jterk/mu4e-update-interval 300

        mu4e-compose-keep-self-cc nil
        mu4e-headers-skip-duplicates t
        mu4e-headers-visible-lines 20
        mu4e-compose-signature nil
        mu4e-compose-signature-auto-include nil
        mu4e-sent-messages-behavior 'delete
        message-kill-buffer-on-exit t
        mu4e-headers-sort-field :date
        mu4e-headers-sort-direction :ascending)

  ;; Blacklist some problematic email address patterns. Note that mu4e needs to be
  ;; restarted before changes here take effect.
  (setq mu4e-compose-complete-ignore-address-regexp
        (concat
         ;; This will become '\(abc|xyz|zyx\)'
         "\\("
         (jterk/concat-with-separator
          '(
            ;; These become members of the "or" portion mentioned above.
            "no-?reply"
            "@docs.google.com"
            "@resource.calendar.google.com"
            "[a-z0-9]\\{9\\}@jobvite.com"
            "yammer\\+re\\+.*@yammer.com"
            "paper.*@dropbox.com"
            )
          "\\|")
         "\\)"))

  (add-hook 'mu4e-headers-mode-hook 'mu4e-disable-trailing-whitespace-hook)
  (add-hook 'mu4e-view-mode-hook 'mu4e-disable-trailing-whitespace-hook)
  (add-hook 'mu4e-view-mode-hook 'mu4e-configure-wrapping)

  ;; By default, collapse TO and CC lists, in case they contain a lot of entries
  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (let ((headers (list "To:" "Cc:")))
                (dolist (header headers)
                  (save-excursion
                    (if (search-forward header nil 'true)
                        (mu4e~view-header-field-fold)))))))

  ;; soft wrap when composing
  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (flyspell-mode)
              (auto-fill-mode -1)
              (setq message-fill-column nil)
              (visual-line-mode)))

  ;; Dynamically determine the right trash folder to use based on the message
  ;; being trashed. TODO consider moving to mu4e-extensions
  (setq mu4e-trash-folder
        (lambda (message)
          (mu4e-get-folder message 'mu4e-trash-folder)))

  ;; ... and refiling. TODO consider moving to mu4e-extensions
  (setq mu4e-refile-folder
        (lambda (message)
          (mu4e-get-folder message 'mu4e-refile-folder)))

  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  (defun mu4e-headers-keybinding-hook ()
    "Key binding hook for the mu4e headers view.
Performs the following modifications:

* Binds 'o' to `mu4e-headers-view-message'

TODO: Consider making a local copy of the key map."
    (define-key mu4e-headers-mode-map (kbd "o") 'mu4e-headers-view-message))

  (add-hook 'mu4e-headers-mode-hook 'mu4e-headers-keybinding-hook))

(use-package mu4e-contrib
  :after mu4e)

(use-package org-mu4e
  :after mu4e
  :config
  (setq org-mu4e-convert-to-html t)

  ;; Re-defined here since org -> HTML conversion is not customizable
  ;;
  ;; Patched according to http://www.brool.com/index.php/using-mu4e
  ;;
  ;; diff --git a/mu4e/org-mu4e.el b/mu4e/org-mu4e.el
  ;; index 44d5ea1..6697486 100644
  ;; --- a/mu4e/org-mu4e.el
  ;; +++ b/mu4e/org-mu4e.el
  ;; @@ -170,7 +170,9 @@ and images in a multipart/related part."
  ;;             ;; because we probably don't want to export a huge style file
  ;;             (org-export-htmlize-output-type 'inline-css)
  ;;             ;; makes the replies with ">"s look nicer
  ;; -           (org-export-preserve-breaks t)
  ;; +           (org-export-preserve-breaks nil)
  ;; +            ;; turn off table of contents
  ;; +            (org-export-with-toc nil)
  ;;             ;; dvipng for inline latex because MathJax doesn't work in mail
  ;;             (org-export-with-LaTeX-fragments 'dvipng)
  ;;             ;; to hold attachments for inline html images
  ;;
  ;; TODO: add customization; create pull request
  (defun org~mu4e-mime-convert-to-html ()
    "Convert the current body to html."
    (unless (fboundp 'org-export-string-as)
      (mu4e-error "Require function 'org-export-string-as not found"))
    (unless (executable-find "dvipng")
      (mu4e-error "Required program dvipng not found"))
    (let* ((begin
            (save-excursion
              (goto-char (point-min))
              (search-forward mail-header-separator)))
           (end (point-max))
           (raw-body (buffer-substring begin end))
           (tmp-file (make-temp-name (expand-file-name "mail"
                                                       temporary-file-directory)))
           ;; because we probably don't want to skip part of our mail
           (org-export-skip-text-before-1st-heading nil)
           ;; because we probably don't want to export a huge style file
           (org-export-htmlize-output-type 'inline-css)
           ;; makes the replies with ">"s look nicer
           (org-export-preserve-breaks nil)
           ;; turn off table of contents
           (org-export-with-toc nil)
           ;; dvipng for inline latex because MathJax doesn't work in mail
           (org-export-with-LaTeX-fragments 'dvipng)
           ;; to hold attachments for inline html images
           (html-and-images
            (org~mu4e-mime-replace-images
             (org-export-string-as raw-body 'html t)
             tmp-file))
           (html-images (cdr html-and-images))
           (html (car html-and-images)))
      (delete-region begin end)
      (save-excursion
        (goto-char begin)
        (newline)
        (insert (org~mu4e-mime-multipart
                 raw-body html (mapconcat 'identity html-images "\n")))))))

(use-package mu4e-extensions
  :after mu4e)

;; For tramp
(setq tramp-auto-save-directory "~/tmp/tramp/")
(if (file-exists-p "/bin/bash")
    (setq shell-file-name "/bin/bash"))
;; Colors
(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-dark t))

;; Font
(defvar jterk/frame-font "-apple-Menlo-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")
(if (x-list-fonts jterk/frame-font)
    (progn
      (set-frame-font jterk/frame-font)
      (add-to-list 'default-frame-alist `(font . ,jterk/frame-font))))

;; Frame size
(add-to-list 'default-frame-alist '(height . 65))
(add-to-list 'default-frame-alist '(width . 202))

;; Syntax Highlighting
(use-package font-lock
  :config (cond ((fboundp 'global-font-lock-mode)
                 (global-font-lock-mode t)
                 (setq font-lock-maximum-decoration t))))

;; Tabs -> 2 spaces
(setq c-basic-offset 2)
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq python-indent-offset 4)
(setq typescript-indent-level 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 100)

;; Show trailing whitespace
(setq-default show-trailing-whitespace 't)

;; Make it harder to quit emacs.
(global-unset-key (kbd "s-q"))

;; Don't show the font menu.
(global-unset-key (kbd "s-t"))

(global-unset-key (kbd "s-k"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "s-p"))
(global-unset-key (kbd "C-x C-z"))

;; Key Bindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key "\C-xu" 'browse-url-at-point)

;; Keep various files in ~/tmp.
(setq backup-directory-alist '(("." . "~/tmp/backups")))
(setq ido-save-directory-list-file "~/tmp/ido.last")
(setq recentf-save-file "~/tmp/recentf")
(setq semanticdb-default-save-directory "~/tmp/semantic.cache")
(setq wisent-log-file "~/tmp/wisent.output")

;; Browser
(setq browse-url-browser-function 'browse-url-default-browser)

;; No line truncation!
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows t)

;; Don't make me type 'yes'. Ever.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use objc-mode for .m and .mm files
(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))

;; And Objective C .h files
(defun bh-choose-header-mode ()
  "Determine the mode for a .h file."
  (interactive)
  (if (string-equal (substring (buffer-file-name) -2) ".h")
      (progn
        ;; OK, we got a .h file, if a .m file exists we'll assume it's
        ;; an objective c file. Otherwise, we'll look for a .cpp file.
        (let ((dot-m-file (concat (substring (buffer-file-name) 0 -1) "m"))
              (dot-cpp-file (concat (substring (buffer-file-name) 0 -1) "cpp")))
          (if (file-exists-p dot-m-file)
              (progn
                (objc-mode)
                )
            (if (file-exists-p dot-cpp-file)
                (c++-mode)))))))

(add-hook 'find-file-hook 'bh-choose-header-mode)

;; xcodebuild (UNUSED)
(defun bh-compile ()
  "Invoke xcodebuid."
  (interactive)
  (let ((df (directory-files "."))
        (has-proj-file nil))
    (while (and df (not has-proj-file))
      (let ((fn (car df)))
        (if (> (length fn) 10)
            (if (string-equal (substring fn -10) ".xcodeproj")
                (setq has-proj-file t))))
      (setq df (cdr df)))
    (if has-proj-file
        (compile "xcodebuild -configuration Debug")
      (compile "make"))))

;; Make TODO and FIXME more prominent
(defun add-fixme-todo-highlights ()
  "Highlight TODO and FIXME by adding to font-lock keywords."
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\)" 1 font-lock-warning-face t))))

(add-hook 'c-mode-common-hook 'add-fixme-todo-highlights)
(add-hook 'nxml-mode-hook 'add-fixme-todo-highlights)

;; Java Mode (and descendents) indentation rules.
(defun my-c-lineup-java-inher (langelem)
  "Line up Java implements and extends declarations.
Lines up LANGELEM in line with Java standard conventions.  TODO:
fix for inner classes.

I.e.

public class A extends B,
    C, D, E {
<--> 2 x c-basic-offset

public class A
    extends B implements C,
    D, E {
<--> 2 x c-basic-offset"
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (let* ((syn (car (c-guess-basic-syntax)))
           (sym (c-langelem-sym syn)))
      (cond
       ((or (eq `inher-cont sym)
            (eq `inher-intro sym))
        (move-beginning-of-line nil)
        (while (looking-at "[ \t]")
          (forward-char))
        (vector (current-column)))
       ((or (eq `topmost-intro sym)
            (eq `topmost-intro-cont sym))
        (vector (+ (* 2 c-basic-offset) (c-langelem-col syn t))))))))

(defconst my-java-style
  `("java"
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)
    (c-offsets-alist . ((arglist-intro . ++)
                        (arglist-cont-nonempty . ++)
                        (func-decl-cont . ++)
                        (statement-cont . ++)
                        (topmost-intro-cont . 0)
                        (inher-intro . ++)
                        (inher-cont . my-c-lineup-java-inher))))
  "My Java Style.")

(c-add-style "my-java" my-java-style)

(add-hook 'java-mode-hook
          '(lambda ()
             (c-set-style "my-java")))

;; Emacs server
(server-start)

(defun buffer-visiting-file-p (buffer-name file-name)
  "Naively determine if BUFFER-NAME is visiting a file.

If FILE-NAME is nil, prints an error message and returns nil.
Otherwise returns 't.  This is intended to be used as:

\(buffer-visiting-file-p (buffer-name) (buffer-file-name))"
  (if (not file-name)
      (progn (message "Buffer '%s' is not visiting a file!" buffer-name)
             nil)
    't))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (buffer-visiting-file-p name filename)
        (if (get-buffer new-name)
            (message "A buffer named '%s' already exists!" new-name)
          (progn
            (rename-file name new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil))))))

;; info
(setq Info-additional-directory-list (list "~/docs/info"))

;; Show column numbers in the status bar
(column-number-mode t)

;; Ido Mode configuration
(setq ido-ignore-buffers '("\\` "
                           "\\*magit-process:"
                           "\\*magit-diff:"
                           "\\*magit-revision:"
                           "\\*magit-log:"
                           "\\*magit-refs:"))
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

(put 'upcase-region 'disabled nil)

(use-package company
  :ensure t
  :config
  (global-company-mode t))

;; The Silver Searcher
(use-package ag
  :ensure t)

;; haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

;; Use python-mode for pystachio files
(add-to-list 'auto-mode-alist '("\\.pyst$" . python-mode))

;; pydoc in python-mode
(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc)))

(use-package magit
  :ensure t
  :config
  (setq magit-repository-directories `((,(concat my-home "src") . 1)))
  (setq magit-fetch-arguments '("--prune"))
  (set-variable 'magit-auto-revert-mode nil)
  (set-variable 'global-auto-revert-mode t))

;; Use `magit-blame-addition' instead of `vc-annotate'
(global-set-key "\C-xvg" 'magit-blame-addition)

(defun trim-string-list (list)
  "Remove empty and non-string elements from LIST.
Returns nil if LIST is nil or LIST contains only empty strings."
  (if list
      (let ((first (car list))
            (rest (cdr list)))
        (if (and (stringp first)
                 (< 0 (length first)))
            (cons first (trim-string-list rest))
          (trim-string-list rest)))))

(ert-deftest test-trim-string-list ()
  "Tests for `trim-string-list'."
  (should (equal nil (trim-string-list (list ""))))
  (should (equal (list "astring") (trim-string-list (list "astring"))))
  (should (equal (list "1" "2" "3") (trim-string-list (list "" "" "1" "" "2" "3" "" "")))))

(defun insert-guid-at-point ()
  "Insert a GUID (globally unique identifier) at point."
  (interactive)
  (insert (replace-regexp-in-string "[\n\r]*" "" (shell-command-to-string "uuidgen"))))

;; web-mode
(setq
 web-mode-attr-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-markup-indent-offset 2
 web-mode-script-padding 2)

(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))

;; org-mode additions & customizations
(defun jterk/org-yank-sql-table ()
  "Yank the current entry in the kill ring as a table.
Converts 'sql' result tables into `org-mode' tables in the
process.

TODO: Hook into org-yank?"
  (interactive)
  (save-excursion
    (insert (replace-regexp-in-string "^\+" "|" (current-kill 0))))
  (org-ctrl-c-ctrl-c))

(defun jterk/org-insert-src-block ()
  "Insert an `org-mode' code block at point.

  #+BEGIN_SRC

  #+END_SRC

TODO: Add an (optional) type"
  (interactive)
  (save-excursion
    (insert "#+BEGIN_SRC ")
    (indent-for-tab-command)
    (insert "\n\n#+END_SRC")
    (indent-for-tab-command))
  (end-of-line))

(add-hook
 'org-mode-hook
 (lambda ()
   (flyspell-mode)
   (define-key org-mode-map (kbd "C-c s") 'jterk/org-insert-src-block)))

(use-package org-table)
(defun jterk/org-table-copy-right (n)
  "Copy a field right in the current row.

If the field at the cursor is empty, copy into it the content of
the nearest non-empty field to the left.  With argument N, use
the nth non-empty field.  If the current field is not empty it is
copied right to the next column and the cursor is moved with it.
Repeating this command causes the row to be filled
column-by-column.

See `org-table-copy-down'

TODO (maybe): Currently missing `org-table-copy-increment'
functionality."
  (interactive "p")
  (let* ((col (current-column))
         (field (save-excursion (org-table-get-field)))
         (non-empty (string-match "[^ \t]" field)))
    (org-table-check-inside-data-field)
    (if non-empty
        (progn
          (setq txt (org-trim field))
          (org-table-next-field) ;; FIXME this will create a NEW field if we're at the end
          (org-table-blank-field))
      ()) ;; TODO get nth non-empty field
    (progn
      (insert txt)
      (org-table-align))))

;; If there's a non-org-scratch buffer, replace it with ~/tmp/*scratch*
(let ((scratch-buffer (get-buffer "*scratch*")))
  (if scratch-buffer
      (kill-buffer scratch-buffer)))
(find-file-noselect (concat my-home "tmp/*scratch*"))

(use-package web-mode
  :ensure t
  :mode "\\.ejs\\'")

(use-package htmlize :ensure t)
(use-package ess :ensure t)
(use-package slime :ensure t)

(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (flyspell-mode))))


;; Make scratch buffers for different modes
(mapc (lambda (mode)
        (let* ((buffer-name (concat "*" (symbol-name mode) "-scratch*"))
               (buffer (find-file-noselect (concat my-home "tmp/" buffer-name))))
          (with-current-buffer buffer
            (funcall mode))))
      (list 'org-mode 'markdown-mode))

(setq inferior-lisp-program "sbcl")
(setq nginx-indent-level 2)
(global-hl-line-mode)

(add-hook 'eww-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; eshell
(use-package eshell)
(use-package em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-smart-initialize)
            (setq show-trailing-whitespace nil)))

(add-hook 'git-commit-setup-hook
          (lambda ()
            (flyspell-mode)))

(add-hook 'prog-mode-hook
          (lambda ()
            (flyspell-prog-mode)))

(use-package json-mode :ensure t)

(use-package restclient
  :ensure t)

;; Sometimes eshell doesn't cut it
(add-hook 'shell-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; Move between windows with SHIFT + arrow
(windmove-default-keybindings)

(defconst jterk/eshell-csi-regexp "\033\\[\\([0-9]*G\\|\\?25h\\)"
  "Regexp that matches ANSI CSI codes.")

(defun jterk/eshell-handle-control-codes ()
  "Attempt to handle (some) ANSI codes.

Currently limited to stripping problematic codes."
  (save-excursion
    (goto-char eshell-last-output-block-begin)
    (unless (eolp)
      (beginning-of-line))
    (while (re-search-forward jterk/eshell-csi-regexp eshell-last-output-end t)
      (replace-match ""))))

(add-to-list 'eshell-output-filter-functions 'jterk/eshell-handle-control-codes)

(autoload 'ansi-color-apply-on-region "ansi-color")

(defun jterk/compilation-filter-hook ()
  "Compilation filter.

Uses `ansi-color-apply-on-region' to handle ANSI color codes, and
strips other problematic ANSI codes."
  (save-excursion
    (let ((end (point)))
      (ansi-color-apply-on-region compilation-filter-start end)
      (goto-char compilation-filter-start)
      (while (re-search-forward jterk/eshell-csi-regexp eshell-last-output-end t)
        (replace-match "")))))

(add-hook 'compilation-filter-hook 'jterk/compilation-filter-hook)

;; Advice for `eshell-watch-for-password-prompt' to prevent prompting for a password when the string
;; 'password:' appears in build/test output.
(defun jterk/eshell-password-prompt-predicate ()
  "Advice predicate for eshell-watch-for-password-prompt.

Returns t if eshell-watch-for-password-prompt should be invoked."
  (when (eshell-interactive-process)
    (save-excursion
      (let ((case-fold-search t))
        (goto-char eshell-last-output-block-begin)
        (beginning-of-line)
        (if (re-search-forward "DEBUG.*LOGGER" eshell-last-output-end t)
            nil
          t)))))

(advice-add
 'eshell-watch-for-password-prompt
 :before-while
 #'jterk/eshell-password-prompt-predicate)

(use-package visual-fill-column
  :ensure t
  :config
  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode))

(use-package flycheck
  :ensure t
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix
    flycheck-command-map)
  (global-flycheck-mode))

(use-package yaml-mode
  :ensure t)

(use-package coffee-mode
  :ensure t)

(use-package go-mode
  :ensure t
  )

(use-package protobuf-mode
  :ensure t
  :config
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (c-add-style "my-protobuf-style"
                           '((c-basic-offset . 4)
                             (indent-tabs-mode . nil)))
              (c-set-style "my-protobuf-style"))))

(use-package typescript-mode
  :ensure t)

(use-package pyvenv
  :ensure t)

(use-package auto-virtualenv
  :ensure t
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'focus-in-hook 'auto-virtualenv-set-virtualenv))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-jar-path "/usr/local/Cellar/plantuml/1.2020.2/libexec/plantuml.jar")
  (setq plantuml-output-type "png")
  (add-to-list 'auto-mode-alist '("\\.uml$" . plantuml-mode)))

(use-package flycheck-plantuml
  :ensure t
  :config
  (flycheck-plantuml-setup))

(use-package bazel-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\BUILD.in\\'" . bazel-mode)))

;; Detect aspell on Windows
;; TODO use PATH instead of explicit exe. locations
(setq win-aspell "C:/tools/msys64/mingw64/bin/aspell.exe")
(if (file-exists-p win-aspell)
    (setq ispell-program-name win-aspell))

(use-package ag
  :ensure t
  :config
  (setq win-ag "C:/tools/msys64/mingw64/bin/ag.exe")
  (if (file-exists-p win-ag)
      (setq ag-executable win-ag)))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  (go-mode . lsp)
  (go-mode . (lambda ()
               (add-hook 'before-save-hook #'lsp-format-buffer t t)
               (add-hook 'before-save-hook #'lsp-organize-imports t t)))
  :config
  (use-package lsp-ui
    :ensure t))

(use-package blog-publish)

;; Stuff for work. Do all of this last so that it can override anything set above.
(let ((db-emacs (concat my-home "Dropbox Dropbox/Jason Terk/emacs")))
  (if (file-exists-p db-emacs)
      (progn
        (add-to-list 'load-path db-emacs)
        (require 'db)
        (require 'stone-mode)
        (require 'pyxl-mode)
        (require 'jterk-mu4e)
        (if (fboundp 'mu4e-apply-account-vars)
            (mu4e-apply-account-vars (cdr (assoc mu4e-default-account mu4e-account-alist))))
        (setq yas-snippet-dirs (append yas-snippet-dirs `(,(concat db-emacs "/snippets")))))))

(provide '.emacs)
;;; .emacs ends here
