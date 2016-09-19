;;; package --- Summary -*- lexical-binding: t -*-
;;
;; File: .emacs
;;
;;; Commentary:
;;; Code:

;; Remove startup wait time.
(modify-frame-parameters nil '((wait-for-wm . nil)))

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
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Set up the load path
(defvar my-home (concat (getenv "HOME") "/") "Home is where the heart is.")
(add-to-list 'load-path (concat my-home "emacs"))
(add-to-list 'load-path (concat my-home "emacs-private"))

;; Configure PATH related vars from
(use-package exec-path-from-shell
  :ensure t
  :config
  (add-to-list `exec-path-from-shell-variables "GOPATH")
  (exec-path-from-shell-initialize))

(setenv "LC_ALL" "en_US.UTF-8")

;; org
(use-package org
  :ensure t)
(defvar jterk/org-todo-file)
(setq jterk/org-todo-file (concat my-home "sync/docs/org/todo.org"))
(setq org-log-done t)
(setq org-agenda-files (list jterk/org-todo-file))
(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))
(setq org-default-notes-file jterk/org-todo-file)
(setq org-directory (concat my-home "sync/docs/org"))

(defvar org-mobile-inbox-for-pull)
(defvar org-mobile-directory)
(setq org-mobile-inbox-for-pull (concat org-directory "/mobile-inbox.org"))
(setq org-mobile-directory (concat my-home "Dropbox/Apps/MobileOrg"))

;; Don't convert to super/subscript unless an explicit '{' and '}' pair is present
(setq org-use-sub-superscripts '{})
(defvar org-export-with-sub-superscripts)
(setq org-export-with-sub-superscripts '{})

(defvar org-capture-templates)
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline jterk/org-todo-file "Inbox")
         "* TODO %?\n  %i\n  %a")))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-unset-key (kbd "s-k"))

;; functions
(defun jterk/concat-with-separator (sequence &optional separator)
  "Concatenate all of the elements of SEQUENCE as strings.
SEPARATOR, or ' ' will be used to separate the entries in
SEQUENCE."
  (mapconcat 'identity sequence (or separator " ")))

;; mu4e comes from Homebrew
(require 'mu4e)
(require 'mu4e-contrib)
(require 'org-mu4e)
(require 'mu4e-extensions)

(setq sendmail-program (concat my-home "bin/msmtpq"))
(setq message-sendmail-f-is-evil 't)
(setq message-sendmail-extra-arguments '("--read-envelope-from"))

(defun jterk/mu4e-shr2text ()
  "Html to text using the shr engine.
This can be used in `mu4e-html2text-command' in a new enough
Emacs.  Based on code by Titus von der Malsburg."
  (interactive)
  (let ((dom (libxml-parse-html-region (point-min) (point-max)))
        (shr-blocked-images "^https?.*")
        (shr-table-horizontal-line ?-))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))

;; mu4e settings
(setq mu4e-maildir "~/Maildir"
      mu4e-attachment-dir  "~/Downloads"
      send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it
      mu4e-html2text-command 'jterk/mu4e-shr2text
      mu4e-update-interval 300
      mu4e-compose-keep-self-cc nil
      mu4e-headers-skip-duplicates t
      mu4e-headers-visible-lines 20
      mu4e-compose-signature nil
      mu4e-compose-signature-auto-include nil
      mu4e-sent-messages-behavior 'delete
      message-kill-buffer-on-exit t
      org-mu4e-convert-to-html t
      mu4e-headers-sort-field :date
      mu4e-headers-sort-direction :ascending)

;; For tramp
(setq shell-file-name "/bin/bash")

(require 'jterk-mu4e)

;; Set mu4e defaults from first account in the list
(mu4e-apply-account-vars (cdr (assoc my-mu4e-default-account my-mu4e-account-alist)))

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

(add-hook 'mu4e-headers-mode-hook 'my-mu4e-disable-trailing-whitespace-hook)
(add-hook 'mu4e-view-mode-hook 'my-mu4e-disable-trailing-whitespace-hook)
(add-hook 'mu4e-view-mode-hook 'my-mu4e-configure-wrapping)

;; soft wrap when composing
(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (flyspell-mode)
            (auto-fill-mode -1)
            (setq message-fill-column nil)
            (visual-line-mode)))

;; Dynamically determine the right trash folder to use based on the message being trashed.
(setq mu4e-trash-folder
      (lambda (message)
        (my-mu4e-get-folder message 'mu4e-trash-folder)))

;; ... and refiling.
(setq mu4e-refile-folder
      (lambda (message)
        (my-mu4e-get-folder message 'mu4e-refile-folder)))

(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(defun my-mu4e-headers-keybinding-hook ()
  "Key binding hook for the mu4e headers view.
Performs the following modifications:

* Binds 'o' to `mu4e-headers-view-message'"
  (define-key mu4e-headers-mode-map (kbd "o") 'mu4e-headers-view-message))

(add-hook 'mu4e-headers-mode-hook 'my-mu4e-headers-keybinding-hook)

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
(require 'org-mu4e)
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
		  raw-body html (mapconcat 'identity html-images "\n"))))))

;; Colors
(use-package zenburn-theme
  :ensure t)
;; Experimenting with a light theme
;; (load-theme 'zenburn t)

(use-package leuven-theme
  :ensure t)
;; (load-theme 'leuven t)

(use-package solarized-theme
  :ensure t)
(load-theme 'solarized-light t)

;; Font
(set-frame-font "-apple-Menlo-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")
;; (set-frame-font "-*-Hack-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")
;; (set-face-attribute 'default nil :family "Menlo" :height 110)

;; Frame size
(add-to-list 'default-frame-alist '(height . 65))
(add-to-list 'default-frame-alist '(width . 202))
(add-to-list 'default-frame-alist '(font . "-apple-Menlo-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1"))

;; Syntax Highlighting
(require 'font-lock)
(cond ((fboundp 'global-font-lock-mode)
       (global-font-lock-mode t)
       (setq font-lock-maximum-decoration t)))

;; Tabs -> 2 spaces
(setq c-basic-offset 2)
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq python-indent-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 80)

;; Show trailing whitespace
(setq-default show-trailing-whitespace 't)

;; Make it harder to quit emacs.
(global-unset-key (kbd "s-q"))

;; Don't show the font menu.
(global-unset-key (kbd "s-t"))

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
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

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

(defun java-find-tests ()
  "Find the unit test files for the current (Java) buffer."
  (interactive)
  (let ((src-path "src/main/java")
        (test-path "src/test/java"))
    (if (buffer-visiting-file-p (buffer-name) buffer-file-name)
        ;; Look for src/main/java in the file path
        (if (not (and (string-match src-path buffer-file-name)
                      (string-equal "java" (file-name-extension buffer-file-name))))
            (message "'%s' doesn't appear to be a Java source file." buffer-file-name)
          (let* ((src-filename (file-name-base buffer-file-name))
                 (src-directory (file-name-directory buffer-file-name))
                 (test-filename (concat src-filename "Test.java"))
                 (test-directory (replace-regexp-in-string src-path test-path src-directory))
                 (test-file-path (concat test-directory "/" test-filename)))
            (if (file-exists-p test-file-path)
                (find-file test-file-path)
              (message "'%s' doesn't exist." test-file-path)))))))

(defun my-eclim-java-format-region (prefix)
  "Format a Java region.

If PREFIX is not nil the entire buffer is formatted.

If PREFIX is nil the active region is formatted.  If there is no
active region no formatting is performed."
  (interactive "P")
  (if prefix
      (eclim-java-format)
    (if (not (use-region-p))
        (message "No active region")
      (eclim/execute-command
       "java_format" "-p" "-f" ("-h" (region-beginning)) ("-t" (region-end)) "-e"))))

;; C-c C-e f s should format the region, if possible.
(defun my-eclim-hook ()
  "My Eclim Hook."
  (substitute-key-definition 'eclim-java-format 'my-eclim-java-format-region eclim-mode-map))

(add-hook 'eclim-mode-hook 'my-eclim-hook)

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

;; eclim
(setq eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim")
(setq eclim-autoupdate-problems t)
(setq eclim-problems-hl-errors t)

(when (file-exists-p eclim-executable)
  (setq eclim-autoupdate-problems t)
  (setq eclim-problems-hl-errors t)

  (global-eclim-mode)

  (setq eclim-use-yasnippet nil))

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)

(help-at-pt-set-timer)

(use-package company
  :ensure t)
(global-company-mode t)

;; The Silver Searcher
(use-package ag
  :ensure t)

;; haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; pydoc in python-mode
(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc)))

(use-package magit
  :ensure t)
(setq magit-repository-directories (list (concat my-home "dev")))
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-fetch-arguments '("--prune"))

;; Use `magit-blame' instead of `vc-annotate'
(global-set-key "\C-xvg" 'magit-blame)

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

(defun magit-gerrit-project-name ()
  "Get the name of the current git repository.

Uses `magit-get-top-dir' to find the root of the currect
repository, stripping '.git', if present."
  (let ((project-dir (directory-file-name (magit-git-dir))))
    (save-match-data
      (string-match "/[^/]*\\'" project-dir)
      (substring project-dir (+ 1 (match-beginning 0))))))

(defun magit-gerrit-default-reviewers (project)
  "Get the default reviewers for PROJECT.
Reads from `magit-gerrit-default-reviewers-alist'.  Returns the
list of reviewers, or nil."
  (cadr (assoc project magit-gerrit-default-reviewers-alist)))

(defun magit-gerrit-submit-review (&optional reviewers)
  "Submit a (set of) commit(s) to Gerrit for review.
Determines the default reviewer(s) (if any) for the current
project using `magit-gerrit-default-reviewers-alist' and then
prompts for any additional reviewers.  The reviewer prompt offers
completion based on `magit-gerrit-reviewers-list'.  Once the list
of REVIEWERS is obtained, prompts for the name of the remote
branch to which the review should be pushed.

TODO: Once successful, put the URL of the review on the
clipboard."
  (interactive
   (let ((default-reviewers (magit-gerrit-default-reviewers (magit-gerrit-project-name))))
     (list (trim-string-list
            (completing-read-multiple "Reviewers: " magit-gerrit-reviewers-list nil nil
                                      (jterk/concat-with-separator default-reviewers ","))))))
  (let ((branch (car (magit-log-read-revs))))
    (magit-run-git-async
     "push"
     (if reviewers
         (concat "--receive-pack=git receive-pack "
                 (mapconcat (lambda (s) (concat "--reviewer=" s)) reviewers " ")))
     "-v" "origin" (concat "HEAD:refs/for/" branch))))

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
   (define-key org-mode-map (kbd "C-c s") 'jterk/org-insert-src-block)
   (define-key org-mode-map (kbd "C-c t") 'jterk/org-yank-sql-table)))

(require 'org-table)
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

;; Make scratch buffers for different modes
(mapc (lambda (mode)
        (let* ((buffer-name (concat "*" (symbol-name mode) "-scratch*"))
               (buffer (find-file-noselect (concat my-home "tmp/" buffer-name))))
          (with-current-buffer buffer
            (funcall mode))))
      (list 'org-mode 'markdown-mode))

(require 'shyp)

(use-package web-mode
  :ensure t
  :mode "\\.ejs\\'")

(use-package htmlize :ensure t)
(use-package markdown-mode :ensure t)
(use-package ess :ensure t)
(use-package slime :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (R . t)))

(setq inferior-lisp-program "sbcl")
(setq nginx-indent-level 2)
(global-hl-line-mode)

(add-hook 'eww-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; eshell
(require 'eshell)
(require 'em-smart)
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

(use-package coffee-mode
  :ensure t
  :bind ("C-c C-C" . shyp/coffee-compile)
  :commands (coffee-repl)
  :config
  (add-hook 'coffee-mode-hook
            (lambda ()
              ;; Disable prompting for the compile-command
              (setq fill-column 120)
              (set (make-local-variable 'compilation-read-command) nil)
              (set (make-local-variable 'compile-command)
                   (shyp/make-compile-command)))))

(use-package json-mode :ensure t)

(use-package restclient
  :ensure t)

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

(provide '.emacs)

;;; .emacs ends here
