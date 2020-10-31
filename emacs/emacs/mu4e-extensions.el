;;; mu4-extensions --- extensions to the mu4e email client -*- lexical-binding: t -*-
;;
;; File: mu4e-extensions.el
;; Copyright 2014-2017 Jason Terk <jason@goterkyourself.com>
;;
;; Personal extensions to mu4e.
;;
;;; Commentary:
;;; Code:
(defvar mu4e-account-alist
  '()
  "Indicates the set of configured mu4e accounts.

Each entry in the list is is a list whose entries take the
form (VAR VALUE IGNORE).  When used, the VAR is set to VALUE.  If
IGNORE is non-nil VAR will not be set when the entry is processed
by mu4e-apply-account-vars.  Setting APPLY to t is useful, for
example, to avoid overwriting the `mu4e-trash-folder'
variable (which is determined at runtime based on the trashed
message).

TODO: Switch to using `mu4e-contexts' instead.

Example:

\(setq mu4e-account-alist
  '((\"Account1\"
     (mu4e-sent-folder \"/Account1/INBOX.Sent Items\")
     (mu4e-trash-folder \"/Account1/INBOX.Trash\" t))
    (\"Account2\"
     (mu4e-sent-folder \"/Account2/INBOX.Sent Items\")
     (mu4e-trash-folder \"/Account2/INBOX.Trash\" t))))")

(defvar mu4e-default-account nil
  "Indicates the default mu4e account.

When set this should be one of the top level keys from
`mu4e-account-alist'.")

(defun mu4e-apply-account-vars (account-vars)
  "Apply settings from `mu4e-account-alist'.

Each entry in ACCOUNT-VARS is inspected and, if IGNORE is not t,
the specified setting is applied using `set'."
  (mapc #'(lambda (var)
            (if (not (cddr var))
                (set (car var) (cadr var))))
        account-vars))

;; Always wrap, by default
(defun mu4e-configure-wrapping ()
  "Setup line wrapping in mu4e view mode."
  (visual-line-mode))

(defun mu4e-disable-trailing-whitespace-hook ()
  "Disable font-lock for trailing whitespace."
  (setq show-trailing-whitespace nil))

(defun mu4e-get-folder (message folder)
  "Find an account specific folder for MESSAGE.

FOLDER should be a quoted m4u folder name,
e.g. 'mu4e-trash-folder."
  (let ((account (mu4e-get-message-account-name message)))
    (message account)
    (cadr (assoc folder (cdr (assoc account mu4e-account-alist))))))

(defun mu4e-get-message-account-name (message)
  "Get the name of the account for MESSAGE."
  (if message
      (let ((maildir (mu4e-message-field message :maildir)))
        (string-match "/\\(.*?\\)/" maildir)
        (match-string 1 maildir))))

(defun mu4e-set-compose-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (mu4e-get-message-account-name mu4e-compose-parent-message)
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var)) mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) mu4e-account-alist)
                             nil t nil nil (caar mu4e-account-alist))))
         (account-vars (cdr (assoc account mu4e-account-alist))))
    (if account-vars
        (mu4e-apply-account-vars account-vars)
      (error "No email account found"))))

(defun jterk/mu4e-action-view-in-browser (msg)
  "View the body of MSG in a web browser.

You can influence the browser to use with the variable
`browse-url-generic-program'."
  (let* ((html (mu4e-message-field msg :body-html))
         (txt (mu4e-message-field msg :body-txt))
         (tmpfile (format "%s%x.html" temporary-file-directory (random t))))
    (unless (or html txt)
      (mu4e-error "No body part for this message"))
    (with-temp-buffer
      ;; simplistic -- but note that it's only an example...
      (insert (or html (concat "<pre>" txt "</pre>")))
      (write-file tmpfile)
      (browse-url (concat "file://" tmpfile)))))

;; mu4e Email Send Undo
(defvar jterk/async-smtpmail-send-it-timer nil
  "A timer for the last sent message.

A send can be canceled with `jterk/async-smtpmail-send-it-cancel'.")

(defvar jterk/async-smtpmail-send-it-wait "30 sec"
  "The amount of time to wait before sending an email.

This should be a relative time.")

(defun jterk/async-smtpmail-send-it ()
  "`async-smtpmail-send-it' that permits cancelling a send.

The queued message will be sent at the time specified by
`jterk/async-smtpmail-send-it-wait'."
  (let ((to          (message-field-value "To"))
        (buf-content (buffer-substring-no-properties
                      (point-min) (point-max))))
    (setq jterk/async-smtpmail-send-it-timer
          (run-at-time
           jterk/async-smtpmail-send-it-wait nil
           (lambda ()
             (message "Delivering message to %s..." to)
             (async-start
              `(lambda ()
                 (require 'smtpmail)
                 (with-temp-buffer
                   (insert ,buf-content)
                   (set-buffer-multibyte nil)
                   ;; Pass in the variable environment for smtpmail
                   ,(async-inject-variables
                     "\\`\\(smtpmail\\|\\(user-\\)?mail\\)-"
                     nil "\\`\\(mail-header-format-function\\|smtpmail-address-buffer\\|mail-mode-abbrev-table\\)")
                   (smtpmail-send-it)))
              `(lambda (&optional ignore)
                 (message "Delivering message to %s...done" ,to))))))))

(defun my-async-smtpmail-send-it-cancel ()
  "Cancel the last queued (via `my-async-smtpmail-send-it')
email."
  (interactive)
  (cancel-timer jterk/async-smtpmail-send-it-timer)
  (setq jterk/async-smtpmail-send-it-timer nil))

(defvar jterk/mu4e-update-interval nil
  "See `mu4e-update-interval'.")

(defvar jterk/mu4e-last-update-time nil
  "The time that `jterk/mu4e-update-timer-fn' last executed.")

(defvar jterk/mu4e-update-timer nil
  "The timer for `jterk/mu4e-update-timer-fn', if scheduled.")

(defun jterk/mu4e-update-timer-fn ()
  "Periodically invokes `mu4e-update-mail-and-index'.

This function tracks its last execution time (in
`jterk/mu4e-last-update-time') to ensure that it runs at most
once per every `mu4-update-interval'."
  (if (>= (- (float-time) (or jterk/mu4e-last-update-time 0)) jterk/mu4e-update-interval)
      (progn
        (setq jterk/mu4e-last-update-time (float-time))
        (mu4e-update-mail-and-index mu4e-index-update-in-background))))

(defun jterk/mu4e-wrapper (orig-fn &rest args)
  "Advice intended for use with `mu4e'.

Inhibits mu4e's background updates in favor of
`jterk/mu4e-update-timer-fn'."
    (apply orig-fn args)
    (if (and jterk/mu4e-update-interval (null jterk/mu4e-update-timer))
        (setq jterk/mu4e-update-timer
              (run-at-time 0 jterk/mu4e-update-interval 'jterk/mu4e-update-timer-fn))))

(defun jterk/mu4e-quit-wrapper ()
  "Advice intended for use with `mue4-quit'

Cancels `jterk/mu4e-update-timer-fn', if scheduled."
  (when jterk/mu4e-update-timer
    (cancel-timer jterk/mu4e-update-timer)
    (setq jterk/mu4e-update-timer nil)))

;; It seems that v. 1.4 of mu/mu4e broke this 
(defun jterk/mu4e-install-advice ()
  "Activates mu4e advice.

Installs `jterk/mu4e-wrapper' and `jterk/mu4e-quit-wrapper'."
  (advice-add 'mu4e :around #'jterk/mu4e-wrapper)
  (advice-add 'mu4e-quit :after-while #'jterk/mu4e-quit-wrapper))

(jterk/mu4e-install-advice)

(provide 'mu4e-extensions)
;;; mu4e-extensions.el ends here
