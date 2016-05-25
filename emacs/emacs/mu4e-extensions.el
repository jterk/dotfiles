;;; mu4-extensions --- extensions to the mu4e email client -*- lexical-binding: t -*-
;;
;; File: mu4e-extensions.el
;; Copyright 2014 Jason Terk <jason@goterkyourself.com>
;;
;; Personal extensions to mu4e.
;;
;; TODO: Rationalize "my-mu4e" vs "mu4e" prefixes.
;;
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))
(use-package smtpmail-async)

(defvar my-mu4e-account-alist
  '()
  "The set of configured mu4e accounts. Each entry in the list is
of the form (VAR VALUE IGNORE). When used, the VAR is set to
VALUE. If IGNORE is non-nil VAR will not be set when the entry is
processed by mu4e-apply-account-vars. Setting APPLY to t is
useful, for example, to avoid overwriting the `mu4e-trash-folder'
variable.")

(defun mu4e-apply-account-vars (account-vars)
  "Apply the settings specified in an entry in
`my-mu4e-account-alist'."
  (mapc #'(lambda (var)
            (if (not (cddr var))
                (set (car var) (cadr var))))
        account-vars))

;; Always wrap, by default
(defun my-mu4e-configure-wrapping ()
  "Setup line wrapping in mu4e view mode."
  (visual-line-mode))

(defun my-mu4e-disable-trailing-whitespace-hook ()
  "Disable font-lock for trailing whitespace."
  (setq show-trailing-whitespace nil))

(defun my-mu4e-get-folder (message folder)
  "Get the account specific version of FOLDER based on the
account to which MESSAGE corresponds. FOLDER should be a quoted
m4u folder name, e.g. 'mu4e-trash-folder."
  (let ((account (my-mu4e-get-message-account-name message)))
    (message account)
    (cadr (assoc folder (cdr (assoc account my-mu4e-account-alist))))))

(defun my-mu4e-get-message-account-name (message)
  "Get the name of the account for a given message."
  (if message
      (let ((maildir (mu4e-message-field message :maildir)))
        (string-match "/\\(.*?\\)/" maildir)
        (match-string 1 maildir))))

;; Apply account settings when composing
(defun my-mu4e-set-compose-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (my-mu4e-get-message-account-name mu4e-compose-parent-message)
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mu4e-apply-account-vars account-vars)
      (error "No email account found"))))

(defun my-mu4e-action-view-in-browser (msg)
  "View the body of the message in a web browser.
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
(defvar my-async-smtpmail-send-it-timer nil
  "The last sent message. A send can be canceled with
`my-async-smtpmail-send-it-cancel'.")

(defvar my-async-smtpmail-send-it-wait "30 sec"
  "The amount of time to wait before sending an email. This
should be a relative time.")

(defun my-async-smtpmail-send-it ()
  "Extension of `async-smtpmail-send-it' (from async.el) that
permits cancelling a message send. The queued message will be
sent at the time specified by `my-async-smtpmail-send-it-wait'."
  (let ((to          (message-field-value "To"))
        (buf-content (buffer-substring-no-properties
                      (point-min) (point-max))))
    (setq my-async-smtpmail-send-it-timer
          (run-at-time
           my-async-smtpmail-send-it-wait nil
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
  "Cancel the last queued (via. `my-async-smtpmail-send-it')
email."
  (interactive)
  (cancel-timer my-async-smtpmail-send-it-timer)
  (setq my-async-smtpmail-send-it-timer nil))

(provide 'mu4e-extensions)
;;; mu4e-extensions.el ends here
