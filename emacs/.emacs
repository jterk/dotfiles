;;; package --- Summary -*- lexical-binding: t -*-
;;
;; File: .emacs
;;
;;; Commentary:
;;; Code:

;;; TODO
;; [ ] org mode

;;; Startup/UI Tuning

;; Remove startup wait time.
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; Use more RAM etc, particularly for LSP mode
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Don't show the startup message.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message "jterk"
      initial-scratch-message nil)

;;; Packages
(require 'package
         :config
         (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

;;; Appearances _are_ important

;; Turn off the GUI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Colors
(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-dark t))

;; Font
(defvar jterk/frame-font "Menlo 12")
(if (x-list-fonts jterk/frame-font)
    (progn
      (set-frame-font jterk/frame-font)
      (add-to-list 'default-frame-alist `(font . ,jterk/frame-font))))

(column-number-mode t)

;; Show line numbers for modes that inherit from prog-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;; key bind crutches

;; Make it harder to quit emacs.
(global-unset-key (kbd "s-q"))

;; Don't show the font menu.
(global-unset-key (kbd "s-t"))

;; Unbind kill-buffer
(global-unset-key (kbd "s-k"))

;; Unbind suspend-frame
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Printing is rarely what I want
(global-unset-key (kbd "s-p"))

;; Key Bindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key "\C-xu" 'browse-url-at-point)

;;; Utilities

;; Get exec path from shell env
(use-package exec-path-from-shell
  :unless (eq system-type 'windows-nt)
  :ensure t
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (add-to-list `exec-path-from-shell-variables "SHELL")
  (exec-path-from-shell-initialize))

;; Magit
(use-package magit
  :ensure t)

;; Get URLs for files in github (and other) repos
(use-package browse-at-remote
  :ensure t)

;; Org
(use-package org
  :config
  (setq org-startup-indented t))

(use-package org-roam
  :ensure t
  :after 'org)

(use-package verb
  :ensure t
  :after 'org)

;; Vertico completion https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :config
  (vertico-mode))

;; corfu completion popups https://github.com/minad/corfu
(use-package corfu
  :ensure t
  :config
  (global-corfu-mode)
  :custom
  (corfu-auto t))

;; Orderless https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

;; Consult https://github.com/minad/consult
(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; Marginalia https://github.com/minad/marginalia
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Maybe embark/embark-consult? https://github.com/oantolin/embark

(use-package flymake-popon
  :ensure t
  :config
  (add-hook 'flymake-mode-hook 'flymake-popon-mode))

;; ag
(use-package ag
  :ensure t)

;; GPT shell
(use-package chatgpt-shell
  :ensure t)

;; restclient
(use-package restclient
  :ensure t)

;;; Documentation

(setq Info-additional-directory-list (list "~/docs/info"))

;;; Languages

;; Tree sitter
(setq treesit-language-source-alist
      '((go "https://github.com/tree-sitter/tree-sitter-go" "master" "src")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod" "main" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json" "master" "src")
        (php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))

(setq major-mode-remap-alist
      '((go-mode . go-ts-mode)
        (json-mode json-ts-mode)
        (php-mode . php-ts-mode)
	      (python-mode . python-ts-mode)
	      (tsx-mode . tsx-ts-mode)
	      (typescript-mode . typescript-ts-mode)))

;; bazel
(use-package bazel
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("'\\BUILD.in\\'" . bazel-mode)))

;; docker files
(use-package dockerfile-mode
  :ensure t)

;; eglot for LSP
(use-package eglot
  :ensure t)

;; Go
(use-package go-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(add-hook 'go-ts-mode-hook 'eglot-ensure)

;; JSON
(use-package json-mode
  :ensure t)

;; PHP
(use-package php-mode
  :ensure t)

;; TODO Not on melpa/elpa needs alternative install
;; (use-package php-ts-mode
;;  :ensure t)

;; Plant UML
(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-output-type "png")
  (setq plantuml-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.uml$" . plantuml-mode))
  )

;; Protocol Buffers
(use-package protobuf-mode
  :ensure t)

;; Python
;; Use python-mode for pystachio files
(add-to-list 'auto-mode-alist '("\\.pyst$" . python-mode))
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp" "-vv" "--log-file" "/Users/jterk/tmp/pylsp.log")))
;; (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp")))

;; Automatically detect the right python environment
(use-package pet
  :ensure t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; Rust
(use-package rustic
  :ensure t
  :config
  (setq rustic-lsp-client 'eglot))

;; TOML
(use-package toml-mode
  :ensure t)

;; Typescript
(use-package typescript-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-mode))
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure))

;; YAML
(use-package yaml-mode
  :ensure t)

;; Caddyfiles
(use-package caddyfile-mode
  :ensure t)

;;; Formatting
;; Show trailing whitespace
(setq-default show-trailing-whitespace 't)

;; Tabs -> 2 spaces
(setq c-basic-offset 2)
(setq css-indent-offset 2)
(setq go-ts-mode-indent-offset 2)
(setq js-indent-level 2)
(setq python-indent-offset 4)
(setq typescript-indent-level 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 100)

;; No line truncation!
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows t)

;;; Bookkeeping
;; Consider replacing with no-littering https://github.com/emacscollective/no-littering
;; Keep various files in ~/tmp.
(use-package no-littering
  :ensure t
  :config
  (no-littering-theme-backups))

;; effectively disable customization by shunting it to an unused file
(if (not (file-directory-p "~/tmp"))
    (make-directory "~/tmp"))
(setq custom-file "~/tmp/custom.el")

;; Don't make me type 'yes'. Ever.
;;(defalias 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;; Emacs server
(setenv "EDITOR" "emacsclient")
(server-start)

;;; Shells
(defun jterk/hide-trailing-whitespace-hook ()
  "Hook to hide trailing whitespace"
  (setq-local show-trailing-whitespace nil))

(use-package vterm
  :unless (eq system-type 'windows-nt)
  :ensure t
  :config
  (setq vterm-shell (getenv "SHELL"))
  (add-hook 'vterm-mode-hook 'jterk/hide-trailing-whitespace-hook))

(use-package eshell
  :config
  (add-hook 'eshell-mode-hook 'jterk/hide-trailing-whitespace-hook))

;;; Context specific
(add-to-list 'load-path (concat (getenv "HOME") "/emacs"))
(use-package context-init
  :if (file-exists-p (concat (getenv "HOME") "/emacs/context-init.el")))

