
(with-eval-after-load 'gnutls
  (add-to-list 'gnutls-trustfiles (expand-file-name "~/.emacs.d/cacert.pem"))
  (setq tls-program "openssl s_client -connect %h:%p -servername %h -no_ssl2 -no_ssl3 -ign_eof -CAfile %t"))

(require 'gnutls)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(require 'package)
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(when (boundp 'package-native-compile)
  (setq package-native-compile 1))

;; ;; Disable native comp error buffer
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors nil))

(defconst private-dir  (expand-file-name "private" user-emacs-directory))
(defconst temp-dir (format "%s/cache" private-dir)
  "Hostname-based elisp temp directories")

;; Core settings
;; UTF-8 please
(set-charset-priority 'unicode)
(set-language-environment "UTF-8")     ; pretty
(set-default-coding-systems 'utf-8)    ; pretty
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

;; Emacs customizations
(setq-default confirm-kill-emacs                  'y-or-n-p
      confirm-nonexistent-file-or-buffer  t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point                 t
      require-final-newline               t
      visible-bell                        nil
      ring-bell-function                  'ignore
      custom-file                         "~/.emacs.d/.custom.el"
      ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
      minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

      ;; Disable non selected window highlight
      cursor-in-non-selected-windows     nil
      highlight-nonselected-windows      nil
      ;; PATH
      indent-tabs-mode                   nil
      tab-always-indent                  'complete
      tab-stop-list                      (number-sequence 4 120 4)
      column-number-mode                 t
      delete-selection-mode              t
      inhibit-startup-message            t
      gc-cons-threshold                  80000000
	  read-process-output-max            (* 1024 1024)
      buffers-menu-max-size              30
      fringes-outside-margins            t
      frame-title-format                 "%b (%f)"
      scroll-margin                      3
      scroll-conservatively              101
      scroll-up-aggressively             0.01
      scroll-down-aggressively           0.01
      scroll-preserve-screen-position    t
      auto-window-vscroll                nil
	  inhibit-compacting-font-caches     t
      use-package-always-ensure          t)

(setq-default tab-width 4)

;; Bookmarks
(setq
 ;; persistent bookmarks
 bookmark-save-flag                      t
 bookmark-default-file              (concat temp-dir "/bookmarks"))

;; Backups enabled, use nil to disable
(setq
 history-length                     10
 backup-inhibited                   t
 make-backup-files                  nil
 auto-save-default                  nil
 auto-save-list-file-name           (concat temp-dir "/autosave")
 create-lockfiles                   nil
 backup-directory-alist            `((".*" . ,(concat temp-dir "/backup/")))
 auto-save-file-name-transforms    `((".*" ,(concat temp-dir "/auto-save-list/") t)))

(unless (file-exists-p (concat temp-dir "/auto-save-list"))
  (make-directory (concat temp-dir "/auto-save-list") :parents))

(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)


;; Disable toolbar & menubar
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (  fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Server
(use-package server
  :ensure nil
  :config
  (setq server-name "server")
  (server-force-delete)
  (server-start))

(use-package dired-x
  :ensure nil
  :config
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..+$")))

;;spell checking
(use-package ispell
  :defer 15
  :ensure nil
  :config
  (setq ispell-program-name (executable-find  "hunspell"))
  (setq ispell-really-hunspell t)
  (setq
   ispell-local-dictionary-alist `(
								   ("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)
								   ("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB") nil utf-8)
								   ("spanish" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "es") t utf-8)
								   )))
;; Desktop
(use-package desktop
  :ensure nil
  :config
  (desktop-save-mode)
  (setq-default desktop-save 'if-exists))

;; Paren-mode
(show-paren-mode 1)
(setq show-paren-style 'mixed)
;; smartparents??
(electric-pair-mode)
;;inputs
(setq-default default-input-method 'spanish-postfix)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(load custom-file)

;; for encryption
(setq epg-pinentry-mode 'loopback)
;;
(defun guess-shell ()
  "Guess which shell I'm using"
  (cond ((executable-find "fish") (executable-find "fish"))
       ((executable-find "bash") (executable-find "bash"))
        ((executable-find "zsh") (executable-find "zsh"))))
(setq-default shell-file-name (guess-shell))
(provide 'base)
;;; base ends here
