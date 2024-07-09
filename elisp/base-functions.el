;; Add your custom functions here

;; (defun something
;;    (do-something))
(defun buffer-cd ()
  "Cambia el directorio actual hacia donde apunta el buffer."
  (interactive)
  (when (stringp buffer-file-name)
    (cd (file-name-directory (buffer-file-name)))))

;; Tomado de dired-single
(defun my-dired-init ()
  "Configura `dired-mode' con varias opciones."
  ;;(with-eval-after-load 'dired  (require 'dired-filetype-face))
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

(defun buffer-list-filter ()
  "Like `buffer-list' but filtering some interesting buffers."
  )

;; Of course we can always use `kill-some-buffers'
(defun kob ()
  "Kill other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun reindent-buffer ()
  "Reindenta todo el buffer."
  (interactive)
  (indent-region (buffer-end -1) (buffer-end 1)))

(defun wg/kludge-gpg-agent ()
  "Setting GPG_TTY to get appropriate pinentry for each terminal or GUI frame."
  (if (display-graphic-p)
      (setenv "DISPLAY" (terminal-name))
    (progn (setenv "GPG_TTY" (terminal-name))
           (setenv "DISPLAY"))))

(defun set-proxy (proxy-url)
  "Setea el proxy a PROXY-URL."
  (interactive "sProxy url: ")
  (setq url-proxy-services `(("http" . ,proxy-url)
                             ("https" . ,proxy-url))))

(defun clear-recentf-list ()
  "Clears recentf list."
  (interactive)
  (when (boundp 'recentf-list)
    (setq recentf-list nil)))

(defsubst my-trim-left (string)
  "Remove leading whitespace from STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string))

;; A little helper for yasnippets
(defmacro with-comment (&rest text)
  "Wrap TEXT into comments."
  `(my-trim-left (concat comment-start ,@text comment-end)))

(defun yaml-lint ()
  "Ejecuta yamllint en el buffer actual."
  (interactive)
  (compile (format "%s %s %s" "yamllint" "-f parsable"
                   (expand-file-name buffer-file-name))))

(defun insert-date-time ()
  "Insert current date-time string in full ISO 8601 format.
Example: 2010-11-29T23:23:35-08:00"
  (interactive)
  (insert
   (concat
    (format-time-string "%Y-%m-%dT%T")
    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
     (format-time-string "%z")))))

(add-to-list 'flycheck-checkers 'yamllint)
(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

(defun xml-find-file-hook ()
  "Hook para activar where en XML."
  (when (derived-mode-p 'nxml-mode)
    (which-function-mode t)
    (add-hook 'which-func-functions 'nxml-where t t)))

(defun scratch ()
  (interactive)
  (switch-to-buffer (generate-new-buffer (generate-new-buffer-name "*scratch*"))))

(defun noccur-dired (regexp &optional nlines)
  "Perform `multi-occur' with REGEXP in all dired marked files.
When called with a prefix argument NLINES, display NLINES lines before and after."
  (interactive (occur-read-primary-args))
  (multi-occur (mapcar #'find-file (dired-get-marked-files)) regexp nlines))

(defun noccur-project (regexp &optional nlines)
  "Perform `multi-occur' in the current project files."
  (interactive (occur-read-primary-args))
  (let* ((directory (read-directory-name "Search in directory: "))
         (files (if (and directory (not (string= directory (projectile-project-root))))
                    (projectile-files-in-project-directory directory)
                  (projectile-current-project-files)))
         (buffers (mapcar #'find-file
                          (mapcar #'(lambda (file)
                                      (expand-file-name file (projectile-project-root)))
                                  files))))
    (multi-occur buffers regexp nlines)))

(eval-after-load 'flycheck
  (flycheck-define-checker json-python-json-windows
  "A JSON syntax checker using Python json.tool module.

See URL `https://docs.python.org/3.5/library/json.html#command-line-interface'."
  :command ("py" "-m" "json.tool" source
            ;; Send the pretty-printed output to the null device
            null-device)
  :error-patterns
  ((error line-start
          (message) ": line " line " column " column
          ;; Ignore the rest of the line which shows the char position.
          (one-or-more not-newline)
          line-end))
  :modes json-mode
  ;; The JSON parser chokes if the buffer is empty and has no JSON inside
  :predicate (lambda () (not (flycheck-buffer-empty-p)))))


(provide 'base-functions)
