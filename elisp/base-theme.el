(use-package all-the-icons
    :straight t)

;;(require 'uwu-theme)

;; (enable-theme 'uwu)

(use-package catppuccin-theme
  :straight t
  :config
  (load-theme 'catppuccin :no-confirm))

(defun select-font ()
  (interactive)
  (cond
   ((find-font (font-spec :name "Cascadia Code"))
	(set-frame-font "Cascadia Code-14")
	(setq default-frame-alist '((font . "Cascadia Code-14"))))
   ((find-font (font-spec :name "Fira Code"))
	(set-frame-font "Fira Code-14")
	(setq default-frame-alist '((font . "Fira Code-14"))))
   ((find-font (font-spec :name "Inconsolata"))
	(set-frame-font "inconsolata-14")
	(setq default-frame-alist '((font . "inconsolata-14"))))
   ((find-font (font-spec :name "Lucida Console"))
	(set-frame-font "Lucida Console-14")
	(setq default-frame-alist '((font . "Lucida Console-14"))))
   ((find-font (font-spec :name "courier"))
	(set-frame-font "courier-14")
	(setq default-frame-alist '((font . "courier-14"))))))

(add-hook 'focus-in-hook #'select-font)
(add-hook 'server-after-make-frame-functions 'select-font)
;(add-hook 'server-after-make-frame-functions 'nano-dark)
;(add-hook 'focus-in-hook 'nano-dark)


(use-package mood-line
  :straight t
  :config
  (mood-line-mode))


(use-package ligature
  :straight t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (use-package emacs-emojify
    :straight t
    :bind
    ("C-c ." . emojify-insert-emoji)
    :config
    (setq emojify-display-style 'unicode)
    (setq emojify-emoji-styles '(unicode)))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))
(provide 'base-theme)
