(use-package all-the-icons
    :straight t)

;;(require 'uwu-theme)

;; (enable-theme 'uwu)

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :config
  (color-theme-sanityinc-tomorrow-eighties))

(defun select-font ()
  (interactive)
  (cond
   ((find-font (font-spec :name "Cascadia Code"))
	(set-frame-font "Cascadia Code-13")
	(setq default-frame-alist '((font . "Cascadia Code-13"))))
   ((find-font (font-spec :name "Fira Code"))
	(set-frame-font "Fira Code-12")
	(setq default-frame-alist '((font . "Fira Code-12"))))
   ((find-font (font-spec :name "Inconsolata"))
	(set-frame-font "inconsolata-11")
	(setq default-frame-alist '((font . "inconsolata-11"))))
   ((find-font (font-spec :name "Lucida Console"))
	(set-frame-font "Lucida Console-11")
	(setq default-frame-alist '((font . "Lucida Console-11"))))
   ((find-font (font-spec :name "courier"))
	(set-frame-font "courier-11")
	(setq default-frame-alist '((font . "courier-11"))))))

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
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))
(provide 'base-theme)
