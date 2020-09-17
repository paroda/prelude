;;;;
;; a bug fixed in 26.3, until then keep the below line
(when (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; prefer utf-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; disable prelude-theme
(setq prelude-theme nil)
