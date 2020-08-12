;;;; Code

(setq gc-cons-threshold 100000000)

;; a bug fixed in 26.3, until then keep the below line
(when (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; enable native compilation when available
(when (and (fboundp 'native-comp-available-p)
	       (native-comp-available-p))
  ;; (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
  (setq native-comp-async-report-warnings-errors nil)
  (setq comp-deferred-compilation t)
  (setq package-native-compile t))

;; disable prelude-theme
(setq prelude-theme nil)

;; minimalistic ui, like no line numbers globally by default
(setq prelude-minimalistic-ui t)

;; prefer utf-8
(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)

(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq locale-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
