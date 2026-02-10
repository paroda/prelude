;;;
;;; consult-config.el --- Provide configuration for Consult

;;; Code:

(require 'consult)

;; C-c key bindings
(global-set-key (kbd "C-c h") #'consult-history)
(global-set-key (kbd "C-c m") #'consult-mode-command)
(global-set-key (kbd "C-c k") #'consult-kmacro)

;; C-x key bindings
(global-set-key (kbd "C-x M-:") #'consult-complex-command)     ;; orig. repeat-command
(global-set-key (kbd "C-x b") #'consult-buffer)                ;; orig. switch-to-buffer
(global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
(global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
(global-set-key (kbd "C-x r b") #'consult-bookmark)            ;; orig. bookmark-jump
(global-set-key (kbd "C-x p b") #'consult-project-buffer)      ;; orig. project-switch-to-buffer

;; custom M-# bindings for fast register access
(global-set-key (kbd "M-#") #'consult-register-load)
(global-set-key (kbd "M-'") #'consult-register-store)        ;; orig. abbrev-prefix-mark (unrelated)
(global-set-key (kbd "C-M-#") #'consult-register)

;; other custom bindings
(global-set-key (kbd "M-y") #'consult-yank-pop)

;; M-g bindings (goto map)
(global-set-key (kbd "M-g e") #'consult-compile-error)
(global-set-key (kbd "M-g f") #'consult-flymake)
(global-set-key (kbd "M-g g") #'consult-goto-line)
(global-set-key (kbd "M-g M-g") #'consult-goto-line)
(global-set-key (kbd "M-g o") #'consult-outline)
(global-set-key (kbd "M-g m") #'consult-mark)
(global-set-key (kbd "M-g k") #'consult-global-mark)
(global-set-key (kbd "M-g i") #'consult-imenu)
(global-set-key (kbd "M-g I") #'consult-imenu-multi)

;; C-c c bindings
(global-set-key (kbd "C-c c d") #'consult-find)
(global-set-key (kbd "C-c c D") #'consult-locate)
(global-set-key (kbd "C-c c g") #'consult-grep)
(global-set-key (kbd "C-c c G") #'consult-git-grep)
(global-set-key (kbd "C-c c r") #'consult-ripgrep)
(global-set-key (kbd "C-c c l") #'consult-line)
(global-set-key (kbd "C-c c L") #'consult-line-multi)
(global-set-key (kbd "C-c c m") #'consult-multi-occur)
(global-set-key (kbd "C-c c k") #'consult-keep-lines)
(global-set-key (kbd "C-c c u") #'consult-focus-lines)

;; Isearch integration
(global-set-key (kbd "C-c c s") #'consult-isearch-history)
(define-key isearch-mode-map (kbd "M-e") #'consult-isearch-history)   ;; orig. isearch-edit-string
(define-key isearch-mode-map (kbd "M-s e") #'consult-isearch-history) ;; orig. isearch-edit-string
(define-key isearch-mode-map (kbd "M-s l") #'consult-line)            ;; needed by consult-line to detect isearch
(define-key isearch-mode-map (kbd "M-s L") #'consult-line-multi)      ;; needed by consult-line to detect isearch

;; Minibuffer history
(define-key minibuffer-local-map (kbd "M-s") #'consult-history)  ;; orig. next-matching-history-element
(define-key minibuffer-local-map (kbd "M-r") #'consult-history)  ;; orig. previous-matching-history-element

;; Optionally configure the register formatting. This improves the register
;; preview for `consult-register', `consult-register-load',
;; `consult-register-store' and the Emacs built-ins.
(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)

;; Optionally tweak the register preview window.
;; This adds thin lines, sorting and hides the mode line of the window.
(advice-add #'register-preview :override #'consult-register-window)

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; Optionally configure preview. The default value
;; is 'any, such that any key triggers the preview.
;; (setq consult-preview-key 'any)
;; (setq consult-preview-key (kbd "M-."))
;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
;; For some commands and buffer sources it is useful to configure the
;; :preview-key on a per-command basis using the `consult-customize' macro.
;;
;; NOTE: this would disable the automatic preview for those commands for which
;;       a diffrent :preview-key is specified other than 'any
;;
;; (consult-customize
;;  consult-theme
;;  :preview-key '(:debounce 0.2 any)
;;  consult-ripgrep consult-git-grep consult-grep
;;  consult-bookmark consult-recent-file consult-xref
;;  consult--source-bookmark consult--source-recent-file
;;  consult--source-project-recent-file
;;  :preview-key (kbd "M-."))

;; Optionally configure the narrowing key.
;; Both < and C-+ work reasonably well.
(setq consult-narrow-key "<") ;; (kbd "C-+")

;; Optionally make narrowing help available in the minibuffer.
;; You may want to use `embark-prefix-help-command' or which-key instead.
;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

;; By default `consult-project-function' uses `project-root' from project.el.
;; Optionally configure a different project root function.
;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
(autoload 'projectile-project-root "projectile")
(setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))

;;; FLYCHECK
(require 'consult-flycheck)

;; Optionally add the `consult-flycheck' command
(define-key flycheck-command-map (kbd "!") #'consult-flycheck)

;;; fd
(defun consult-fd (&optional dir initial) ;; NOTE: require fd be installed
  (interactive "P")
  (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
    (consult-find dir initial)))

(global-set-key (kbd "C-c c F") 'consult-fd)

;;; toggle preview
(defvar-local consult-toggle-preview-orig nil)

(defun consult-toggle-preview ()
  "Command to enable/disable preview."
  (interactive)
  (if consult-toggle-preview-orig
      (setq consult--preview-function consult-toggle-preview-orig
            consult-toggle-preview-orig nil)
    (setq consult-toggle-preview-orig consult--preview-function
          consult--preview-function #'ignore)))

(define-key vertico-map (kbd "M-P") #'consult-toggle-preview)

;;;

(setq consult-locate-args  "locate --ignore-case --existing --regexp")

;;;

(provide 'consult-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; consult-config.el ends here
