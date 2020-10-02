;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; temp issue with https
;; (setq
;;  package-archives
;; '(("gnu" . "http://elpa.gnu.org/packages/")
;;   ("melpa" . "https://melpa.org/packages/")))

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path (expand-file-name "modules" prelude-personal-dir))

;; install my packages
(defvar my-packages
  '(org
    org-bullets
    ob-http
    markdown-mode
    graphviz-dot-mode
    plantuml-mode
    gnuplot

    paredit
    edn ;; needed for other clojure package
    cider
    clojure-mode-extra-font-locking
    flycheck
    flycheck-pos-tip
    flycheck-joker
    flycheck-clj-kondo

    darkokai-theme
    doom-themes
    doom-modeline
    highlight-symbol
    all-the-icons

    treemacs
    treemacs-projectile
    treemacs-icons-dired
    treemacs-magit

    fish-mode
    eterm-256color
    magit-gitflow
    perspective
    helm-rg
    persistent-scratch
    counsel))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; enable auto root mode open
(crux-reopen-as-root-mode)

;; no need for ~ files when editing
(setq create-lockfiles nil)

;; disable flyspell
(setq prelude-flyspell nil)

;; disable auto save of prelude
(setq prelude-auto-save nil)
(super-save-mode -1)
(setq super-save-remote-files nil)

;; Show line numbers
;; (if (version<= "26.0.50" emacs-version)
;;     (global-display-line-numbers-mode)
;;   (global-linum-mode))
(setq nlinum-highlight-current-line t)

;;;;;;;;; darkokai theme ;;;;;;;;;;;;;
;;
;; (require 'darkokai-theme)
;; (setq darkokai-mode-line-padding 1)
;; (load-theme 'darkokai t)
;;
;; (custom-theme-set-faces
;;  'darkokai
;;  '(org-block-begin-line ;; the line delimiting the begin of source blocks
;;    ((t (:foreground "#666" :background "#333" :extend t))))
;;  '(org-block ;; the source block background
;;    ((t (:foreground "#FFFFEA" :background "#000" :extend t))))
;;  '(org-block-end-line ;; the line delimiting the end of source blocks.
;;    ((t (:foreground "#666" :background "#333" :extend t)))))
;;
;; (setq org-emphasis-alist
;;       '(("*" (bold :foreground "Orange"))
;;         ("/" (italic :foreground "light blue"))
;;         ("_" (underline))
;;         ("=" (:foreground "green" :family "Mono"))
;;         ("~" (:foreground "deep sky blue"))
;;         ("+" (:strike-through t))))

;;;;;;;;;;; doom theme ;;;;;;;;;;;;

(require 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-one t)
;; (load-theme 'doom-sourcerer)
;; (load-theme 'doom-nord t)
;; (load-theme 'doom-moonlight t)
;; (load-theme 'doom-opera t)

;; highlight matching parentheses or braces when cusror is on one
(set-face-attribute 'sp-show-pair-match-face nil :foreground "white" :background "#2257A0")

;; popups
(set-face-attribute 'tooltip nil :background "#334455")
(set-face-attribute 'company-tooltip nil :background "#283644")

;; enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; enable custom treemacs theme
(setq doom-themes-treemacs-theme "doom-colors")
(doom-themes-treemacs-config)

;; fix and improve org mode native fontification
(doom-themes-org-config)

;;;;;;;;;; global key binding ;;;;;;;;;;

;; hot key for switching window
(global-set-key (kbd "M-p") 'ace-window)

;; short cut for vc refresh state
(global-set-key (kbd "C-x v 0") 'vc-refresh-state)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; shows a list of buffers
;; (global-set-key (kbd "C-x C-b") 'ibuffer) ;; switched to persp-ibuffer


;; configure comment tool
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(define-key flyspell-mode-map (kbd "C-;") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable git flow
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; Syntax highlighting
(require 'highlight-symbol)
(global-set-key (kbd "C-.") 'highlight-symbol-at-point)
(define-key flyspell-mode-map (kbd "C-.") nil)

;; hydra-ibuffer menu
(require 'hydra-ibuffer)
(define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)
(add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)

(require 'smartparens)
(smartparens-global-mode)

(require 'paredit)
(require 'cider)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; (require 'clj-refactor)
;; (add-hook 'clojure-mode-hook
;;           (lambda ()
;;             (clj-refactor-mode 1)
;;             (yas-minor-mode 1) ; for adding require/use/import statements
;;             ;; insert keybinding setup here
;;             ;; NOTE: this choice leaves cider-macroexpand-1 unbound
;;             (cljr-add-keybindings-with-prefix "C-c C-m")))

;; enable pretty lambda (replace fn keyword with greek letter)
(require 'clojure-pretty-lambda)
(add-hook 'clojure-mode-hook 'clojure-pretty-lambda-mode)
(add-hook 'cider-repl-mode-hook 'clojure-pretty-lambda-mode)

;; enable cider repl pprint using fipp
(setq cider-print-fn 'fipp)
(setq cider-repl-use-pretty-printing t)

;; add short cut for cider-repl-clear-buffer
(define-key cider-repl-mode-map (kbd "C-c SPC") 'cider-repl-clear-buffer)

;; setup flycheck linters
(require 'flycheck)
(require 'flycheck-pos-tip)
(require 'flycheck-joker)
(require 'flycheck-clj-kondo)

(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

(dolist (checkers '((clj-kondo-clj . clojure-joker)
                    (clj-kondo-cljs . clojurescript-joker)
                    (clj-kondo-cljc . clojure-joker)
                    (clj-kondo-edn . edn-joker)))
  (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode)

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(add-hook 'cider-mode-hook
          (lambda ()
            (setq next-error-function #'flycheck-next-error-function)))

;; cider repl mode hide line numbers
;; (add-hook 'cider-repl-mode
;;           (lambda ()
;;             (linum-mode -1)
;;             (display-line-numbers-mode -1)))

;; cider mode enable history file
(setq cider-repl-history-file "~/.cider-repl-history")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; graphviz

(require 'graphviz-dot-mode)
(setq graphviz-dot-indent-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plantuml

(require 'plantuml-mode)
(setq plantuml-server-url "http://www.plantuml.com/plantuml")
(setq plant-uml-jar-path (expand-file-name "~/sdk/plantuml.jar"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode config

(require 'org)
(require 'org-bullets)
(require 'ob-http)
(require 'cider)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (clojure . t)
   (js . t)
   (http . t)
   (ditaa . t)
   (plantuml . t)
   (dot . t)
   (gnuplot . t)))

;; specify clojure backend to use in org-mode
(setq org-babel-clojure-backend 'cider)
(setq org-babel-clojure-sync-nrepl-timeout nil)

;; useful keybindings when using clojure in org-mode
(org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)
(org-defkey org-mode-map "\C-x\C-d" 'cider-doc)

(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
(setq org-plantuml-jar-path (expand-file-name "~/sdk/plantuml.jar"))
(setq org-confirm-babel-evaluate nil)
(setq org-babel-results-keyword "results") ;; make label lowercase

(defun org-babel-plantuml-make-body (body params)
  (let ((assignments (org-babel-variable-assignments:plantuml params)))
    (org-babel-expand-body:generic body params assignments)))


;; force display of images after execute
(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (condition-case nil
                (org-display-inline-images)
              (error nil)))
          'append)

;; turn on visual-line-mode for org-mode
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (linum-mode -1)
;;             (display-line-numbers-mode -1)
;;             (whitespace-mode -1)
;;             (turn-on-visual-line-mode)))
;;
;; (setq org-hide-emphasis-markers t)
;; (setq org-src-tab-acts-natively t)
;; (setq org-edit-src-content-indentation 0)
;; (setq org-src-fontify-natively t)
;; (setq org-src-preserve-indentation nil)
;; (setq org-pretty-entities t)
;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq org-latex-packages-alist '(("margin=2cm" "geometry" nil)))

;; (set-face-attribute 'org-meta-line nil
;;                     :height 0.8
;;                     :slant 'normal
;;                     ;; :foreground "black"
;;                     :weight 'light)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAMP
(customize-set-variable
 'tramp-ssh-controlmaster-options
 (concat
  "-o ControlPath=/tmp/ssh-master-%%r@%%h:%%p "
  "-o ControlMaster=auto "
  "-o ControlPersist=yes"))

;; Tramp speed up by disabling some
(require 'editorconfig)
(editorconfig-mode -1)

;; counsel
(require 'counsel)
(global-set-key (kbd "C-x 8 RET") 'counsel-unicode-char)

(defadvice projectile-project-root (around ignore-remote first activate)
  (unless (file-remote-p default-directory) ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SHA1

(require 'sha1)
(defun sha1-region (start end)
  "Computes the SHA1 hash of currently selected region."
  (interactive "r")
  (let ((hash (sha1 (buffer-substring start end))))
    (deactivate-mark)
    (message "Region sha1 hash: %s, (C-y to insert)," hash)
    (kill-new hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  ;; set arrow keys in isearch
  ;; left/right is backward/forward
  ;; up/down is history
  ;; press Return to exit
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)
  (define-key minibuffer-local-isearch-map (kbd "<left>")
    'isearch-reverse-exit-minibuffer)
  (define-key minibuffer-local-isearch-map (kbd "<right>")
    'isearch-forward-exit-minibuffer))
(global-set-key (kbd "C-S-s") 'isearch-forward-symbol-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hideshow setup
(load-library "hideshow")
(progn
  (define-key hs-minor-mode-map (kbd "C-t") 'hs-toggle-hiding)
  (define-key hs-minor-mode-map (kbd "C-S-t") 'hs-hide-all))
(add-hook 'clojure-mode-hook 'hs-minor-mode)
(add-hook 'cider-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; persistent scratch buffer

(require 'persistent-scratch)
(persistent-scratch-setup-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable doom-modeline

(require 'doom-modeline)
(doom-modeline-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; treemacs setup

(require 'treemacs)
(require 'treemacs-projectile)
(require 'treemacs-magit)

(progn
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 t
        treemacs-file-event-delay              1000 ; 5000
        treemacs-file-follow-delay             0.2
        treemacs-follow-after-init             t
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         t
        treemacs-max-git-entries               5000
        treemacs-missing-project-action        'ask
        treemacs-no-png-images                 nil
        treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                      'left
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             nil
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-asc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-width                         35)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (define-key global-map (kbd "M-0")       'treemacs-select-window)
  (define-key global-map (kbd "C-x t 1")   'treemacs-delete-other-windows)
  (define-key global-map (kbd "C-x t t")   'treemacs)
  (define-key global-map (kbd "C-x t B")   'treemacs-bookmark)
  (define-key global-map (kbd "C-x t C-t") 'treemacs-find-file)
  (define-key global-map (kbd "C-x t M-t") 'treemacs-find-tag)

  (define-key treemacs-mode-map [mouse-1] 'treemacs-single-click-expand-action))

;; perspective

(require 'perspective)
(persp-mode)
(global-set-key (kbd "C-x C-b") 'persp-ibuffer)

;; integrate treemacs to follow perspective
(require 'treemacs-perspective)

(setq frame-title-format
      '(""
        (:eval (or (persp-current-name) "Prelude"))
        " - "
        (:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eterm-256color)
(add-hook 'term-mode-hook #'eterm-256color-mode)

(defvar vterm-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'vterm--self-insert)
    (define-key map (kbd "C-e") 'vterm--self-insert)
    map)
  "Keymap for VTerm minor mode.")

;; must define it before loading vterm
(define-minor-mode vterm-minor-mode
  "Minor mode for VTerm"
  :lighter " vtl"
  :keymap vterm-minor-mode-map
  :global nil)

;; setup vterm
(when (package-installed-p 'vterm)
  (require 'vterm)
  ;; * you need to manually install vterm and fish and configure them.
  ;;   this will not auto install them. it only adds some settings,
  ;;   which are ignored in the absence of required components.
  (setq vterm-term-environment-variable "eterm-256color")
  (setq vterm-shell (or (executable-find "fish")
                        (executable-find "bash")))
  (setq vterm-buffer-name-string "vterm:%s")
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-copy-exclude-prompt t)

  (add-hook 'vterm-mode-hook 'vterm-minor-mode)
  (define-key vterm-copy-mode-map (kbd "C-a") 'vterm-beginning-of-line)
  (define-key vterm-copy-mode-map (kbd "C-e") 'vterm-end-of-line)
  (define-key prelude-mode-map (kbd "C-c t") 'vterm)
  (define-key prelude-mode-map (kbd "C-c T") 'vterm-other-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI Only!!

(when (display-graphic-p)

  (require 'all-the-icons) ;; load icons
  (require 'treemacs-icons-dired)
  (treemacs-icons-dired-mode)

  (setq frame-resize-pixelwise t) ;; fix full-size issue on Xming
  (setq default-frame-alist  '((left . 50)
                               (top . 50)
                               (width . 90)  ; chars
                               (height . 30) ; lines
                               (vertical-scroll-bars . nil)
                               (horizontal-scroll-bars . nil)
                               (font . "InputMono-11")
                               ))
  (menu-bar-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
