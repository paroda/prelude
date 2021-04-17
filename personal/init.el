;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold 100000000)

;; temp issue with https
;; (setq
;;  package-archives
;; '(("gnu" . "http://elpa.gnu.org/packages/")
;;   ("melpa" . "https://melpa.org/packages/")))

;; simplify yes/no prompt
(fset 'yes-or-no-p 'y-or-n-p)

;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path (expand-file-name "modules" prelude-personal-dir))

;; install my packages
(defvar my-packages
  '(org
    org-superstar
    ob-http
    markdown-mode
    graphviz-dot-mode
    plantuml-mode
    gnuplot
    pandoc-mode

    selectrum
    consult
    consult-flycheck
    marginalia
    embark-consult
    company-box

    paredit
    edn ;; needed for other clojure package
    cider
    clojure-mode-extra-font-locking
    flycheck
    flycheck-pos-tip
    flycheck-joker
    flycheck-clj-kondo

    js2-refactor
    json-mode

    darkokai-theme
    doom-themes
    doom-modeline
    highlight-symbol
    all-the-icons
    unicode-fonts

    treemacs
    treemacs-projectile
    treemacs-icons-dired
    treemacs-magit

    yasnippet
    yasnippet-snippets

    fish-mode
    eterm-256color
    magit-gitflow
    perspective
    ripgrep
    persistent-scratch))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; override prelude

;; disable flyspell
;; (setq prelude-flyspell nil)

;; disable auto save of prelude
(setq prelude-auto-save nil)
(super-save-mode -1)
(setq super-save-remote-files nil)

;;;;;;;;;;; doom theme ;;;;;;;;;;;;

(require 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-one t)

;; highlight matching parentheses or braces when cusror is on one
(set-face-attribute 'sp-show-pair-match-face nil
                    :foreground "white" :background "#2257A0")

;; popups
(set-face-attribute 'tooltip nil :background "#334455")
(set-face-attribute 'company-tooltip nil :background "#283644")

;; enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; fix and improve org mode native fontification
(doom-themes-org-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable doom-modeline

(require 'doom-modeline)
(add-hook 'after-init-hook #'doom-modeline-mode)
(add-hook 'calc-mode-hook
          (lambda ()
            (setq mode-line-format '("%e" mode-line-buffer-identification
                                     (:eval (doom-modeline-format--main))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun switch-theme ()
  "An interactive function to cleanly switch themes"
  (interactive)
  (disable-theme (intern (car (mapcar #'symbol-name custom-enabled-themes))))
  (call-interactively #'load-theme))

;; prettify things
(global-prettify-symbols-mode 1)

(defun add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols. See https://unicodelookup.com for more."
  (setq prettify-symbols-alist
        '(
          ("lambda" . ?λ)
          ("->" . ?⟶)
          ("<=" . ?≤)
          (">=" . ?≥)
          ("#+BEGIN_SRC" . ?✎)
          ("#+END_SRC"    . ?□)
          ("#+BEGIN_EXAMPLE" . (?ℰ (Br . Bl) ?⇒)) ;; ℰ⇒
          ("#+END_EXAMPLE"    . ?⇐)               ;; ⇐
          ("#+BEGIN_QUOTE" . (?𝒬 (Br . Bl) ?⇒))   ;; 𝒬⇒
          ("#+END_QUOTE"    . ?⇐)                 ;; ⇐
          )))

(add-hook 'org-mode-hook 'add-pretty-lambda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default
 help-window-select t         ;; Focus new help window when opened
 debug-on-error t
 window-combination-resize t  ;; Resize window proportionally
 history-delete-duplicates t)

;; dired hotkey
(define-key dired-mode-map (kbd "r") 'dired-kill-subdir)

;; hotkey for vc refresh state
(global-set-key (kbd "C-x v 0") 'vc-refresh-state)

(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; selectrum
(require 'consult-selectrum-config)
(global-set-key (kbd "C-x C-z") #'selectrum-repeat)

;; company-box
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Ask before killing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; no need for ~ files when editing
(setq create-lockfiles nil)

;; enable auto root mode open
(crux-reopen-as-root-mode)

(require 'smartparens)
(smartparens-global-mode)
(smartparens-global-strict-mode)

;; keep scratch buffer on restart
(require 'persistent-scratch)
(persistent-scratch-setup-default)

;; enable git flow
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; Syntax highlighting
(require 'highlight-symbol)
(require 'flyspell)
(global-set-key (kbd "C-.") 'highlight-symbol-at-point)
(define-key flyspell-mode-map (kbd "C-.") nil)

;; hideshow setup
(require 'hideshow)
(progn
  (define-key hs-minor-mode-map (kbd "C-t") 'hs-toggle-hiding)
  (define-key hs-minor-mode-map (kbd "C-S-t") 'hs-hide-all))
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; use paredit for clojure and elisp
(require 'paredit)
(require 'cider)
(dolist (m '(emacs-lisp-mode-hook
             clojure-mode-hook clojurescript-mode-hook cider-repl-mode-hook))
  (add-hook m 'paredit-mode))

;; configure comment tool
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(define-key flyspell-mode-map (kbd "C-;") nil)

;; Show line numbers
(require 'nlinum)
(setq nlinum-highlight-current-line t)
(global-nlinum-mode -1)
;; (add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascript extra

(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")

(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure extra

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; enable pretty lambda (replace fn keyword with greek letter)
(require 'clojure-pretty-lambda)
(dolist (m '(clojure-mode-hook clojurescript-mode-hook cider-repl-mode-hook))
  (add-hook m 'clojure-pretty-lambda-mode))

(require 'cider)
;; enable cider repl pprint using fipp
(setq cider-print-fn 'fipp)
(setq cider-repl-use-pretty-printing t)
;; cider mode enable history file
(setq cider-repl-history-file "~/.cider-repl-history")
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
;; yasnippet

(require 'yasnippet)
(require 'yasnippet-snippets)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode configuration

(require 'org)
(require 'org-superstar)
(require 'org-indent)
(require 'ob-http)

(setq org-ellipsis " ▾"
      org-hide-emphasis-markers t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0
      org-hide-block-startup nil
      org-src-preserve-indentation nil
      org-startup-folded 'content
      org-cycle-separator-lines 1
      org-adapt-indentation nil
      org-indent-indentation-per-level 2)

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode)
            (org-superstar-mode)
            (variable-pitch-mode 1)
            (auto-fill-mode 0)
            (set-fill-column 100)
            (whitespace-mode 0)
            (visual-line-mode 1)))

(setq org-superstar-remove-leading-stars t
      org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil
                      :font "Cantarell" :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil
                    :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-superstar-item nil :inherit 'fixed-pitch)

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

(setq org-latex-packages-alist '(("margin=2cm" "geometry" nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pandoc
(require 'pandoc-mode)
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

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
;; perspective

(require 'perspective)
(persp-mode)
(global-set-key (kbd "C-x K") 'persp-kill-buffer*)
(global-set-key (kbd "C-x C-b") 'persp-ibuffer)
(setq frame-title-format
      '(""
        (:eval (or (persp-current-name) "Prelude"))
        " - "
        (:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; treemacs setup

(require 'treemacs)
(require 'treemacs-projectile)
(require 'treemacs-magit)
(require 'treemacs-perspective)

(require 'my-treemacs-all-the-icons)
(treemacs-load-theme "my-all-the-icons")

(progn
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-directory-name-transformer    #'identity
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 t
        treemacs-file-event-delay              1000 ; 5000
        treemacs-file-extension-regex          treemacs-last-period-regex-value
        treemacs-file-follow-delay             0.2
        treemacs-file-name-transformer         #'identity
        treemacs-follow-after-init             t
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         t ; nil
        treemacs-max-git-entries               5000
        treemacs-missing-project-action        'ask
        treemacs-move-forward-on-expand        nil
        treemacs-no-png-images                 nil
        treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                      'left
        treemacs-read-string-input             'from-minibuffer ; 'from-child-frame
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             nil ; t
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-asc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-user-mode-line-format         nil
        treemacs-user-header-line-format       nil
        treemacs-width                         35
        treemacs-workspace-switch-cleanup      nil)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eterm-256color)
(add-hook 'term-mode-hook #'eterm-256color-mode)

(defvar vterm-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'vterm-send-C-a)
    (define-key map (kbd "C-e") 'vterm-send-C-e)
    (define-key map (kbd "C-c C-y") 'vterm-send-C-y)
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
  (set-face-attribute 'vterm-color-green nil :foreground "#009900")

  (add-hook 'vterm-mode-hook
            (lambda ()
              (vterm-minor-mode 1)
              (set (make-local-variable 'buffer-face-mode-face)
                   '(:family "Fira Code Nerd Font Mono" :height 110))
              (buffer-face-mode t)))
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
  (setq default-frame-alist '(;; Explicit size/position is useful only for
                              ;; GTK builds over X11 to overcome resize issue.
                              ;; Not required for LUCID builds
                              ;; (left . 50)
                              ;; (top . 50)
                              ;; (width . 90)  ; chars
                              ;; (height . 30) ; lines
                              (vertical-scroll-bars . nil)
                              (horizontal-scroll-bars . nil)
                              ;; (font . "InputMono-11")
                              ;; (font . "FiraCode-11")
                              ;; (font . "DejaVuSansMono-11")
                              ))
  (menu-bar-mode -1)


  ;; set font
  (set-face-attribute 'default nil :font "Fira Code Nerd Font" :height 110)
  ;; fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Nerd Font" :height 110)
  ;; variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular)

  ;; unicode fonts remap
  ;;  (require 'unicode-fonts)
  ;;  (setq unicode-fonts-skip-font-groups '(low-quality-gyphs))
  ;;  (mapc
  ;;   (lambda (block-name)
  ;;     (let* ((old-font "Apple Color Emoji")
  ;;            (new-font "Noto Color Emoji")
  ;;            (block-idx (cl-position-if
  ;;                        (lambda (i) (string-equal (car i) block-name))
  ;;                        unicode-fonts-block-font-mapping))
  ;;            (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
  ;;            (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
  ;;       (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
  ;;             `(,updated-block))))
  ;;   '("Dingbats"
  ;;     "Emoticons"
  ;;     "Miscellaneous Symbols and Pictographs"
  ;;     "Transport and Map Symbols"))
  ;;  (unicode-fonts-setup)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold 50000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
