;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold (* 100 1024 1024))

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
    ;; pandoc-mode

    selectrum
    consult
    consult-flycheck
    company-box
    company-prescient
    ibuffer-projectile
    marginalia
    embark
    embark-consult

    edn ;; needed for other clojure package
    cider
    clojure-mode-extra-font-locking
    flycheck
    flycheck-pos-tip
    flycheck-clj-kondo

    js2-refactor
    json-mode

    doom-themes
    doom-modeline
    highlight-symbol
    all-the-icons
    ;; unicode-fonts

    treemacs
    treemacs-projectile
    treemacs-icons-dired
    treemacs-magit

    yasnippet
    yasnippet-snippets

    ;; magit-gitflow
    ripgrep
    persistent-scratch))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; override prelude
(setq
 ;; prelude-flyspell nil ;; disable flyspell
 prelude-auto-save nil ;; disable auto save of prelude
 super-save-remote-files nil)

(super-save-mode -1)

(global-set-key (kbd "C-x p") project-prefix-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 confirm-kill-emacs 'y-or-n-p
 create-lockfiles nil
 eww-download-directory "~/Downloads/"
 whitespace-line-column 100)

(setq-default
 tab-width 4
 help-window-select t         ;; Focus new help window when opened
 debug-on-error t
 jit-lock-defer-time 0
 window-combination-resize t  ;; Resize window proportionally
 history-delete-duplicates t
 dired-kill-when-opening-new-dired-buffer t ;; reduce dired buffer littering
 ;; trying out
 redisplay-skip-fontification-on-input t
 describe-bindings-outline t)

;; window & buffer hotkey
(global-set-key (kbd "<f3>") 'winner-undo)
(global-set-key (kbd "<f4>") 'winner-redo)
(global-set-key (kbd "<f7>") 'other-window)
(global-set-key (kbd "<f8>") 'switch-to-prev-buffer)
(global-set-key (kbd "<f9>") 'switch-to-next-buffer)

;; editing hotkey
(global-set-key (kbd "M-O") 'crux-smart-open-line-above)
(global-set-key (kbd "M-RET") 'comment-indent-new-line)

;; hotkey for vc refresh state
(global-set-key (kbd "C-x v 0") 'vc-refresh-state)

(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; repurpose C-_ for contract region to complement C-= for expand region
(define-key undo-tree-map (kbd "C-_") nil)
(global-set-key (kbd "C-_") 'er/contract-region)

;; So Long mitigates slowness due to extremely long lines.
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; enable auto root mode open
(crux-reopen-as-root-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired - sort by grouping directories first

(setq dired-listing-switches "-al --group-directories-first")

;; dired hotkey
(define-key dired-mode-map (kbd "r") 'dired-kill-subdir)

(defun my-dired-show-disk-usage ()
  (interactive)
  (let* ((filename (dired-get-filename))
         (cmd (concat "du -h -d0 " filename)))
    (shell-command cmd)))
(define-key dired-mode-map (kbd "z") 'my-dired-show-disk-usage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; doom theme ;;;;;;;;;;;;

(require 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-one t)

;; highlight matching parentheses or braces when cusror is on one
;; set both the faces sp-show-pair-match-face and show-paren-match as they are at same priority level
;; and it is uncertain which will be applied. Another option is to somehow disable one.
(set-face-attribute 'sp-show-pair-match-face nil
                    :foreground "white" :background "#2257A0")
(set-face-attribute 'show-paren-match nil
                    :foreground "white" :background "#2257A0")

(set-face-attribute 'mode-line nil :box '(:line-width -1 :style released-button))
(set-face-attribute 'mode-line-inactive nil :box '(:line-width -1 :style pressed-button))

(set-face-attribute 'mode-line nil :background "#210")
(set-face-attribute 'mode-line-inactive nil :background "#012")

;; enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; fix and improve org mode native fontification
(doom-themes-org-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable doom-modeline

(require 'doom-modeline)
(setq doom-modeline-height 40
      doom-modeline-bar-width 8)
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
  "Use a pretty lambda symbol"
  (setq prettify-symbols-alist '(("lambda" . ?𝛌))))
(dolist (m '(lisp-mode-hook emacs-lisp-mode-hook))
  (add-hook m 'add-pretty-lambda))

(defun add-pretty-org ()
  "Make some word or string show as pretty Unicode symbols. See https://unicodelookup.com for more."
  (setq prettify-symbols-alist
        '(("lambda" . ?𝛌)
          ("->" . ?⟶)
          ("<=" . ?≤)
          (">=" . ?≥)
          ("#+BEGIN_SRC" . ?✎)
          ("#+begin_src" . ?✎)
          ("#+END_SRC"    . ?□)
          ("#+end_src"    . ?□)
          ("#+BEGIN_EXAMPLE" . (?ℰ (Br . Bl) ?⇒)) ;; ℰ⇒
          ("#+begin_example" . (?ℰ (Br . Bl) ?⇒)) ;; ℰ⇒
          ("#+END_EXAMPLE"    . ?⇐)
          ("#+end_example"    . ?⇐)
          ("#+BEGIN_QUOTE" . (?𝒬 (Br . Bl) ?⇒))   ;; 𝒬⇒
          ("#+begin_quote" . (?𝒬 (Br . Bl) ?⇒))   ;; 𝒬⇒
          ("#+END_QUOTE"    . ?⇐)
          ("#+end_quote"    . ?⇐))))
(add-hook 'org-mode-hook 'add-pretty-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; selectrum
(require 'prelude-selectrum)
(require 'consult-selectrum-config)
(global-set-key (kbd "C-x C-z") #'selectrum-repeat)

;; marginalia
(require 'marginalia)
(marginalia-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
(require 'prelude-company)
(require 'company-prescient)
(company-prescient-mode t)
(require 'company-box)
(global-set-key (kbd "M-TAB") #'company-complete)
(add-hook 'company-mode-hook 'company-box-mode)
(setf (alist-get 'left-fringe company-box-frame-parameters) 10)
(setf (alist-get 'right-fringe company-box-frame-parameters) 10)

;; popups
(set-face-attribute 'tooltip nil :background "#334455")
(set-face-attribute 'company-tooltip nil :background "#283644")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable ibuffer grouping as vcs
(require 'ibuffer-projectile)
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; consult - extras
(defun consult-fd (&optional dir initial) ;; NOTE: require fd be installed
  (interactive "P")
  (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
    (consult-find dir initial)))
(global-set-key (kbd "C-c c F") 'consult-fd)

(defvar-local consult-toggle-preview-orig nil)
(defun consult-toggle-preview ()
  "Command to enable/disable preview."
  (interactive)
  (if consult-toggle-preview-orig
      (setq consult--preview-function consult-toggle-preview-orig
            consult-toggle-preview-orig nil)
    (setq consult-toggle-preview-orig consult--preview-function
          consult--preview-function #'ignore)))
(define-key selectrum-minibuffer-map (kbd "M-P") #'consult-toggle-preview)

(setq consult-locate-args  "locate --ignore-case --existing --regexp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; embark - consult
(require 'embark)

(global-set-key (kbd "C-c C-.") 'embark-act)         ;; pick some comfortable binding
(global-set-key (kbd "C-c C-h B") 'embark-bindings) ;; alternative for `describe-bindings'
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

;; embark-consult
(require 'embark-consult)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  'isearch-forward-exit-minibuffer)
(global-set-key (kbd "C-S-s") 'isearch-forward-thing-at-point)
(setq isearch-allow-motion t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keep scratch buffer on restart
(require 'persistent-scratch)
(setq persistent-scratch-save-file "~/.emacs.d/.persistent/scratch")
(defun my-persistent-scratch-default-scratch-buffer-p ()
  (string-match-p "^\\*scratch\\*\\( (.+)\\)?$" (buffer-name)))
(setq persistent-scratch-scratch-buffer-p-function 'my-persistent-scratch-default-scratch-buffer-p)
(persistent-scratch-setup-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable git flow
;; (require 'magit-gitflow)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting
(require 'highlight-symbol)
(require 'flyspell)
(global-set-key (kbd "C-.") 'highlight-symbol-at-point)
(define-key flyspell-mode-map (kbd "C-.") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hideshow setup
(require 'hideshow)
(progn
  (define-key hs-minor-mode-map (kbd "C-t") 'hs-toggle-hiding)
  (define-key hs-minor-mode-map (kbd "C-S-t") 'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-S-a") 'hs-show-all))
(add-hook 'prog-mode-hook 'hs-minor-mode)

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
;; extra prelude modules
(require 'prelude-shell)
(require 'prelude-xml)
(require 'prelude-yaml)
(require 'prelude-emacs-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web (html/css/js)

(require 'prelude-web)
(require 'prelude-css)
(require 'prelude-js)

(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")

(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint
                        json-jsonlint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure extra

(require 'prelude-clojure)
(require 'cider)
;; enable cider repl pprint using fipp
(setq cider-print-fn 'fipp)
(setq cider-repl-use-pretty-printing t)
;; cider mode enable history file
(setq cider-repl-history-file "~/.cider-repl-history")
(setq cider-repl-history-size 1000)
;; set completion hotkey to use company
(define-key cider-mode-map (kbd "C-M-i") 'company-complete)

;; setup flycheck linters
(require 'flycheck)
(require 'flycheck-pos-tip)
(require 'flycheck-clj-kondo)

(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode)

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(add-hook 'cider-mode-hook
          (lambda ()
            (setq next-error-function #'flycheck-next-error-function)))

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; enable pretty lambda (replace fn keyword with greek letter)
(defun add-pretty-clojure ()
  "Use a pretty lambda symbol"
  (setq prettify-symbols-alist '(("fn" . ?𝛌))))
(dolist (m '(clojure-mode-hook clojurescript-mode-hook cider-repl-mode-hook))
  (add-hook m 'add-pretty-clojure))

;; hide all block on load
(add-hook 'clojure-mode-hook #'hs-hide-all)
(add-hook 'clojurec-mode-hook #'hs-hide-all)
(add-hook 'clojurescript-mode-hook #'hs-hide-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; graphviz

(require 'graphviz-dot-mode)
(setq graphviz-dot-indent-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plantuml

(require 'plantuml-mode)
(setq plantuml-server-url "http://www.plantuml.com/plantuml")
(setq plant-uml-jar-path (expand-file-name "~/.sdk/plantuml.jar"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet

(require 'yasnippet)
(require 'yasnippet-snippets)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode configuration

(require 'prelude-org)
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

;; setup node modules load path for org babel node
(setenv "NODE_PATH"
        (concat "./node_modules:"
                (getenv "HOME") "/my/org/node_modules:"
                (getenv "NODE_PATH")))

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
(setq org-plantuml-jar-path plant-uml-jar-path)
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; pandoc
;; (require 'pandoc-mode)
;; (add-hook 'markdown-mode-hook 'pandoc-mode)
;; (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

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
;; treemacs setup

(require 'treemacs)
(require 'treemacs-projectile)
(require 'treemacs-magit)

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

  (define-key global-map (kbd "<f6>")      'treemacs-select-window)
  (define-key global-map (kbd "M-0")       'treemacs-select-window)
  (define-key global-map (kbd "C-x t 1")   'treemacs-delete-other-windows)
  (define-key global-map (kbd "C-x t t")   'treemacs)
  (define-key global-map (kbd "C-x t B")   'treemacs-bookmark)
  (define-key global-map (kbd "C-x t C-t") 'treemacs-find-file)
  (define-key global-map (kbd "C-x t M-t") 'treemacs-find-tag)

  (define-key treemacs-mode-map [mouse-1] 'treemacs-single-click-expand-action))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vterm setup

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
  (setq vterm-term-environment-variable "xterm-256color")
  (setq vterm-shell (or (executable-find "fish")
                        (executable-find "bash")))
  (setq vterm-buffer-name-string "vterm:%s")
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-copy-exclude-prompt t)

  (add-hook 'vterm-mode-hook
            (lambda ()
              (vterm-minor-mode 1)
              (set (make-local-variable 'buffer-face-mode-face)
                   '(:family "Fira Code Nerd Font Mono" :height 120))
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
  (setq default-frame-alist '(;; (alpha . (90 . 75))
                              (vertical-scroll-bars . nil)
                              (horizontal-scroll-bars . nil)))

  ;; Font
  (when (member "FiraCode Nerd Font Mono" (font-family-list))
    ;; set font
    (set-face-attribute 'default nil :font "Fira Code Nerd Font Mono" :height 120)
    ;; fixed pitch face
    (set-face-attribute 'fixed-pitch nil :font "Fira Code Nerd Font Mono" :height 120))
  (when (member "Cantarell" (font-family-list))
    ;; variable pitch face
    (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 140 :weight 'regular))

  ;; unicode fonts remap
  ;; (require 'unicode-fonts)
  ;; (setq unicode-fonts-skip-font-groups '(low-quality-gyphs))
  ;; (mapc
  ;;  (lambda (block-name)
  ;;    (let* ((old-font "Apple Color Emoji")
  ;;           (new-font "Noto Color Emoji")
  ;;           (block-idx (cl-position-if
  ;;                       (lambda (i) (string-equal (car i) block-name))
  ;;                       unicode-fonts-block-font-mapping))
  ;;           (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
  ;;           (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
  ;;      (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
  ;;            `(,updated-block))))
  ;;  '("Dingbats"
  ;;    "Emoticons"
  ;;    "Miscellaneous Symbols and Pictographs"
  ;;    "Transport and Map Symbols"))
  ;; (unicode-fonts-setup)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; reduce size to shorten GC pause
  (setq gc-cons-threshold  (* 5 1024 1024)))

;; start server
;; (server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
