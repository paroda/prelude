;;
;;; treemacs-perspective.el --- Provide an integration
;;                              between treemacs and perspective
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar my-perspective-treemacs-ws nil)

(require 'treemacs)
(require 'perspective)

(add-hook 'persp-before-switch-hook
          (lambda ()
            ;; (message "persp-before-switch")
            (let ((p (persp-current-name))
                  (w (treemacs-workspace->name (treemacs-current-workspace)))
                  (tr (if (--filter (string-match "Treemacs-Scoped-Buffer"
                                                  (buffer-name (window-buffer it)))
                                    (window-list))
                          t))
                  (wnd (car (window-list))))
              ;; (message "persp-before-switch: %s %s" p w)
              (->> my-perspective-treemacs-ws
                   (--reject (string= (car it) p))
                   (cons (list p w tr wnd))
                   (setq my-perspective-treemacs-ws)))))

(add-hook 'persp-switch-hook
          (lambda ()
            ;; (message "persp-switch")
            (let* ((p (persp-current-name))
                   (v (cdr (--first (string= (car it) p)
                                    my-perspective-treemacs-ws)))
                   (w (car v))
                   (tr (car (cdr v)))
                   (wnd (car (cdr (cdr v)))))
              ;; (message "persp-switch: %s %s" p w)
              (if w
                  (let ((sel (--first (string= (treemacs-workspace->name it) w)
                                      treemacs--workspaces)))
                    (when sel
                      (setf (treemacs-current-workspace) sel)
                      (treemacs--invalidate-buffer-project-cache)
                      (treemacs--rerender-after-workspace-change)
                      (run-hooks 'treemacs-switch-workspace-hook))))
              (if tr (treemacs))
              (if (--first (eq it wnd) (window-list))
                  (select-window wnd)))))

;;;;

(provide 'treemacs-perspective)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; treemacs-perspective.el ends here
