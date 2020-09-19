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
                  (w (treemacs-workspace->name (treemacs-current-workspace))))
              ;; (message "persp-before-switch: %s %s" p w)
              (->> my-perspective-treemacs-ws
                   (--reject (string= (car it) p))
                   (cons (list p w))
                   (setq my-perspective-treemacs-ws)))))

(add-hook 'persp-activated-hook
          (lambda ()
            ;; (message "persp-activated")
            (let* ((p (persp-current-name))
                   (w (car (cdr (--first (string= (car it) p)
                                         my-perspective-treemacs-ws)))))
              ;; (message "persp-activated: %s %s" p w)
              (if w
                  (let* ((ws (--map (cons (treemacs-workspace->name it) it)
                                    treemacs--workspaces))
                         (sel (cdr (--first (string= (car it) w) ws))))
                    (setf (treemacs-current-workspace) sel)
                    (treemacs--invalidate-buffer-project-cache)
                    (treemacs--rerender-after-workspace-change)
                    (run-hooks 'treemacs-switch-workspace-hook))))))

;;;;

(provide 'treemacs-perspective)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; treemacs-perspective.el ends here
