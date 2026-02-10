;; my-treemacs-all-the-icons.el --- override a few icons in treemacs

;;; Code:

(require 'all-the-icons)
(require 'treemacs)

(treemacs-create-theme "my-all-the-icons"
  :extends "Default"
  :config
  (progn
    (treemacs-create-icon :icon (format "%s " (all-the-icons-octicon "repo" :height 1.4 :v-adjust -0.1
                                                                     :face 'treemacs-term-node-face))
                          :extensions (root-closed root-open)
                          :fallback (propertize "* " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :icon (format "%s " (all-the-icons-faicon "folder-open" :height 1.1 :v-adjust -0.1
                                                                    :face 'all-the-icons-purple))
                          :extensions (dir-open)
                          :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :icon (format "%s " (all-the-icons-faicon "folder" :height 1.1 :v-adjust -0.1
                                                                    :face 'all-the-icons-purple))
                          :extensions (dir-closed)
                          :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    )
  )

(provide 'my-treemacs-all-the-icons)

;;; my-treemacs-all-the-icons.el ends here
