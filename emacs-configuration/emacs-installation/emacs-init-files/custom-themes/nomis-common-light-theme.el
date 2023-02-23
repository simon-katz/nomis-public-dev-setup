(deftheme nomis-common-light
  "Created 2023-02-23.")

(custom-theme-set-faces
 'nomis-common-light

 `(lsp-ui-sideline-global
   ((t
     :background "LemonChiffon1")))

 `(lsp-ui-sideline-code-action
   ((t
     :foreground "gray95"
     :background "springgreen4")))

 `(lsp-face-highlight-textual
   ;; c.f. tailoring of `nomis/idle-highlight-muted`.
   ((t
     :background "#ffff00"
     :foreground "black")))

 `(hc-hard-space
   ((t
     :background "grey60")))

 `(terraform--resource-name-face
   ((t
     :foreground "Blue"))))


(provide-theme 'nomis-common-light)
