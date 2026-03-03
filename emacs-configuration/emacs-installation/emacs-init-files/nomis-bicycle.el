;;; nomis-bicycle.el --- Nomis bicycle  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'bicycle)
(require 'nomis-msg)
(require 'nomis-tree-outline-2)

;;;; Thoughts on bicycle

;; `bicycle` combines `outline` and `hideshow`.
;;
;; It seemed like a nice idea, but I'm not sure I want to combine outline and
;; hide-show.
;;
;; Now we have (or will have) `nomis-tree`, which makes some of
;; `bicycle` unnecessary.

;;;; Bicycle tailoring

;;;;; Provide feedback in `bicycle-cycle-local`

(defvar *-nomis/bicycle/in-bicycle-cycle-local?* nil)

(advice-add 'bicycle-cycle-local
            :around
            (lambda (orig-fun &rest args)
              (let* ((*-nomis/bicycle/in-bicycle-cycle-local?* t))
                (apply orig-fun args)))
            '((name . -nomis/bicycle/bicycle-feedback)))

(advice-add 'outline-show-subtree
            :around
            (lambda (orig-fun &rest args)
              (when *-nomis/bicycle/in-bicycle-cycle-local?*
                (nomis/tree/outline/pulse-current-section))
              (apply orig-fun args))
            '((name . -nomis/bicycle/bicycle-feedback)))

;;;;; Provide feedback in `bicycle-cycle-global`

(defvar *-nomis/bicycle/in-bicycle-cycle-global?* nil)

(advice-add 'bicycle-cycle-global
            :around
            (lambda (orig-fun &rest args)
              (let* ((*-nomis/bicycle/in-bicycle-cycle-global?* t))
                (apply orig-fun args)))
            '((name . -nomis/bicycle/bicycle-feedback)))

(advice-add 'outline-show-all
            :around
            (lambda (orig-fun &rest args)
              (when *-nomis/bicycle/in-bicycle-cycle-global?*
                (nomis/msg/pulse-buffer))
              (apply orig-fun args))
            '((name . -nomis/bicycle/bicycle-feedback)))

;;; End

(provide 'nomis-bicycle)
