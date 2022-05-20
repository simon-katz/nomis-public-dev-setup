;;;; nomis-loccur.el --- loccur tailoring ---  -*- lexical-binding: t -*-

(require 'loccur)

(set-face-attribute 'loccur-face
                    nil
                    :foreground "grey10"
                    :background (cl-case 1
                                  (1 "findHighlightColor")
                                  (2 "LightGoldenrod1")
                                  (3 "yellow1")))

(defun nomis/init-occur-mode ()
  ;; This `toggle-truncate-lines` seemed like a good idea at first, but using
  ;; `next-error` it causes the list window to scroll in a way that means you
  ;; lose your bearings.
  ;; (unless truncate-lines (toggle-truncate-lines))
  )

(add-hook 'occur-mode-hook 'nomis/init-occur-mode)

(provide 'nomis-loccur)
