;;;; nomis-loccur.el --- loccur tailoring ---  -*- lexical-binding: t -*-

(require 'loccur)

(set-face-attribute 'loccur-face
                    nil
                    :foreground "grey10"
                    :background (case 1
                                  (1 "findHighlightColor")
                                  (2 "LightGoldenrod1")
                                  (3 "yellow1")))

(defun nomis/init-occur-mode ()
  (unless truncate-lines
    (toggle-truncate-lines)))

(add-hook 'occur-mode-hook 'nomis/init-occur-mode)

(provide 'nomis-loccur)
