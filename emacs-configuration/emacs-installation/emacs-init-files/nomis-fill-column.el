;;;; nomis-fill-column ---  -*- lexical-binding: t -*-

(require 'nomis-right-margin-column)

(setq-default fill-column nomis/right-margin-column)

(setq nomis/right-margin-column-color "#cc99cc")

(provide 'nomis-fill-column)
