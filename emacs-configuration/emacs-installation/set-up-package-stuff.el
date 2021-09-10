(require 'package)

(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("gnu"          . 5)
        ;; ("melpa-stable" . 10)
        ("melpa"        . 100)))

(package-initialize)
