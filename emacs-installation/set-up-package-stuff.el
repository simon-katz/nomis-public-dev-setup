(require 'package)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(;; ("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 100)))

(package-initialize)
