;;;; nomis-highlight-indentation.el --- nomis highlight-indentation tailoring ---  -*- lexical-binding: t -*-

(defun nomis/setup-highlight-indentation-mode ()
  ;; From https://github.com/antonj/Highlight-Indentation-for-Emacs
  (set-face-background 'highlight-indentation-face "grey90"))

(defun nomis/setup-highlight-indentation-current-column-mode ()
  ;; From https://github.com/antonj/Highlight-Indentation-for-Emacs
  (set-face-background 'highlight-indentation-current-column-face "grey60"))

(add-hook 'highlight-indentation-mode-hook
          'nomis/setup-highlight-indentation-mode)

(add-hook 'highlight-indentation-current-column-mode-hook
          'nomis/setup-highlight-indentation-current-column-mode)

(provide 'nomis-highlight-indentation)
