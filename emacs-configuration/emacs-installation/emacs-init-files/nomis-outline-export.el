;;; nomis-outline-export.el --- Export outlines  -*- lexical-binding: t; -*-

(defvar -nomis/clojure-outline-to-html-script
  (expand-file-name "clojure-outline-to-html.bb"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the Babashka script used by `nomis/clojure-outline-to-html'.")

(defun nomis/outline/export-clojure-to-html ()
  "Export the current Clojure outline file to HTML.
Output is written to a `_no-commit_' subdirectory next to the input file.
Delegates all work to the Babashka script at
`-nomis/clojure-outline-to-html-script'."
  (interactive)
  (let* ((input-file  (buffer-file-name))
         (output-dir  (expand-file-name "_no-commit_"
                                        (file-name-directory input-file))))
    (make-directory output-dir t)
    (shell-command
     (format "bb %s %s %s"
             (shell-quote-argument -nomis/clojure-outline-to-html-script)
             (shell-quote-argument input-file)
             (shell-quote-argument output-dir)))
    (message "Exported to %s" output-dir)))

(provide 'nomis-outline-export)
