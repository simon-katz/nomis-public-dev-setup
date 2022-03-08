;;; kaocha-runner-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "kaocha-runner" "kaocha-runner.el" (0 0 0 0))
;;; Generated autoloads from kaocha-runner.el

(autoload 'kaocha-runner-hide-windows "kaocha-runner" "\
Hide all windows that kaocha has opened." t nil)

(autoload 'kaocha-runner-run-tests "kaocha-runner" "\
Run tests in the current namespace.
If prefix argument TEST-ID? is present ask user for a test-id to run.

\(fn &optional TEST-ID\\=\\?)" t nil)

(autoload 'kaocha-runner-run-test-at-point "kaocha-runner" "\
Run the test at point in the current namespace." t nil)

(autoload 'kaocha-runner-run-all-tests "kaocha-runner" "\
Run all tests." t nil)

(autoload 'kaocha-runner-show-warnings "kaocha-runner" "\
Display warnings from the last kaocha test run.
Prefix argument SWITCH-TO-BUFFER? opens a separate window.

\(fn &optional SWITCH-TO-BUFFER\\=\\?)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kaocha-runner" '("kaocha-runner-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; kaocha-runner-autoloads.el ends here
