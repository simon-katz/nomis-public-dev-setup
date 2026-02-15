;;; nomis-mark -- -*- lexical-binding: t -*-

;;; Code

(advice-add 'pop-to-mark-command
            :around
            (lambda (orig-fun &rest args)
              ;; Inspired by
              ;; https://endlessparentheses.com/faster-pop-to-mark-command.html
              ;; Repeatedly pop until the cursor moves. Try up to 10 times.
              (let ((p (point)))
                (dotimes (i 10)
                  (when (= p (point))
                    (apply orig-fun args)))))
            '((name . norg/add-level-info)))

;;; End

(provide 'nomis-mark)
