;;;; Init stuff -- Nomis GPG tailoring --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

;; Without this, gpg decryption fails. With this, it asks for the passphrase.
;; Ah; I think something is broken. The default is supposed to pop up a dialog.
;; The `loopback` value fakes a pinentry by "using inquiries back to the caller
;; to ask for a passphrase".
(setf epg-pinentry-mode 'loopback)

;;;; ___________________________________________________________________________

(provide 'nomis-gpg)
