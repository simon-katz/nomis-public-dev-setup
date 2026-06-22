;;; nomis-key-chord.el --- key-chord tailoring  -*- lexical-binding: t; -*-

;;; Code:

(require 'key-chord)
(key-chord-mode 1)

(setopt key-chord-one-key-delay 0.3)

(setopt key-chord-typing-detection
        ;; I find this is essential.
        ;; Without it:
        ;; - Initially key chords work -- eg "qq" wprks.
        ;; - If I type "q", then wait, I get a "q" (as expected). (This is with
        ;;   my key chords in `nomis-tree-key-bindings`.)
        ;; - Subsequently If I type the key chord "qq" again, it'\s not treated
        ;;   as a key chord. This state persists for quite some time, then
        ;;   resets to allow key chords again.
        t)

;;; End

(provide 'nomis-key-chord)
