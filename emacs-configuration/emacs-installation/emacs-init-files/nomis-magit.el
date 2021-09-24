;;;; Init stuff -- nomis-magit  -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________

;;;; See https://emacs.stackexchange.com/questions/35701/magit-sets-auto-revert-mode-annoying
;;;;
;;;; Don't globally set auto-revert-mode (that's very rude!).

(magit-auto-revert-mode 0)

;;;; TODO: I'm not sure, but maybe Magit no longer sets auto-revert-mode
;;;;       for buffers, and maybe it just reverts them.
;;;;       Try this out.

;;;; ___________________________________________________________________________

(provide 'nomis-magit)
