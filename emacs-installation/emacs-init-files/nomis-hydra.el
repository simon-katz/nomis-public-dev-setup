;;;; Init stuff -- nomis/hydra

;;;; ___________________________________________________________________________

(require 'hydra)

(cl-defmacro define-nomis/hydra (name &key
                                      name-as-string
                                      key
                                      vars
                                      init-form
                                      cancel-form
                                      quit-form
                                      hydra-heads)
  (declare (indent 1))
  ;; The hacking with `key` and binding it to `.../body`
  ;; is to get the hints to appear when the command is first activated.

  (mmt-with-gensyms (initialised-p
                     cancel-fun-name
                     quit-fun-name)
   
    `(progn

       ,@(mapcar (lambda (var) `(defvar ,var))
                 vars)
      
       (defvar ,initialised-p nil)
      
       (defun ,cancel-fun-name ()
         (interactive)
         ,cancel-form)
      
       (defun ,quit-fun-name ()
         (interactive)
         ,quit-form)
      
       (defhydra ,name
         (global-map ,key
                     :pre (unless ,initialised-p
                            ,init-form
                            (setq ,initialised-p t))
                     :post (setq ,initialised-p nil))
         ,name-as-string
         ,@hydra-heads
         ("<escape>" ,cancel-fun-name "Cancel" :exit t)
         ("<return>" ,quit-fun-name   "Quit"   :exit t)
         ("q"        ,quit-fun-name   "Quit"   :exit t))
      
       (define-key global-map (kbd ,key)
         ',(intern (concat (symbol-name name)
                           "/body"))))))

;;;; ___________________________________________________________________________

(provide 'nomis-hydra)
