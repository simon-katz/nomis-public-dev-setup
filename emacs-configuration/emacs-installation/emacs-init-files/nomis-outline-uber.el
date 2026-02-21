;;; nomis-outline-uber -- -*- lexical-binding: t -*-

;;; To dos

;; TODO: There's lots of potential work here and its not straightforward. So for
;;       now do nothing apart from (a) mull things over, and (b) look at
;;       existing code.

;; TODO: The tables in `nomis-outline-key-bindings` summarise `norg`,
;;       `hide-show` and `nomis/outline`.

;; TODO: Have we decided that we don't want `bicycle`? (I think we have.)

;; TODO: I don't think I want code to be headings.
;;       - What would I lose if I remove code from `outline-regexp`?
;;       - We'd have to do that for all modes (all languages).
;;       - What about wanting to have my code be useful for other people?

;; TODO: We could integrate hide-show -- so eg `nomis/tree/tab` `:outline`
;;       method could do `nomis/hs/adjust/more` for children that are code.

;; TODO: Make `nomis/tree/set-step-n-levels-to-show`-style functionality
;;       available here.

;; TODO: Compare with `norg` commands.
;;       - Indefinite number of levels.
;;       - Behaviour when reaching last sibling
;;       - Implement more commands here.
;;       - Make a shared low-level API around org and outline, and
;;         make higher-level functions use that.
;;       - Hmmmm, it might be non-trivial. I see that `norg` uses a mix of
;;         `outline` and `org` functionality as its base layer. The `org`
;;         stuff won't be available for outlines.

;; TODO: The key bindings for hide-show will need to be made different (because
;;       they duplicate our `norg` key bindings, which we want for outline
;;       stuff). Maybe a prefix in front of the existing bindings. Oh, or maybe
;;       a modal UI.

;;; Code

;;;; Requires

(require 'a)
(require 'cl-lib)
(require 'dash)
(require 'nomis-popup)
(require 'nomis-scrolling)

;;;; Utilities

;;;;; Misc

(defun -nomis/outline-last-command ()
  (or (bound-and-true-p *nomis/smex/last-command*)
      last-command))

(defun -nomis/outline-ordinal (n)
  (cl-format nil "~a~a"
             n
             (let ((x (cl-format nil "~:r" n)))
               (subseq x (- (length x) 2)))))

;;;;; Simple outline wrappers

(defun -nomis/outline-on-heading? ()
  (outline-on-heading-p t))

(defun -nomis/outline-on-visible-heading? ()
  (outline-on-heading-p))

(defun -nomis/outline-back-to-heading? ()
  (outline-back-to-heading t))

(defun -nomis/outline-back-to-visible-heading? ()
  (outline-back-to-heading))

(defun -nomis/outline-up-heading (n)
  (outline-up-heading n t))

(defun -nomis/outline-up-visible-heading (n)
  (outline-up-heading n))

(defun -nomis/outline-at-beginning-of-heading? ()
  (and (bolp)
       (-nomis/outline-on-heading?)))

(defun -nomis/outline-on-top-level-heading? ()
  "Are we on a top-level heading?"
  ;; `(outline-level)` and `(funcall outline-level)` return weird numbers in
  ;; some modes. This, we hope, is bulletproof.
  (save-excursion
    (when (-nomis/outline-on-heading?)
      (let* ((opoint (point))
             (olevel (funcall outline-level)))
        (ignore-errors
          ;; `ignore-errors` is needed when before first heading.
          (-nomis/outline-up-heading 1))
        (or (not (-nomis/outline-on-heading?)) ; blank lines at top of file?
            (= olevel (funcall outline-level)))))))

(defun -nomis/outline-top-level-level ()
  (assert (-nomis/outline-on-heading?))
  (save-excursion
    (beginning-of-buffer)
    (unless (-nomis/outline-on-heading?) (outline-next-heading))
    (funcall outline-level)))

(defun -nomis/outline-ensure-heading-shown ()
  (when (outline-invisible-p)
    ;; Is there a simpler way to show the heading but not the body?
    (outline-show-entry)
    (outline-hide-entry)))

(defun -nomis/show-children ()
  ;; The `1` is important; otherwise we get bodies of children.
  (outline-show-children 1))

(defun -nomis/outline-prev-or-next (direction)
  (cl-ecase direction
    (:backward (outline-previous-heading))
    (:forward (outline-next-heading))))

;;;;; Lineage spec

;; A lineage-spec controls how lineages are displayed and has the following
;; entries (with permitted values nested):
;;
;; - `:spec/pre-hide-all?`
;;   - boolean
;;
;; - `:spec/pre-hide-children?`
;;   - boolean
;;
;; - `:spec/parents-approach` (doesn't hide anything, but can show things)
;;   - `nil`
;;     - Do nothing.
;;   - `:parents/thin`
;;     - Show parents.
;;   - `:parents/fat`
;;     - Show parents, siblings of parents, and siblings.
;;
;; - `:spec/children-approach` (doesn't hide anything, but can show things)
;;   - `nil` or `0`
;;     - Do nothing.
;;   - `1` / `2` / `3` / `4`
;;     - Show body/children/branches/subtree.

(defconst -nomis/outline-children-approach-max 4)

(defconst fat-parents-lineage-spec
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat))

(defconst max-visibility-span-lineage-spec
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach 1))

(defconst max-lineage-spec
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach -nomis/outline-children-approach-max))

(defconst step-lineage-spec
  (a-hash-table :spec/pre-hide-all? t
                :spec/parents-approach :parents/fat
                :spec/children-approach -nomis/outline-children-approach-max))

(defconst navigation-lineage-spec
  (a-hash-table :spec/parents-approach :parents/thin))

(defun show-children-lineage-spec (children-approach)
  (a-hash-table :spec/pre-hide-children? t
                :spec/children-approach children-approach
                :spec/pulse-max-children? t))

;;;;; Hide/show lineage

(defun -nomis/outline-hsl-hide (lineage-spec)
  (let* ((pre-hide-all? (a-get lineage-spec :spec/pre-hide-all?))
         (parents-approach (a-get lineage-spec :spec/parents-approach)))
    (when pre-hide-all?
      (let* ((top-level-level (-nomis/outline-top-level-level))
             (hide-level (cl-ecase parents-approach
                           ((nil :parents/thin) (1- top-level-level))
                           (:parents/fat top-level-level))))
        (outline-hide-sublevels (max 1 ; avoid error when < 1
                                     hide-level))))))

(defun -nomis/outline-hsl-show-parents (lineage-spec)
  (let* ((parents-approach (a-get lineage-spec :spec/parents-approach)))
    (when parents-approach
      (let* ((parent-points
              (let* ((ps '()))
                (save-excursion
                  (while (and (-nomis/outline-on-heading?)
                              (not (-nomis/outline-on-top-level-heading?)))
                    (-nomis/outline-up-heading 1)
                    (push (point) ps)))
                ps)))
        (save-excursion
          (cl-loop
           for p in parent-points
           do (progn
                (goto-char p)
                (-nomis/outline-ensure-heading-shown)
                (cl-ecase parents-approach
                  (:parents/thin nil)
                  (:parents/fat (-nomis/show-children))))))))))

(defun -nomis/outline-hsl-show-children (lineage-spec)
  (when (a-get lineage-spec :spec/pre-hide-children?)
    (outline-hide-subtree))
  (cl-ecase (a-get lineage-spec :spec/children-approach)
    ((nil) nil)
    (0 nil)
    (1 (outline-show-entry))
    (2 (outline-show-entry)
       (-nomis/show-children))
    (3 (outline-show-entry)
       (outline-show-branches))
    (4 (outline-show-subtree))))

(defun -nomis/outline-show-lineage (lineage-spec)
  (-nomis/outline-hsl-hide lineage-spec)
  (-nomis/outline-hsl-show-parents lineage-spec)
  (-nomis/outline-ensure-heading-shown)
  (-nomis/outline-hsl-show-children lineage-spec)
  (when (and (a-get lineage-spec :spec/pulse-max-children?)
             (= (a-get lineage-spec :spec/children-approach)
                -nomis/outline-children-approach-max))
    (-nomis/outline-pulse-current-section)))

;;;;; Previous/next helpers

(defun -nomis/outline-prev-next-same-level (direction allow-cross-parent?)
  (let* ((opoint (point))
         (level (funcall outline-level))
         (npoint  (save-excursion
                    ;; The logic here is a copy-and-edit of
                    ;; `outline-get-last-sibling` and
                    ;; `outline-get-next-sibling`.
                    (-nomis/outline-prev-or-next direction)
                    (when (cl-ecase direction
                            (:backward (and (/= (point) opoint)
                                            (outline-on-heading-p t)))
                            (:forward t))
                      (while (and (cl-ecase direction
                                    (:backward t)
                                    (:forward (not (eobp))))
                                  (funcall (if allow-cross-parent? #'/= #'>)
                                           (funcall outline-level)
                                           level)
                                  (cl-ecase direction
                                    (:backward (not (bobp)))
                                    (:forward t)))
                        (-nomis/outline-prev-or-next direction))
                      (if (or (cl-ecase direction
                                (:backward nil)
                                (:forward (eobp)))
                              (< (funcall outline-level) level))
                          nil
                        (cl-assert (= level (funcall outline-level)))
                        (point))))))
    (when npoint
      (goto-char npoint))))

(defun -nomis/outline-prev-or-next-heading-pos (lineage-spec
                                                start
                                                direction
                                                kind)
  (when start
    (save-excursion
      (goto-char start)
      (let* ((boh? (-nomis/outline-at-beginning-of-heading?)))
        (if (and (eq direction :backward)
                 (not boh?))
            (progn
              (-nomis/outline-back-to-heading?)
              (point))
          (when (and (eq direction :forward)
                     (not boh?))
            (-nomis/outline-back-to-heading?))
          (cl-ecase kind
            (:any-level
             (-nomis/outline-prev-or-next direction))
            (:sibling
             (-nomis/outline-prev-next-same-level direction nil))
            (:same-level-allow-cross-parent
             (-nomis/outline-prev-next-same-level direction t)))
          (when (and (/= (point) start)
                     (-nomis/outline-on-heading?))
            ;; ^^ Check of `(-nomis/outline-on-heading?)` needed because
            ;;    `-nomis/outline-prev-or-next` goes to BOF or EOF when there's
            ;;    no prev/next heading.
            (point)))))))

(defun -nomis/outline-prev-or-next-heading (lineage-spec
                                            n
                                            direction
                                            kind)
  (let* ((pos (->> (-iterate (lambda (start)
                               (-nomis/outline-prev-or-next-heading-pos
                                lineage-spec
                                start
                                direction
                                kind))
                             (point)
                             (1+ n))
                   cl-rest
                   (-drop (1- n))
                   cl-first)))
    (if pos
        (progn
          (goto-char pos)
          (-nomis/outline-show-lineage lineage-spec))
      (let* ((direction-word (cl-ecase direction
                               (:backward "previous")
                               (:forward "next")))
             (kind-word (cl-ecase kind
                          (:any-level "heading")
                          (:sibling "sibling")
                          (:same-level-allow-cross-parent "same-level"))))
        (nomis/popup/error-message
         "No %s%s %s"
         (if (= n 1) "" (concat (-nomis/outline-ordinal n)
                                "-"))
         direction-word
         kind-word)))))

;;;;; -nomis/outline-command

(defun -nomis/outline-command* (f)
  (push-mark)
  (nomis/scrolling/with-maybe-maintain-line-no-in-window
    (funcall f)))

(cl-defmacro -nomis/outline-command (_opts &body body)
  (declare (indent 1))
  `(-nomis/outline-command* (lambda () ,@body) ))

;;;; API

;;;;; nomis/outline-inc-children / nomis/outline-dec-children

(defvar -nomis/outline-increments-children-approach)

(defun -nomis/outline-inc-dec-message (approach)
  (cl-ecase approach
    (0 (nomis/popup/message "Folded"))
    (1 (nomis/popup/message "Body"))
    (2 (nomis/popup/message "Children"))
    (3 (nomis/popup/message "Branches"))
    (4 (nomis/popup/message "Subtree"))))

(defun nomis/outline-show-lineage-with-incs-or-decs (inc-or-dec)
  (let* ((current-approach
          ;; TODO: At some point change this to look at the actual text rather
          ;;       than relying on `-nomis/outline-increments-children-approach`.
          (if (member (-nomis/outline-last-command)
                      '(nomis/tree/tab
                        nomis/tree/shifttab))
              -nomis/outline-increments-children-approach
            ;; TODO: These out-of-range values are a bit "clever".
            ;;       Maybe rewrite.
            (cl-ecase inc-or-dec
              (:inc -1)
              (:dec (1+ -nomis/outline-children-approach-max))))))
    (if (= current-approach (cl-ecase inc-or-dec
                              (:inc -nomis/outline-children-approach-max)
                              (:dec 0)))
        (nomis/popup/error-message (cl-ecase inc-or-dec
                                     (:inc "Already fully expanded")
                                     (:dec "Already fully collapsed")))
      (let* ((approach (cl-ecase inc-or-dec
                         (:inc (1+ current-approach))
                         (:dec (1- current-approach)))))
        (setq -nomis/outline-increments-children-approach approach)
        (-nomis/outline-show-lineage (show-children-lineage-spec approach))
        (-nomis/outline-inc-dec-message approach)))))

(defun nomis/outline-inc-children ()
  (nomis/outline-show-lineage-with-incs-or-decs :inc))

(defun nomis/outline-dec-children ()
  (nomis/outline-show-lineage-with-incs-or-decs :dec))

;;;;; Search heading text

(cl-defmethod nomis/tree/search-heading-text--aux ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/search-heading-text-again--aux ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

;;;;; nomis/outline/visibility-span

(cl-defmethod nomis/tree/visibility-span/less--aux ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/visibility-span/more--aux ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/visibility-span/set-min--aux ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/visibility-span/set-max--aux ((k (eql :outline)))
  (-nomis/outline-show-lineage max-visibility-span-lineage-spec))

;;;;; nomis/outline-show-max-lineage

(cl-defmethod nomis/tree/max-lineage--aux ((k (eql :outline)))
  (-nomis/outline-show-lineage max-lineage-spec))

;;;;; nomis/tree/show-tree-only--aux

(cl-defmethod nomis/tree/show-tree-only--aux ((k (eql :outline)))
  (-nomis/outline-show-lineage fat-parents-lineage-spec))

;;;;; nomis/tree/set-step-n-levels-to-show--aux

(cl-defmethod nomis/tree/set-step-n-levels-to-show--aux ((k (eql :outline))
                                                         n)
  (error "Not supported: %s %s" k this-command))

;;;;; Expand/collapse

(cl-defmethod nomis/tree/show-children-from-point/incremental/less--aux
  ((k (eql :outline)) n)
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-point/incremental/more--aux
  ((k (eql :outline)) n)
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-point/set-min--aux
  ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-point/fully-expand--aux
  ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-root/incremental/less--aux
  ((k (eql :outline)) n)
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-root/incremental/more--aux
  ((k (eql :outline)) n)
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-root/set-min--aux
  ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-root/fully-expand--aux
  ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-parent/incremental/less--aux
  ((k (eql :outline)) n)
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-parent/incremental/more--aux
  ((k (eql :outline)) n)
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-parent/set-min--aux
  ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-parent/fully-expand--aux
  ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-all-roots/incremental/less--aux
  ((k (eql :outline)) n)
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-all-roots/incremental/more--aux
  ((k (eql :outline)) n)
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-all-roots/set-min--aux
  ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-all-roots/fully-expand--aux
  ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-root/to-current-level--aux
  ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

(cl-defmethod nomis/tree/show-children-from-all-roots/to-current-level--aux
  ((k (eql :outline)))
  (error "Not supported: %s %s" k this-command))

;;;;; Movement

(cl-defmethod nomis/tree/previous-sibling--aux ((k (eql :outline)))
  (nomis/outline-previous-sibling 1))

(cl-defmethod nomis/tree/next-sibling--aux ((k (eql :outline)))
  (nomis/outline-next-sibling 1))

(cl-defmethod nomis/tree/previous-sibling/allow-cross-parent--aux
  ((k (eql :outline)))
  (nomis/outline-previous-sibling/allow-cross-parent 1))

(cl-defmethod nomis/tree/next-sibling/allow-cross-parent--aux
  ((k (eql :outline)))
  (nomis/outline-next-sibling/allow-cross-parent 1))

;;;;; Tab and shift-tab

(cl-defmethod nomis/tree/tab--aux ((k (eql :outline)) arg)
  ;; TODO: Compare with the `:org` method and extract common functionality
  ;;       into the caller.
  ;; TODO: Make use of `arg`.
  (if (and (bolp)
           (looking-at-p outline-regexp))
      (nomis/outline-inc-children)
    ;; Maybe we could find what Tab would be bound to if `outline-minor-mode`
    ;; were not enabled. I've tried but it's non-trivial. So I'm not bothering,
    ;; at least for now.
    (company-indent-or-complete-common arg)))

(cl-defmethod nomis/tree/shifttab--aux ((k (eql :outline)) arg)
  (nomis/outline-dec-children))

;;;;; Previous

(defun nomis/outline-previous-heading (n)
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading navigation-lineage-spec
                                         n
                                         :backward
                                         :any-level)))

(defun nomis/outline-previous-sibling (n)
  "Move backward to the N'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading navigation-lineage-spec
                                         n
                                         :backward
                                         :sibling)))

(defun nomis/outline-previous-sibling/allow-cross-parent (n)
  "Move backward to the N'th heading at same level as this one.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading navigation-lineage-spec
                                         n
                                         :backward
                                         :same-level-allow-cross-parent)))

(cl-defmethod nomis/tree/step-backward--aux ((k (eql :outline)) n)
  "Move backward to the N'th heading at same level as this one, then show
fat parents and all children.
Stop at the first and last headings of a superior heading."
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading step-lineage-spec
                                         (or n 1)
                                         :backward
                                         :sibling)))

(cl-defmethod nomis/tree/step-backward/allow-cross-parent--aux ((k (eql :outline)) n)
  "Move backward to the N'th heading at same level as this one, then show
fat parents and all children.
Can pass by a superior heading."
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading step-lineage-spec
                                         (or n 1)
                                         :backward
                                         :same-level-allow-cross-parent)))

;;;;; Next

(defun nomis/outline-next-heading (n)
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading navigation-lineage-spec
                                         n
                                         :forward
                                         :any-level)))

(defun nomis/outline-next-sibling (n)
  "Move forward to the N'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading navigation-lineage-spec
                                         n
                                         :forward
                                         :sibling)))

(defun nomis/outline-next-sibling/allow-cross-parent (n)
  "Move forward to the N'th heading at same level as this one.
Can pass by a superior heading."
  (interactive "p")
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading navigation-lineage-spec
                                         n
                                         :forward
                                         :same-level-allow-cross-parent)))

(cl-defmethod nomis/tree/step-forward--aux ((k (eql :outline)) n)
  "Move forward to the N'th heading at same level as this one, then show
fat parents and all children.
Stop at the first and last headings of a superior heading."
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading step-lineage-spec
                                         (or n 1)
                                         :forward
                                         :sibling)))

(cl-defmethod nomis/tree/step-forward/allow-cross-parent--aux ((k (eql :outline)) n)
  "Move forward to the N'th heading at same level as this one, then show
fat parents and all children.
Can pass by a superior heading."
  (-nomis/outline-command
      nil
    (-nomis/outline-prev-or-next-heading step-lineage-spec
                                         (or n 1)
                                         :forward
                                         :same-level-allow-cross-parent)))

;;; End

(provide 'nomis-outline-uber)
