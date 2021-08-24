;;;; Init stuff -- Tests for nomis-string-edit --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(ert-deftest nomis/chop-seq-test ()
  (should
   (equal (nomis/chop-seq :sep
                          '(1 2 3 4 5 6 :sep 8 9 0
                              :sep 1 2 3 4 5 6 7 8 9 0 1 2 3 4
                              :sep 1 2 3 :sep 5 6 7 8 9 0
                              :sep 1 2 3 :sep 5 6 7 8 9
                              :sep 1 2 3 :sep 5 6 7 8
                              :sep 1 2 3 :sep 5 6 7
                              :sep 1 2 3 :sep 5 6
                              :sep 1 2 3 :sep 5)
                          10)
          '((1 2 3 4 5 6 :sep 8 9 0)
            (1 2 3 4 5 6 7 8 9 0 1 2 3 4)
            (1 2 3 :sep 5 6 7 8 9 0)
            (1 2 3 :sep 5 6 7 8 9)
            (1 2 3 :sep 5 6 7 8)
            (1 2 3 :sep 5 6 7)
            (1 2 3 :sep 5 6 :sep 1 2 3)
            (5))))
  (should
   (equal (nomis/chop-seq :sep
                          '(:sep :sep 3 4 5 6 :sep 8
                                 :sep 1 2)
                          10)
          '((:sep :sep 3 4 5 6 :sep 8)
            (1 2)))))

(ert-deftest nomis/rearrange-string-into-lines-test ()
  (should
   (equal (nomis/rearrange-string-into-lines
           "I think I've got to grips with it now, and I've summarised my
         understanding of the differences between
         symbols in Common Lisp and Clojure below."
           11
           80)
          "I think I've got to grips with it now, and I've summarised my
           understanding of the differences between symbols in Common Lisp and
           Clojure below."))
  (should
   (equal (nomis/rearrange-string-into-lines
           "123456789012345 7890"
           11
           21)
          "123456789012345
           7890"))
  (should
   (equal (nomis/rearrange-string-into-lines
           "1234567890"
           11
           21)
          "1234567890"))
  (should
   (equal (nomis/rearrange-string-into-lines
           "1234567890 1"
           11
           21)
          "1234567890
           1"))
  (should
   (equal (nomis/rearrange-string-into-lines
           "12345678 0 1"
           11
           21)
          "12345678 0
           1"))
  (should
   (equal (nomis/rearrange-string-into-lines
           "12 4 6 8 0 1 2"
           11
           21)
          "12 4 6 8 0
           1 2"))
  (should
   (equal (nomis/rearrange-string-into-lines
           "1234567890 "
           11
           21)
          "1234567890
           "))
  (should
   (equal (nomis/rearrange-string-into-lines
           "  345  "
           11
           21)
          " 345 "))
  (should
   (equal (nomis/rearrange-string-into-lines
           "12 45 78 0
         12 45 7 90"
           11
           21)
          "12 45 78 0
           12 45 7 90"))
  (should
   (equal (nomis/rearrange-string-into-lines
           "1234567890
         123456789 1"
           11
           21)
          "1234567890
           123456789
           1"))
  (should
   (equal (nomis/rearrange-string-into-lines
           "1234567890
123456789"
           11
           80)
          "1234567890 123456789"))
  (should
   (equal (nomis/rearrange-string-into-lines
           "1



2"
           11
           80)
          "1 2"))
  (should
   (equal (nomis/rearrange-string-into-lines
           "1 1234567890"
           11
           15)
          "1
           1234567890"))
  (should
   (equal (nomis/rearrange-string-into-lines
           "1234567890"
           11
           15)
          "1234567890"))
  (should
   (equal (nomis/rearrange-string-into-lines
           "The quick  quick   brown    fox     jumps       over the lazy dog.
          The quick  quick   brown    fox     jumps       over the lazy dog."
           8
           40)
          "The quick quick brown fox jumps
        over the lazy dog. The quick
        quick brown fox jumps over the
        lazy dog."))
  (should
   (equal (nomis/rearrange-string-into-lines
           "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam eleifend feugiat est, vel tincidunt neque blandit eget. Fusce et iaculis sapien, a vulputate lectus. Sed gravida diam sit amet felis suscipit accumsan. Sed sit amet pellentesque urna, quis varius elit. Maecenas in justo consectetur, ultrices massa sit amet, efficitur urna. Duis nibh augue, fermentum vel eros non, rhoncus rhoncus mi. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In nec suscipit lectus, in commodo ipsum."
           8
           70)
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        Nullam eleifend feugiat est, vel tincidunt neque blandit eget.
        Fusce et iaculis sapien, a vulputate lectus. Sed gravida diam
        sit amet felis suscipit accumsan. Sed sit amet pellentesque
        urna, quis varius elit. Maecenas in justo consectetur,
        ultrices massa sit amet, efficitur urna. Duis nibh augue,
        fermentum vel eros non, rhoncus rhoncus mi. Lorem ipsum dolor
        sit amet, consectetur adipiscing elit. In nec suscipit lectus,
        in commodo ipsum.")))

;;;; ___________________________________________________________________________

(provide 'nomis-string-edit-test)
