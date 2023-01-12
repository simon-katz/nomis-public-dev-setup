;;;; Stuff to manually test `nomis-clojure-privacy-highlighting-mode`.

;;;; `^:private` declarations

(def ^:private xxxx-$&*<>+= 42)

( def
  ^:private
  xxxx-$&*<>+= 42)

(defn ^:private xxxx-$&*<>+= [] 42)

( defn
  ^:private
  xxxx-$&*<>+= [] 42)

(def-whatever ^:private xxxx-$&*<>+= [] 42)

( def-whatever
  ^:private
  xxxx-$&*<>+= [] 42)

(xxxx/def ^:private xxxx-$&*<>+= 42)

(    xxxx/def ^:private xxxx-$&*<>+= 42)

;;;; Use of `def...-`

(def-  xxxx-$&*<>+= 42)

( def-
  xxxx-$&*<>+= 42)

(defn- xxxx-$&*<>+= [] 42)

( defn-
  xxxx-$&*<>+= [] 42)

(def-whatever- xxxx-$&*<>+= [] 42)

( def-whatever-
  xxxx-$&*<>+= [] 42)

;;;; Public

(def  xxxx-$&*<>+= 42)

( def
  xxxx-$&*<>+= 42)

(defn xxxx-$&*<>+= [] 42)

( defn
  xxxx-$&*<>+= [] 42)

(def-whatever xxxx-$&*<>+= [] 42)

( def-whatever
  xxxx-$&*<>+= [] 42)

(xxxx/def xxxx-$&*<>+= 42)

(    xxxx/def xxxx-$&*<>+= 42)
