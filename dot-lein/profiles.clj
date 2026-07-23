{:user
 {:source-paths ["_no-commit_/code"
                 "_no-commit_/stories"]
  :deploy-repositories [
                        ;; ==== Below here same as work
                        ["clojars-no-sign" {:url "https://clojars.org/repo"
                                            :sign-releases false}]]
  :dependencies [;; Upgrade with CIDER. (Do a `cider-jack-in` with these
                 ;; commented out, and look at the command line that is used.)
                 [cider/cider-nrepl "0.57.0"]
                 [refactor-nrepl/refactor-nrepl "3.11.0"]

                 ;; Other
                 [com.nomistech/kaocha-with-nomis-hacks "0.9.0"]
                 [com.nomistech/portal-logger "0.1.0-SNAPSHOT"]
                 [djblue/portal "0.35.0"]
                 [fipp/fipp "0.6.26"]
                 [lambdaisland/deep-diff2 "2.7.169"]
                 [midje/midje "1.10.9"] ; to allow running Midje autotest in a CLJ REPL
                 [nomis-clj-repl-tools/nomis-clj-repl-tools "0.1.7"]
                 [org.clojure/java.classpath "1.0.0"]
                 [pjstadig/humane-test-output "0.11.0"]]
  :aliases {"kaocha" ["run" "-m" "kaocha.runner"]}
  :injections [;; (require 'spyscope.core)
               (require 'nomis-clj-repl-tools)
               (require 'pjstadig.humane-test-output)
               (pjstadig.humane-test-output/activate!)]
  :plugins [[lein-midje "3.2.2"]
            [lein-ancient "1.0.0-RC3"]
            [lein-cljfmt "0.8.0"]
            [lein-cloverage "1.2.4"]
            [walmartlabs/vizdeps "0.2.0"]
            [jonase/eastwood "0.9.6"]
            [lein-kibit "0.1.8" :exclusions [org.clojure/tools.namespace]]
            [lein-nomis-ns-graph "0.14.6"]
            [lein-pprint "1.3.2"]
            ;; [com.github.clojure-lsp/lein-clojure-lsp "0.1.0"] ; NB: Only needed for use with `lein clojure-lsp ...` -- not to use `clojure-lsp` in general
            ]
  :cljfmt {:remove-surrounding-whitespace?  false
           :insert-missing-whitespace?      false
           :remove-consecutive-blank-lines? false
           :indents {;; Let's keep this simple. Nothing here.
                     ;; ;; clojure.core
                     ;; letfn              [[:block 1] [:inner 2]]
                     ;; ;; Midje
                     ;; fact               [[:inner 0]]
                     ;; facts              [[:inner 0]]
                     ;; prerequisite       [[:inner 0]]
                     ;; provided           [[:inner 0]]
                     ;; against-background [[:inner 0]]
                     }}}

 :repl
 {:repl-options {:timeout 120000 ; milliseconds
                 }}

 :clj-usage-graph
 {:dependencies [[com.gfredericks/clj-usage-graph "0.3.0"]]}}
