#!/usr/bin/env bb

;; Usage: bb outline-to-html.bb <input-file> [options]
;;
;; Options:
;;   --output-dir DIR          Directory for the output HTML file
;;   --prose-prefix STR        Comment prefix for prose lines    (default: ;;)
;;   --heading-prefix STR      Comment prefix for H1 headings   (default: ;;;;)
;;   --heading-increment CHAR  Character appended per heading level (default: ;)
;;   --title-strip-prefix STR  Prefix stripped from filename for page title (default: "")
;;
;; Heading conventions (Clojure defaults):  ;;;;  => H1,  ;;;;; => H2, etc.
;; Comment lines (^;;):  grouped into prose sections / paragraphs.
;; Everything else:      rendered as <pre><code> blocks.
;;
;; Special markers:
;;   ;; --new-section--        Force a prose-section break (useful after a code block).

(ns outline-to-html
  (:require [clojure.string :as str]))

;;;; Config

(defn make-config [prose-prefix heading-prefix heading-increment]
  (let [qpp (java.util.regex.Pattern/quote prose-prefix)
        qhp (java.util.regex.Pattern/quote heading-prefix)
        qhi (java.util.regex.Pattern/quote heading-increment)]
    {:prose-prefix      prose-prefix
     :heading-prefix    heading-prefix
     :heading-increment heading-increment
     :heading-re        (re-pattern (str "^" qhp "(?:" qhi ")*"))
     :prose-re          (re-pattern (str "^" qpp))
     :strip-re          (re-pattern (str "^" qpp "(?:" qhi ")* ?"))}))

(defn parse-args [args]
  (loop [args   args
         result {:input-file          nil
                 :output-dir          nil
                 :prose-prefix        ";;"
                 :heading-prefix      ";;;;"
                 :heading-increment   ";"
                 :title-strip-prefix  ""}]
    (if (empty? args)
      result
      (case (first args)
        "--output-dir"         (recur (drop 2 args) (assoc result :output-dir         (second args)))
        "--prose-prefix"       (recur (drop 2 args) (assoc result :prose-prefix       (second args)))
        "--heading-prefix"     (recur (drop 2 args) (assoc result :heading-prefix     (second args)))
        "--heading-increment"  (recur (drop 2 args) (assoc result :heading-increment  (second args)))
        "--title-strip-prefix" (recur (drop 2 args) (assoc result :title-strip-prefix (second args)))
        (recur (rest args)    (assoc result :input-file (first args)))))))

;;;; Pure helpers

(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(def numeric-char? #{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0})

(def ^:private apa-minor-words
  "Words lowercased in APA title case unless they are the first word."
  #{"a" "an" "the"
    "and" "as" "but" "for" "if" "nor" "or" "so" "yet"
    "at" "by" "in" "of" "off" "on" "per" "to" "up" "via"})

(defn apa-title-case [s]
  "Apply APA 7th-edition title case to string S."
  (let [words (str/split s #" ")
        first-non-number-pos (or (first (indices (fn [s]
                                                   (not-every? numeric-char? s))
                                                 words))
                                 0)]
    (str/join " "
              (map-indexed (fn [i word]
                             (if (and  (> i first-non-number-pos)
                                       (contains? apa-minor-words (str/lower-case word)))
                               (str/lower-case word)
                               (str/capitalize word)))
                           words))))

(defn html-escape [s]
  (-> s
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

(defn trim-blank-lines [lines]
  (->> lines
       (drop-while str/blank?)
       reverse
       (drop-while str/blank?)
       reverse
       vec))

(defn classify [line {:keys [heading-re prose-re]}]
  (cond
    (re-find heading-re line) :heading
    (re-find prose-re   line) :prose
    (str/blank?         line) :blank
    :else                     :code))

(defn strip-comment-prefix [line {:keys [strip-re]}]
  (str/replace line strip-re ""))

;;;; Inline text styling

(defn style-italics [s]
  ;; Require whitespace or start-of-string before the opening / so that
  ;; Clojure names like m/sp are unaffected.
  (str/replace s #"(\s|^)/([^/]+)/" "$1<em>$2</em>"))

(defn style-concepts [s]
  (str/replace s #"•([^•]+)•" "<span class=\"concept\">$1</span>"))

(defn style-inline-code [s]
  (str/replace s #"`([^`]+)`" "<code>$1</code>"))

(defn style-dashes [s]
  ;; Replace -- with an em dash, but leave backtick-wrapped spans untouched.
  (str/replace s #"`[^`]+`|--"
               (fn [match]
                 (if (str/starts-with? match "`")
                   match
                   "\u2014"))))

(defn style-todos [s]
  ;; Match "TODO:" preceded by whitespace/start, or "TODO" surrounded by whitespace/boundaries.
  (str/replace s #"(?:(?<=\s)|^)TODO(?::|(?=\s|$))"
               (fn [match] (str "<span class=\"todo\">" match "</span>"))))

(defn style-links [s]
  (str/replace s #"https?://\S+"
               (fn [url]
                 (let [trimmed  (str/replace url #"[.,;:!?)\"]+$" "")
                       trailing (subs url (count trimmed))]
                   (str "<a href=\"" trimmed "\">" trimmed "</a>" trailing)))))

(defn apply-styles
  "Apply all inline transformations to an already-HTML-escaped string."
  [s]
  (-> s
      style-italics
      style-concepts
      style-dashes
      style-inline-code
      style-todos
      style-links))

;;;; State flushers

(defn flush-para [{:keys [para-lines] :as state}]
  (if (seq para-lines)
    (-> state
        (update :section-parts conj
                (str "<p>"
                     (str/join "\n" (map (comp apply-styles html-escape) para-lines))
                     "</p>\n"))
        (assoc :para-lines []))
    state))

(defn parse-bullet-entries [lines]
  "Convert raw indented bullet lines to [{:indent :bullet? :text}]."
  (->> lines
       (remove str/blank?)
       (map (fn [line]
              (let [indent  (count (re-find #"^ *" line))
                    trimmed (str/triml line)
                    bullet? (str/starts-with? trimmed "- ")]
                {:indent  indent
                 :bullet? bullet?
                 :text    (if bullet? (subs trimmed 2) trimmed)})))))

(defn build-bullet-tree [entries]
  "Convert flat entries to a tree [{:text :children}]."
  (loop [entries entries
         items   []
         current nil]
    (if (empty? entries)
      (if current (conj items current) items)
      (let [{:keys [indent bullet? text]} (first entries)]
        (cond
          ;; New top-level bullet
          (and bullet? (zero? indent))
          (recur (rest entries)
                 (if current (conj items current) items)
                 {:text text :children []})

          ;; New nested bullet
          (and bullet? (pos? indent))
          (recur (rest entries)
                 items
                 (update current :children conj {:text text :children []}))

          ;; Continuation of last nested bullet
          (and (not bullet?) (pos? indent) (seq (:children current)))
          (recur (rest entries)
                 items
                 (update-in current [:children (dec (count (:children current))) :text]
                            str " " text))

          ;; Continuation of current top-level bullet
          :else
          (recur (rest entries)
                 items
                 (update current :text str " " text)))))))

(defn render-bullet-tree [items]
  "Render a bullet tree to an HTML <ul> string."
  (str "<ul>\n"
       (str/join
        (map (fn [{:keys [text children]}]
               (str "<li>"
                    (apply-styles (html-escape text))
                    (when (seq children)
                      (str "\n" (render-bullet-tree children)))
                    "</li>\n"))
             items))
       "</ul>\n"))

(defn flush-bullets [{:keys [bullet-lines] :as state}]
  (if (seq bullet-lines)
    (let [html (-> bullet-lines
                   parse-bullet-entries
                   build-bullet-tree
                   render-bullet-tree)]
      (-> state
          (update :section-parts conj html)
          (assoc :bullet-lines [])))
    state))

(defn flush-section [state]
  (let [{:keys [section-parts] :as state} (-> state flush-para flush-bullets)]
    (if (seq section-parts)
      (-> state
          (update :html-parts conj
                  (str "<div class=\"prose-section\">\n"
                       (str/join section-parts)
                       "</div>\n"))
          (assoc :section-parts []))
      state)))

(defn flush-code [{:keys [code-lines] :as state}]
  (if (seq code-lines)
    (let [trimmed (trim-blank-lines code-lines)
          state   (assoc state :code-lines [])]
      (if (seq trimmed)
        (update state :html-parts conj
                (str "<pre><code>"
                     (str/join "\n" (map (comp style-todos html-escape) trimmed))
                     "</code></pre>\n"))
        state))
    state))

;;;; Table of contents

(defn build-toc [toc-entries]
  (let [rows (map (fn [{:keys [level sec-num styled]}]
                    (str "<div class=\"toc-level-" level "\">"
                         "<a href=\"#" sec-num "\">"
                         sec-num " " styled
                         "</a></div>\n"))
                  toc-entries)]
    (str "<nav class=\"toc\">\n"
         "<div class=\"toc-title\">Contents</div>\n"
         (str/join rows)
         "</nav>\n")))

;;;; Per-line dispatch

(defn close-details
  "Close all open <details> whose level is >= min-level."
  [state min-level]
  (loop [state state]
    (let [levels (:open-levels state)]
      (if (and (seq levels) (>= (peek levels) min-level))
        (recur (-> state
                   (update :html-parts conj "</details>\n")
                   (update :open-levels pop)))
        state))))

(defn process-heading [state line]
  (let [state    (-> state flush-code flush-section)
        {:keys [heading-prefix heading-increment heading-re]} (:config state)
        prefix   (re-find heading-re line)
        level    (inc (/ (- (count prefix) (count heading-prefix))
                         (count heading-increment)))
        text     (str/triml (subs line (count prefix)))
        tag      (str "h" level)
        counters (-> (:counters state)
                     (update (dec level) inc)
                     (as-> c (reduce #(assoc %1 %2 0) c (range level (count c)))))
        sec-num  (str/join "." (take level counters))
        styled   (apply-styles (html-escape text))
        state    (-> state
                     (assoc :counters counters)
                     (close-details level))]
    (-> state
        (update :toc-entries conj {:level   level
                                   :sec-num sec-num
                                   :styled  styled})
        (update :html-parts conj
                (str "<details open class=\"heading-level-" level "\">\n"
                     "<summary>"
                     "<span class=\"toggle\"></span>"
                     "<span class=\"heading-text\">"
                     "<" tag " id=\"" sec-num "\">"
                     sec-num " " styled
                     "</" tag ">"
                     "</span>"
                     "</summary>\n"))
        (update :open-levels conj level)
        (assoc :mode :code))))

(defn process-prose [state line]
  (let [state (flush-code state)
        text  (strip-comment-prefix line (:config state))]
    (cond
      ;; New-section marker: flush current section and emit an explicit separator.
      (= (str/trim text) "--new-section--")
      (-> state
          flush-section
          (update :html-parts conj "<div class=\"prose-section-separator\"></div>\n")
          (assoc :mode :prose))

      ;; Blank ;; line: skip silently if inside a bullet list (inter-bullet
      ;; spacing), otherwise treat as a paragraph break.
      (str/blank? text)
      (if (seq (:bullet-lines state))
        (assoc state :mode :prose)
        (-> state flush-para (assoc :mode :prose)))

      ;; Bullet at any indent level (top-level or nested).
      (re-find #"^\s*- " text)
      (-> state
          flush-para
          (update :bullet-lines conj text)
          (assoc :mode :prose))

      ;; Continuation line (leading spaces) while collecting bullets.
      (and (seq (:bullet-lines state)) (str/starts-with? text " "))
      (-> state
          (update :bullet-lines conj text)
          (assoc :mode :prose))

      ;; Normal prose line.
      :else
      (-> state
          flush-bullets
          (update :para-lines conj text)
          (assoc :mode :prose)))))

(defn process-blank [state]
  (if (= (:mode state) :prose)
    (-> state flush-section (assoc :mode :code))
    (update state :code-lines conj "")))

(defn process-code-line [state line]
  (-> state
      flush-section
      (update :code-lines conj line)
      (assoc :mode :code)))

(defn process-line [state line]
  (case (classify line (:config state))
    :heading (process-heading   state line)
    :prose   (process-prose     state line)
    :blank   (process-blank     state)
    :code    (process-code-line state line)))

;;;; HTML template

(def css
  "    body {
      font-family: Georgia, serif;
      max-width: 860px;
      margin: 2em auto;
      padding: 0 1em;
      line-height: 1.6;
      color: #222;
    }
    h1, h2, h3, h4, h5, h6 {
      font-family: sans-serif;
      margin-top: 1.4em;
      margin-bottom: 0.3em;
    }
    h1 { font-size: 1.8em; }
    h1.page-title { font-size: 2.2em; border-bottom: 3px solid #aaa; margin-bottom: 0.6em; }
    h2 { font-size: 1.4em; }
    details.heading-level-1 {
      border-top: 2px solid #aaa;
      margin-top: 1.5em;
      padding-top: 0.4em;
    }
    details.heading-level-2 {
      border-top: 1px solid #ddd;
      margin-top: 0.8em;
      padding-top: 0.2em;
    }
    h3 { font-size: 1.2em; }
    h4 { font-size: 1.1em; }
    h5 { font-size: 1.0em; }
    h6 { font-size: 0.95em; }
    p {
      margin-top: 0.5em;
      margin-bottom: 0.5em;
    }
    .prose-section {
      margin-top: 1em;
    }
    .prose-section + .prose-section::before {
      content: '✦';
      display: block;
      text-align: center;
      color: #404040;
    }
    .prose-section-separator {
      text-align: center;
      color: #404040;
      margin-top: 1em;
    }
    .prose-section-separator::before {
      content: '✦';
    }
    .todo {
      color: red;
      font-weight: bold;
    }
    .concept {
      font-weight: bold;
      background: #f8fff8;
      border-radius: 4px;
      border-color: darkgreen;
      padding: 0px 4px;
      border-style: solid;
      border-width: thin;
    }
    details > summary {
      cursor: default;
      display: block;
      list-style: none;
    }
    details > summary::-webkit-details-marker {
      display: none;
    }
    .toggle {
      display: inline-block;
      font-size: 0.65em;
      vertical-align: middle;
      margin-right: 0.4em;
      color: #404040;
      cursor: pointer;
      user-select: none;
    }
    .toggle::before { content: '▶'; }
    details[open] > summary .toggle::before { content: '▼'; }
    .heading-text {
      display: inline;
    }
    details > summary h1,
    details > summary h2,
    details > summary h3,
    details > summary h4,
    details > summary h5,
    details > summary h6 {
      display: inline;
    }
    details {
      margin-top: 0.2em;
    }
    nav.toc {
      background: #f9f9f9;
      border: 1px solid #ddd;
      border-radius: 4px;
      padding: 0.8em 1.5em 1em;
      margin-bottom: 2em;
      display: inline-block;
      min-width: 18em;
    }
    .toc-title {
      font-family: sans-serif;
      font-weight: bold;
      font-size: 1.05em;
      margin-bottom: 0.5em;
    }
    .toc-level-1 { margin-left: 0em; }
    .toc-level-2 { margin-left: 1.5em; }
    .toc-level-3 { margin-left: 3.0em; }
    .toc-level-4 { margin-left: 4.5em; }
    .toc-level-5 { margin-left: 6.0em; }
    .toc-level-6 { margin-left: 7.5em; }
    nav.toc a { text-decoration: none; color: #333; }
    nav.toc a:hover { text-decoration: underline; }
    pre {
      background: #f5f5f5;
      border: 1px solid #ddd;
      border-radius: 4px;
      padding: 0.8em 1em;
      overflow-x: auto;
      line-height: 1.45;
    }
    code {
      font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;
      font-size: 0.88em;
      background: #f0f0f0;
      padding: 1px 4px;
      border-radius: 3px;
      white-space: nowrap;
    }
    pre code {
      background: none;
      padding: 0;
      border-radius: 0;
      white-space: pre;
    }")

(defn html-page [title body]
  (str "<!DOCTYPE html>\n"
       "<html lang=\"en\">\n"
       "<head>\n"
       "  <meta charset=\"UTF-8\">\n"
       "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
       "  <title>" title "</title>\n"
       "  <style>\n" css "\n  </style>\n"
       "</head>\n"
       "<body>\n"
       body
       "<script>\n"
       "document.querySelectorAll('details > summary').forEach(s => {\n"
       "  s.addEventListener('click', e => {\n"
       "    if (e.target.closest('.heading-text')) e.preventDefault();\n"
       "  });\n"
       "});\n"
       "</script>\n"
       "</body>\n"
       "</html>\n"))

;;;; Main

(let [args        (parse-args *command-line-args*)
      input-file  (:input-file args)
      output-file (if-let [dir (:output-dir args)]
                    (str/replace input-file
                                 #"^(.*/)?([^/]+)\.[^.]+$"
                                 (str dir "/$2.html"))
                    (str/replace input-file #"\.[^.]+$" ".html"))
      config      (make-config (:prose-prefix      args)
                               (:heading-prefix    args)
                               (:heading-increment args))
      title       (-> input-file
                      (str/replace #".*/" "")
                      (str/replace #"\.[^.]+$" "")
                      (str/replace (:title-strip-prefix args) "")
                      (str/replace "_" " ")
                      apa-title-case)
      lines       (str/split-lines (slurp input-file))
      initial     {:mode          :code
                   :config        config
                   :counters      (vec (repeat 6 0))
                   :toc-entries   []
                   :open-levels   []
                   :code-lines    []
                   :para-lines    []
                   :bullet-lines  []
                   :section-parts []
                   :html-parts    []}
      final       (-> (reduce process-line initial lines)
                      flush-code
                      flush-section
                      (close-details 1))
      toc         (build-toc (:toc-entries final))
      body        (str "<h1 class=\"page-title\">" title "</h1>\n"
                       toc
                       (str/join (:html-parts final)))]
  (spit output-file (html-page title body))
  (println (str "Exported to " output-file)))
