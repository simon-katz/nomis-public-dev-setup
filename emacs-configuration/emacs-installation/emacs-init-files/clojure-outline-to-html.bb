#!/usr/bin/env bb

;; Usage: bb clojure-outline-to-html.bb <input.clj>
;;
;; Converts a Clojure outline file to HTML, writing <input>.html next to it.
;;
;; Heading conventions:  ;;;;  => H1,  ;;;;; => H2,  ;;;;;; => H3, etc.
;; Comment lines (^;;):  grouped into prose sections / paragraphs.
;; Everything else:      rendered as <pre><code> blocks.

(ns clojure-outline-to-html
  (:require [clojure.string :as str]))

;;; ── Pure helpers ────────────────────────────────────────────────────────────

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

(defn classify [line]
  (cond
    (re-find #"^;;;;+" line) :heading
    (re-find #"^;;"    line) :prose
    (str/blank?        line) :blank
    :else                    :code))

(defn strip-comment-prefix [line]
  (str/replace line #"^;;+ ?" ""))

;;; ── Inline text styling ─────────────────────────────────────────────────────

(defn style-italics [s]
  ;; Require whitespace or start-of-string before the opening / so that
  ;; Clojure names like m/sp are unaffected.
  (str/replace s #"(\s|^)/([^/]+)/" "$1<em>$2</em>"))

(defn style-concepts [s]
  (str/replace s #"•([^•]+)•" "<span class=\"concept\">$1</span>"))

(defn style-inline-code [s]
  (str/replace s #"`([^`]+)`" "<code>$1</code>"))

(defn style-dashes [s]
  (str/replace s "--" "\u2014"))

(defn style-links [s]
  (str/replace s #"https?://\S+"
               (fn [url]
                 (let [trimmed (str/replace url #"[.,;:!?)\"]+$" "")]
                   (str "<a href=\"" trimmed "\">" trimmed "</a>")))))

(defn apply-styles
  "Apply all inline transformations to an already-HTML-escaped string."
  [s]
  (-> s
      style-italics
      style-concepts
      style-inline-code
      style-dashes
      style-links))

;;; ── State flushers ──────────────────────────────────────────────────────────

(defn flush-para [{:keys [para-lines] :as state}]
  (if (seq para-lines)
    (-> state
        (update :section-parts conj
                (str "<p>"
                     (str/join "\n" (map (comp apply-styles html-escape) para-lines))
                     "</p>\n"))
        (assoc :para-lines []))
    state))

(defn flush-bullets [{:keys [bullet-lines] :as state}]
  (if (seq bullet-lines)
    (let [lis (->> bullet-lines
                   (map (comp apply-styles html-escape))
                   (map #(str "  <li>" % "</li>\n")))]
      (-> state
          (update :section-parts conj
                  (str "<ul>\n" (str/join lis) "</ul>\n"))
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
                     (str/join "\n" (map html-escape trimmed))
                     "</code></pre>\n"))
        state))
    state))

;;; ── Table of contents ───────────────────────────────────────────────────────

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

;;; ── Per-line dispatch ───────────────────────────────────────────────────────

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
        semis    (re-find #"^;;;;+" line)
        text     (str/replace line #"^;;;;+\s*" "")
        level    (- (count semis) 3)
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
                (str "<details open>\n"
                     "<summary>"
                     "<" tag " id=\"" sec-num "\">"
                     sec-num " " styled
                     "</" tag ">"
                     "</summary>\n"))
        (update :open-levels conj level)
        (assoc :mode :code))))

(defn process-prose [state line]
  (let [state (flush-code state)
        text  (strip-comment-prefix line)]
    (cond
      (str/blank? text)
      (-> state flush-para flush-bullets (assoc :mode :prose))

      (str/starts-with? text "- ")
      (-> state
          flush-para
          (update :bullet-lines conj (subs text 2))
          (assoc :mode :prose))

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
  (case (classify line)
    :heading (process-heading   state line)
    :prose   (process-prose     state line)
    :blank   (process-blank     state)
    :code    (process-code-line state line)))

;;; ── HTML template ───────────────────────────────────────────────────────────

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
    h1 { font-size: 1.8em; border-bottom: 2px solid #ccc; padding-bottom: 0.2em; }
    h1.page-title { font-size: 2.2em; border-bottom: 3px solid #aaa; margin-bottom: 0.6em; }
    h2 { font-size: 1.4em; border-bottom: 1px solid #eee; padding-bottom: 0.1em; }
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
      content: '✦ ✦ ✦';
      display: block;
      text-align: center;
      color: #888;
    }
    .concept {
      font-weight: bold;
      background: #f0fff0;
      border-radius: 4px;
      border-color: darkgreen;
      padding: 1px 4px;
      border-style: solid;
      border-width: thin;
    }
    details > summary {
      cursor: pointer;
      display: block;
      list-style: none;
    }
    details > summary::-webkit-details-marker {
      display: none;
    }
    details > summary::before {
      content: '▶';
      display: inline-block;
      font-size: 0.65em;
      vertical-align: middle;
      margin-right: 0.4em;
      color: #888;
    }
    details[open] > summary::before {
      content: '▼';
    }
    details > summary > h1,
    details > summary > h2,
    details > summary > h3,
    details > summary > h4,
    details > summary > h5,
    details > summary > h6 {
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
    }
    pre code {
      background: none;
      padding: 0;
      border-radius: 0;
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
       "</body>\n"
       "</html>\n"))

;;; ── Main ────────────────────────────────────────────────────────────────────

(let [input-file  (first *command-line-args*)
      output-file (if-let [dir (second *command-line-args*)]
                    (str/replace input-file
                                 #"^(.*/)?([^/]+)\.[^.]+$"
                                 (str dir "/$2.html"))
                    (str/replace input-file #"\.[^.]+$" ".html"))
      title       (-> input-file
                      (str/replace #".*/" "")
                      (str/replace #"\.[^.]+$" "")
                      (str/replace "_" " ")
                      (as-> s (str/join " " (map str/capitalize (str/split s #" ")))))
      lines       (str/split-lines (slurp input-file))
      initial     {:mode          :code
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
