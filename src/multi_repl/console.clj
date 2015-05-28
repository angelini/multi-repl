(ns multi-repl.console
  (:refer-clojure :exclude [print println read-line])
  (:require [com.stuartsierra.component :as component]
            [multi-repl.utils :refer (print-err)])
  (:import [java.io PrintWriter]
           [jline.console ConsoleReader]))

(defrecord Console [prompt reader out]
  component/Lifecycle

  (start [this]
    (if reader
      this
      (let [reader (ConsoleReader.)
            out (PrintWriter. (.getOutput reader))]
        (.setPrompt reader "> ")
        (print-err ";; Starting console")
        (assoc this
               :reader reader
               :out out))))

  (stop [this]
    (if (not reader)
      this
      (do (print-err ";; Stopping console")
          (.close out)
          (dissoc this :reader :out)))))

(defn new-console [prompt]
  (map->Console {:prompt prompt}))

(defn read-line [console]
  (.flush (:out console))
  (some-> (:reader console)
          .readLine
          clojure.string/trim-newline))

(defn read-char [console]
  (.readCharacter (:reader console)))

(defn print [console & args]
  (binding [*out* (:out console)]
    (apply clojure.core/print args)
    (.flush *out*)))

(defn println [console & args]
  (binding [*out* (:out console)]
    (apply clojure.core/println args)))

(defn print-escape-seq [console c]
  (print console (char 27))
  (print console (char 91))
  (print console c))

(defn history-add [console command]
  (-> (:reader console)
      .getHistory
      (.add (str command "\r"))))

(defn history-remove-last [console]
  (-> (:reader console)
      .getHistory
      .removeLast))
