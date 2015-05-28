(ns multi-repl.console
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

(defn read-console-line [console]
  (.flush (:out console))
  (some-> (:reader console)
          .readLine
          clojure.string/trim-newline))

(defn read-console-char [console]
  (.readCharacter (:reader console)))

(defn cprint [console & args]
  (binding [*out* (:out console)]
    (apply print args)
    (.flush *out*)))

(defn cprintln [console & args]
  (binding [*out* (:out console)]
    (apply println args)))

(defn cprint-escape-seq [console c]
  (cprint console (char 27))
  (cprint console (char 91))
  (cprint console c))

(defn add-to-history [console command]
  (-> (:reader console)
      .getHistory
      (.add (str command "\r"))))

(defn remove-n-from-history [console n]
  (let [history (-> (:reader console)
                    .getHistory)]
    (dotimes [_ n]
      (.removeLast history))))
