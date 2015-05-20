(ns multi-repl.core
  (:require [msgpack.core :as msgpack]
            [me.raynes.conch.low-level :as sh]
            [com.stuartsierra.component :as component])
  (:import [java.io DataOutputStream DataInputStream PrintWriter]
           [jline.console ConsoleReader])
  (:gen-class))

(defn print-flush [& args]
  (apply print args)
  (.flush *out*))

(defn print-err [& args]
  (binding [*out* *err*]
    (apply println args)))

(defrecord Repl [command proc sink source]
  component/Lifecycle

  (start [this]
    (if proc
      this
      (let [proc (apply sh/proc command)]
        (print-err ";; Starting repl:" command)
        (assoc this
               :proc proc
               :sink (DataOutputStream. (:in proc))
               :source (DataInputStream. (:out proc))))))

  (stop [this]
    (if (not proc)
      this
      (do (print-err ";; Stopping repl:" command)
          (sh/destroy (:proc this))
          (dissoc this :proc :sink :source)))))

(defn new-repl [command]
  (map->Repl {:command command}))

(defn format-response [response]
  (clojure.set/rename-keys response {"result" :result
                                     "error" :error}))

(defn eval-in-repl [repl statement]
  (let [{:keys [sink source proc]} repl
        message {:statement statement}]
    (msgpack/pack-stream message sink)
    (.flush sink)
    (format-response (msgpack/unpack-stream source))))

(defn start-python-repl []
  (-> (new-repl ["python" "-u" "python-repl.py"])
      (component/start)))

(defn read-line [reader]
  (-> (.readLine reader)
      clojure.string/trim))

(defmulti run-line (fn [repl line] line))

(defmethod run-line "exit" [repl _]
  (component/stop repl)
  (System/exit 0))

(defmethod run-line :default [repl line]
  (let [{:keys [error result]} (eval-in-repl repl line)]
    (if error
      (println "ERROR:" error)
      (println result))))

(defn -main [& args]
  (let [repl (start-python-repl)
        reader (ConsoleReader.)
        out (PrintWriter. (.getOutput reader))]
    (.setPrompt reader "> ")
    (loop [line (read-line reader)]
      (print-err "line" line)
      (run-line repl line)
      (.flush out)
      (recur (read-line reader)))))
