(ns multi-repl.core
  (:require [msgpack.core :as msgpack]
            [me.raynes.conch.low-level :as sh]
            [com.stuartsierra.component :as component])
  (:import [java.io DataOutputStream DataInputStream])
  (:gen-class))

(defrecord Repl [command proc sink source]
  component/Lifecycle

  (start [this]
    (if proc
      this
      (let [proc (apply sh/proc command)]
        (println ";; Starting repl:" command)
        (assoc this
               :proc proc
               :sink (DataOutputStream. (:in proc))
               :source (DataInputStream. (:out proc))))))

  (stop [this]
    (if (not proc)
      this
      (do (println ";; Stopping repl:" command)
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

(defn print-prompt []
  (print "> ")
  (.flush *out*))

(defn run-line [repl line]
  (if (= line "exit")
    (do (component/stop repl)
        (System/exit 0))
    (let [{:keys [error result]} (eval-in-repl repl line)]
        (if error
          (println "ERROR:" error)
          (println result)))))

(defn -main [& args]
  (let [repl (start-python-repl)]
    (print-prompt)
    (loop [line (read-line)]
      (run-line repl line)
      (print-prompt)
      (recur (read-line)))))
