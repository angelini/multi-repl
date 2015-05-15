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
    (apply prn args)))

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

(defn run-input [repl input]
  (if (= input "exit")
    (do (component/stop repl)
        (System/exit 0))
    (let [{:keys [error result]} (eval-in-repl repl input)]
      (if error
        (println "\nERROR:" error)
        (println "\n" result)))))

(defn valid-input? [input]
  (print-err "valid-input?" input)
  (boolean (re-matches #".*\r" input)))

(defn read-char [reader]
  (-> (.readCharacter reader)
      char))

(defn read-input [reader]
  (loop [c (read-char reader)
         buffer ""]
    (let [buffer (str buffer c)]
      (if (valid-input? buffer)
        (clojure.string/trim buffer)
        (do (print-flush c)
            (recur (read-char reader)
                   buffer))))))

(defn -main [& args]
  (let [repl (start-python-repl)
        reader (ConsoleReader.)]
    (print-flush "> ")
    (loop [input (read-input reader)]
      (run-input repl input)
      (print-flush "> ")
      (recur (read-input reader)))))
