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

(defn parse-input [input]
  (print-err "valid-input?" input)
  (cond
    (= input [27 91 65])           :up
    (= input [27 91 66])           :down
    (= input [27 91 67])           :right
    (= input [27 91 68])           :left
    (= input [127])                :backspace
    (= input [101 120 105 116 13]) :exit
    (= (last input) 13)            (->> input
                                        (map char)
                                        (apply str)
                                        clojure.string/trim)
    :else                          false))

(defn read-input [reader]
  (loop [buffer [(.readCharacter reader)]]
    (if-let [parsed (parse-input buffer)]
      parsed
      (do (print-flush (char (last buffer)))
          (recur (conj buffer (.readCharacter reader)))))))

(defmulti run-input (fn [repl input]
                      (if (keyword? input) input :string)))

(defmethod run-input :default [repl input]
  (println "\nUNKNOWN INPUT:" input))

(defmethod run-input :exit [repl _]
  (component/stop repl)
  (System/exit 0))

(defmethod run-input :string [repl input]
  (let [{:keys [error result]} (eval-in-repl repl input)]
    (if error
      (println "\nERROR:" error)
      (println "\n" result))))

(defn -main [& args]
  (let [repl (start-python-repl)
        reader (ConsoleReader.)]
    (print-flush "> ")
    (loop [input (read-input reader)]
      (print-err "input" input)
      (run-input repl input)
      (print-flush "> ")
      (recur (read-input reader)))))
