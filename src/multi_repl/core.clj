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
                                     "error" :error
                                     "etype" :etype}))

(defn eval-in-repl [repl statement]
  (let [{:keys [sink source proc]} repl
        message {:statement statement}]
    (msgpack/pack-stream message sink)
    (.flush sink)
    (format-response (msgpack/unpack-stream source))))


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
          clojure.string/trim))

(defn println-console [console & args]
  (binding [*out* (:out console)]
    (apply println args)))

(defn drop-first-and-last [v]
  (subvec v 1 (- (count v) 1)))

(defn eval-and-print [system line]
  (print-err "line" line)
  (let [{:keys [repl console]} system
        {:keys [error etype result]} (eval-in-repl repl line)]
    (if error
      (do (println-console console "ETYPE:" etype)
          (println-console console "ERROR:" error))
      (println-console console result))))

(defmulti run-line (fn [system line] line))

(defmethod run-line "%b" [system line]
  (let [console (:console system)]
    (loop [buffer [line]]
      (print-err "buffer" buffer)
      (if (= (last buffer) "%r")
        (->> buffer
             drop-first-and-last
             (clojure.string/join "\n")
             (eval-and-print system))
        (recur (->> (read-console-line console)
                    (conj buffer)))))))

(defmethod run-line "exit" [system _]
  (component/stop system)
  (System/exit 0))

(defmethod run-line :default [system line]
  (eval-and-print system line))

(defn start-python-repl []
  (-> (new-repl ["python" "-u" "python-repl.py"])
      (component/start)))

(def commands
  {:python ["python" "-u" "python-repl.py"]})

(defn new-system [prompt type]
  (component/system-map :repl (new-repl (get commands type))
                        :console (new-console "> ")))

(defn -main [& args]
  (let [system (-> (new-system "> " :python)
                   component/start)
        console (:console system)]
    (loop [line (read-console-line console)]
      (run-line system line)
      (recur (read-console-line console)))))
