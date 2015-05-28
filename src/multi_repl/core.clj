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
          clojure.string/trim-newline))

(defn cprint [console & args]
  (binding [*out* (:out console)]
    (apply print args)
    (.flush *out*)))

(defn cprintln [console & args]
  (binding [*out* (:out console)]
    (apply println args)))

(defn add-to-history [console command]
  (-> (:reader console)
      .getHistory
      (.add (str command "\r"))))

(defn remove-n-from-history [console n]
  (let [history (-> (:reader console)
                    .getHistory)]
    (dotimes [_ n]
      (.removeLast history))))

(defn drop-first-and-last [v]
  (subvec v 1 (- (count v) 1)))

(defn eval-and-print [system line]
  (print-err "line" line)
  (let [{:keys [repl console]} system
        {:keys [error etype result]} (eval-in-repl repl line)]
    (if error
      (do (cprintln console "ETYPE:" etype)
          (cprintln console "ERROR:" error))
      (cprintln console result))))

(defn newline? [c] (= c 13))
(defn escape?  [c] (= c 27))
(defn percent? [c] (= c 37))

(defn min-dec [x minimum]
  (max (dec x) minimum))

(defn max-inc [x maximum]
  (min (inc x) maximum))

(defn insert [vec pos item]
  (print-err "vec" vec)
  (print-err "pos" pos)
  (print-err "item" item)
  (apply conj (subvec vec 0 pos) item (subvec vec pos)))

(defn insert-char [buffer [x y] c]
  (update-in buffer [y] insert x (char c)))

(defn inc-x [cursor] (update-in cursor [0] inc))
(defn dec-x [cursor] (update-in cursor [0] min-dec 0))
(defn inc-y [cursor] (update-in cursor [1] inc))
(defn dec-y [cursor] (update-in cursor [1] min-dec 0))

(defn set-x [cursor n] (assoc cursor 0 n))
(defn set-y [cursor n] (assoc cursor 1 n))

(defn start-buffer [console]
  (let [reader (:reader console)]
    (loop [buffer [[]]
           cursor [0 0]
           c (.readCharacter reader)]
      (print-err "buffer" buffer)
      (print-err "cursor" cursor)
      (cprint console (char c))
      (cond
        (escape? c) (let [escape-seq [(.readCharacter reader) (.readCharacter reader)]]
                      (cprint console (char (nth escape-seq 0)))
                      (cprint console (char (nth escape-seq 1)))
                      (condp = escape-seq
                        [91 65] (recur buffer
                                       (dec-y cursor)
                                       (.readCharacter reader))
                        [91 66] (recur buffer
                                       (inc-y cursor)
                                       (.readCharacter reader))
                        [91 67] (recur buffer
                                       (inc-x cursor)
                                       (.readCharacter reader))
                        [91 68] (recur buffer
                                       (dec-x cursor)
                                       (.readCharacter reader))))
        (newline? c) (do (cprint console (char 27))
                         (cprint console (char 91))
                         (cprint console (char 83))
                         (recur (conj buffer [])
                            (-> cursor inc-y (set-x 0))
                            (.readCharacter reader)))
        (percent? c) (let [c (.readCharacter reader)]
                       (if (= c 114)
                         (clojure.string/join "\n" (map #(apply str %) buffer))
                         (recur buffer cursor c)))
        :else (recur (insert-char buffer cursor c)
                     (update-in cursor [0] inc)
                     (.readCharacter reader))))))

(defmulti run-line (fn [system line] line))

(defmethod run-line "%b" [system line]
  (let [console (:console system)
        buffer (start-buffer console)]
    (eval-and-print system buffer)))

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
