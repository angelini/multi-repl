(ns multi-repl.core
  (:require [com.stuartsierra.component :as component]
            [multi-repl.utils :refer (print-err print-flush)]
            [multi-repl.repl :refer (new-repl eval-in-repl)]
            [multi-repl.console :refer (new-console cprintln read-console-line)]
            [multi-repl.buffer :refer (read-buffer)])
  (:gen-class))

(defn eval-and-print [system line]
  (print-err "line" line)
  (let [{:keys [repl console]} system
        {:keys [error etype result]} (eval-in-repl repl line)]
    (if error
      (do (cprintln console "ETYPE:" etype)
          (cprintln console "ERROR:" error))
      (cprintln console result))))

(defmulti run-line (fn [system line] line))

(defmethod run-line "%b" [system line]
  (let [console (:console system)
        buffer (read-buffer console)]
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
