(ns multi-repl.core
  (:require [com.stuartsierra.component :as component]
            [multi-repl.utils :refer (print-err print-flush)]
            [multi-repl.repl :as re]
            [multi-repl.console :as co]
            [multi-repl.buffer :as bu])
  (:gen-class))

(defn eval-and-print [system line]
  (print-err "line" line)
  (let [{:keys [repl console]} system
        {:keys [error etype result]} (re/eval repl line)]
    (if error
      (do (co/println console "ETYPE:" etype)
          (co/println console "ERROR:" error))
      (co/println console result))))

(defmulti run-line (fn [system line] line))

(defmethod run-line "%b" [system line]
  (let [console (:console system)
        buffer (bu/read-buffer console)]
    (eval-and-print system buffer)
    (co/history-remove-last console)
    (co/history-add console buffer)))

(defmethod run-line "exit" [system _]
  (component/stop system)
  (System/exit 0))

(defmethod run-line :default [system line]
  (eval-and-print system line))

(def commands
  {:python ["python" "-u" "python-repl.py"]})

(defn new-system [prompt type]
  (component/system-map :repl (re/new-repl (get commands type))
                        :console (co/new-console "> ")))

(defn -main [& args]
  (let [system (-> (new-system "> " :python)
                   component/start)
        console (:console system)]
    (loop [line (co/read-line console)]
      (run-line system line)
      (recur (co/read-line console)))))
