(ns multi-repl.core
  (:require [com.stuartsierra.component :as component]
            [multi-repl.utils :refer (print-err print-flush)]
            [multi-repl.repl :as re]
            [multi-repl.console :as co]
            [multi-repl.buffer :as bu])
  (:gen-class))

(defn buffer->key [buffer]
  (-> (str "%- " buffer)
      (clojure.string/replace "\n" "   ")
      (#(subs % 0 (min (count %) 20)))))

(defn eval-and-print [system line]
  (print-err "line" line)
  (let [{:keys [repl console]} system
        {:keys [error etype result]} (re/eval repl line)]
    (if error
      (do (co/println console "ETYPE:" etype)
          (co/println console "ERROR:" error))
      (co/println console result))))

(defmulti run-line (fn [system line lookup] line))

(defmethod run-line "%b" [system line lookup]
  (let [console (:console system)
        buffer (bu/read-buffer console)]
    (eval-and-print system buffer)
    [(buffer->key buffer) buffer]))

(defmethod run-line "exit" [system line lookup]
  (component/stop system)
  (System/exit 0))

(defmethod run-line :default [system line lookup]
  (if-let [buffer (get lookup line)]
    (do (eval-and-print system buffer)
        [line buffer])
    (do (eval-and-print system line)
        [line (clojure.string/trim line)])))

(def commands
  {:python ["python" "-u" "python-repl.py"]})

(defn new-system [prompt type]
  (component/system-map :repl (re/new-repl (get commands type))
                        :console (co/new-console "> ")))

(defn -main [& args]
  (let [system (-> (new-system "> " :python)
                   component/start)
        console (:console system)]
    (loop [line (co/read-line console)
           lookup {}]
      (let [[key entry] (run-line system line lookup)
            history (-> console :reader .getHistory)]
        (co/history-remove-last console)
        (co/history-add console key)
        (.flush history)
        (recur (co/read-line console)
               (assoc lookup key entry))))))
