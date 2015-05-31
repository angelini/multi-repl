(ns multi-repl.buffer
  (:require [multi-repl.utils :refer (print-err)]
            [multi-repl.console :as co]))

;; TODO: Add support for backspace
;; TODO: Implement start-buffer with arg for initial rows

(defn newline? [c] (= c 13))
(defn escape?  [c] (= c 27))
(defn percent? [c] (= c 37))

(defn min-dec [x minimum]
  (max (dec x) minimum))

(defn max-inc [x maximum]
  (min (inc x) maximum))

(defn insert [vec pos item]
  (apply conj (subvec vec 0 pos) item (subvec vec pos)))

(defn insert-char [buffer [x y] c]
  (update-in buffer [y] insert x (char c)))

(defn inc-x [cursor] (update-in cursor [0] inc))
(defn dec-x [cursor] (update-in cursor [0] min-dec 0))
(defn inc-y [cursor] (update-in cursor [1] inc))
(defn dec-y [cursor] (update-in cursor [1] min-dec 0))

(defn set-x [cursor n] (assoc cursor 0 n))
(defn set-y [cursor n] (assoc cursor 1 n))

(defn read-buffer [console]
  (loop [buffer [[]]
         cursor [0 0]
         c (co/read-char console)]
    (co/print console (char c))
    (cond
      (escape? c) (let [[c1 c2] [(co/read-char console)
                                 (co/read-char console)]]
                    (co/print console (char c1))
                    (co/print console (char c2))
                    (condp = [c1 c2]
                      [91 65] (recur buffer
                                     (dec-y cursor)
                                     (co/read-char console))
                      [91 66] (recur buffer
                                     (inc-y cursor)
                                     (co/read-char console))
                      [91 67] (recur buffer
                                     (inc-x cursor)
                                     (co/read-char console))
                      [91 68] (recur buffer
                                     (dec-x cursor)
                                     (co/read-char console))))
      (newline? c) (do (co/print-escape-seq console \S)
                       (recur (conj buffer [])
                              (-> cursor inc-y (set-x 0))
                              (co/read-char console)))
      (percent? c) (let [c1 (co/read-char console)]
                     (if (= c1 114)
                       (clojure.string/join "\n" (map #(apply str %) buffer))
                       (recur buffer cursor c1)))
      :else (recur (insert-char buffer cursor c)
                   (update-in cursor [0] inc)
                   (co/read-char console)))))
