(ns multi-repl.utils)

(defn print-flush [& args]
  (apply print args)
  (.flush *out*))

(defn print-err [& args]
  (binding [*out* *err*]
    (apply println args)))
