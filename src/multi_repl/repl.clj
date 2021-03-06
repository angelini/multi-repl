(ns multi-repl.repl
  (:refer-clojure :exclude [eval])
  (:require [msgpack.core :as msgpack]
            [me.raynes.conch.low-level :as sh]
            [com.stuartsierra.component :as component]
            [multi-repl.utils :refer (print-err)])
  (:import [java.io DataOutputStream DataInputStream]))

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

(defn test-for-error [repl]
  (let [proc (:proc repl)]
    (try
      (.exitValue (-> repl :proc :process))
      (slurp (:err proc))
      (catch IllegalThreadStateException e false))))

(defn eval [repl statement]
  (let [{:keys [sink source proc]} repl
        message {:statement statement}]
    (if-let [error (test-for-error repl)]
      (do (print-err ";; REPL error")
          (print-err error)
          (System/exit 1))
      (do (msgpack/pack-stream message sink)
          (.flush sink)
          (format-response (msgpack/unpack-stream source))))))
