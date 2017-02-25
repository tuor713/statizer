(ns status.persistence
  (:require [status.domain :as dom]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defprotocol Provider
  (save-system [self system])
  (load-system [self]))

(defn import-system [persistence existing-system]
  (dom/merge-systems existing-system (load-system persistence)))

(defn edn-read [r]
  (with-open [r (java.io.PushbackReader. r)]
    (edn/read
     {:readers {'meter (fn [v] (status.domain.InMemoryMeter. (:id v) (:name v) (:type v) []))
                'computed (fn [v] (status.domain.ComputedSignal. (:id v)
                                                                 (:name v)
                                                                 (:inputs v)
                                                                 (var-get (resolve (:f v)))
                                                                 (:ftype v)))

                'range-t (fn [v] (status.types.RangeType. (:type v) (:lower v) (:upper v)
                                                          (:lower-inclusive? v)
                                                          (:upper-inclusive? v)))
                'vector-t (fn [v] (status.types.VectorType. (:type v)))
                'tuple-t (fn [v] (status.types.TupleType. v))
                'varargs-t (fn [v] (status.types.Varargs. (:types v) (:var-type v)))
                'function-t (fn [v] (status.types.FunctionType. (:domain v) (:range v)))}}
     r)))

(defn edn-read-string [s]
  (edn-read (java.io.StringReader. s)))

(defn file-persistence [file]
  (reify Provider
    (save-system [_ system]
      (with-open [w (io/writer file)]
        (binding [*out* w]
          (pr system))))

    (load-system [_]
      (with-open [r (io/reader file)]
        (edn-read r)))))
