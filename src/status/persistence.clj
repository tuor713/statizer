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
     {:readers {'range-t (fn [v] (status.types.RangeType. (:type v) (:lower v) (:upper v)
                                                          (:lower-inclusive? v)
                                                          (:upper-inclusive? v)))
                'vector-t (fn [v] (status.types.VectorType. (:type v)))
                'map-t (fn [v] (status.types.MapType. (:key-type v) (:value-type v)))
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
          (pr @(:config-ref system)))))

    (load-system [_]
      (with-open [r (io/reader file)]
        (dom/new-system (edn-read r))))))
