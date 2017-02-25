(ns status.domain
  "Domain model for status application"
  (:require [status.types :as t]))

;; Meters, Signals, Components

(defprotocol Component
  "Components indicate externally exposed common functions for domain objects such as id and naming"
  (id [self] "Components unique id")
  (component-name [self]))

(defprotocol Meter
  "Meter for objects capturing external measurements"
  (capture [self measurement]
    "Add a new measurement and return updated meter"))

(defprotocol Signal
  "Signal for simple or aggregate signals created from measurements
  A subset of signals are indicators these represent PASS/FAIL status indicators. They are real valued signals [0,1] where 0 is total failure fail, 1 is full pass and values in between are partial failures."
  (value [self system] "Get current measurement")
  (typ [self] "Get the type for the signal"))

(defprotocol Dependent
  (dependencies [self] "Get ids of dependencies"))

(defrecord InMemoryMeter [id name typ measurements]
  Component
  (id [_] id)
  (component-name [_] name)

  Meter
  (capture [self measurement]
    (assoc self :measurements (conj (:measurements self) measurement)))

  Signal
  (value [self _] (last (:measurements self)))
  (typ [self] typ))

(defmethod print-method InMemoryMeter [x ^java.io.Writer w]
  (.write w "#meter")
  (print-method {:id (:id x), :name (:name x), :type (:typ x)} w))

(defn new-system []
  {:components {}
   :next-id 0})

(defn add-component [sys component]
  (assoc-in sys [:components (id component)] component))

(defn get-component [sys id]
  (get-in sys [:components id]))

(defn components [sys]
  (:components sys))



(defn gen-id [sys]
  (let [id (:next-id sys)]
    [id (assoc sys :next-id (inc id))]))

(defn make-meter
  ([sys name] (make-meter sys name t/TAny))
  ([sys name typ]
   (let [[id sys] (gen-id sys)
         comp (InMemoryMeter. id name typ [])]
     [comp (add-component sys comp)])))

(defrecord ComputedSignal [id name inputs f ftype]
  Component
  (id [_] id)
  (component-name [_] name)

  Signal
  (value [self sys]
    (let [ins (map #(value (get-component sys %) sys) inputs)]
      (when-not (some nil? ins)
        (apply f ins))))
  (typ [self] (t/fn-domain ftype))

  Dependent
  (dependencies [_] (set inputs)))

(defmethod print-method ComputedSignal [x ^java.io.Writer w]
  (.write w "#computed")
  (print-method {:id (:id x)
                 :name (:name x)
                 :inputs (:inputs x)
                 :f (:var (meta (:f x)))
                 :ftype (:ftype x)}
                w))

(defn make-computed-signal
  ([sys name inputs function]
   (make-computed-signal sys name inputs function (t/fn-type (t/varargs-type [] t/TAny) t/TAny)))
  ([sys name inputs function ftype]
   (let [[id sys] (gen-id sys)
         missing (remove (partial get-component sys) inputs)
         _ (when (seq missing)
             (throw (IllegalArgumentException. (str "Trying to create computed signal with undefined inputs: "
                                                    (vec missing)))))
         _ (when-not (t/fn-applicable? ftype (apply t/tuple-type (map #(typ (get-component sys %)) inputs)))
             (throw (IllegalArgumentException. (str "Input types do not match, expected: "
                                                    (t/fn-domain ftype)
                                                    ", got: " (apply t/tuple-type (map #(typ (get-component sys %)) inputs))))))
         comp (ComputedSignal. id name inputs function ftype)]
     [comp (add-component sys comp)])))

(defn make-min-signal
  [sys name inputs]
  (make-computed-signal sys name inputs
                        (with-meta min {:var 'clojure.core/min})
                        (t/fn-type (t/varargs-type [] t/TNumber) t/TNumber)))

(defn make-max-signal
  [sys name inputs]
  (make-computed-signal sys name inputs
                        (with-meta max {:var 'clojure.core/max})
                        (t/fn-type (t/varargs-type [] t/TNumber) t/TNumber)))

(defn sys-capture [sys id value]
  (let [meter (get-component sys id)
        new-meter (capture meter value)]
    (add-component sys new-meter)))

(defn merge-systems [sys1 sys2]
  (first
   (reduce
    (fn [[sys idmapping] component]
      (let [[id sys] (gen-id sys)
            ncomponent (cond
                         (instance? InMemoryMeter component)
                         (assoc component :id id)

                         (instance? ComputedSignal component)
                         (assoc component
                                :id id
                                :inputs (mapv idmapping (:inputs component)))

                         :else (throw (IllegalArgumentException.
                                       (str "Don't know how to merge component: " component))))]
        [(add-component sys ncomponent)
         (assoc idmapping (:id component) id)]))
    [sys1 {}]
    (sort-by id (vals (:components sys2))))))

