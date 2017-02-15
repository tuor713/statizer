(ns status.domain
  "Domain model for status application")

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
  (value [self system] "Get current measurement"))

(defrecord InMemoryMeter [id name measurements]
  Component
  (id [_] id)
  (component-name [_] name)

  Meter
  (capture [self measurement]
    (assoc self :measurements (conj (:measurements self) measurement)))

  Signal
  (value [self _] (last (:measurements self))))

(defn new-system []
  {:components {}
   :next-id 0})

(defn add-component [sys component]
  (assoc-in sys [:components (id component)] component))

(defn get-component [sys id]
  (get-in sys [:components id]))

(defn gen-id [sys]
  (let [id (:next-id sys)]
    [id (assoc sys :next-id (inc id))]))

(defn make-meter [sys name]
  (let [[id sys] (gen-id sys)
        comp (InMemoryMeter. id name [])]
    [comp (add-component sys comp)]))

(defrecord ComputedSignal [id name inputs f]
  Component
  (id [_] id)
  (component-name [_] name)

  Signal
  (value [self sys]
    (let [ins (map #(value (get-component sys %) sys) inputs)]
      (when-not (some nil? ins)
        (apply f ins)))))

(defn make-computed-signal
  [sys name inputs function]
  (let [[id sys] (gen-id sys)
        missing (remove (partial get-component sys) inputs)
        _ (when (seq missing)
            (throw (IllegalArgumentException. (str "Trying to create computed signal with undefined inputs: "
                                                   (vec missing)))))
        comp (ComputedSignal. id name inputs function)]
    [comp (add-component sys comp)]))

(defn make-min-signal
  [sys name inputs]
  (make-computed-signal sys name inputs min))

(defn make-max-signal
  [sys name inputs]
  (make-computed-signal sys name inputs max))

(defn sys-capture [sys id value]
  (let [meter (get-component sys id)
        new-meter (capture meter value)]
    (add-component sys new-meter)))

