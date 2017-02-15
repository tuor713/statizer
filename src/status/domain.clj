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
  (value [self] "Get current measurement"))

(defrecord InMemoryMeter [id name measurements]
  Component
  (id [_] id)
  (component-name [_] name)

  Meter
  (capture [self measurement]
    (assoc self :measurements (conj (:measurements self) measurement)))

  Signal
  (value [self] (last (:measurements self))))

(defn make-meter [id name]
  (InMemoryMeter. id name []))


