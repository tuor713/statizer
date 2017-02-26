(ns status.domain
  "Domain model for status application"
  (:require [status.types :as t]
            [clojure.spec :as spec]))

;; Specs for data in the system

;; <Components: Meters and Signals>

;; Data first representation, can be used for serialization and external APIs as well

(spec/def ::id integer?)
(spec/def ::name #(or (symbol? %) (keyword? %) (string? %)))
(spec/def ::value (constantly true))
(spec/def ::default-value ::value)
(spec/def ::type #(satisfies? t/Type %))

(spec/def ::dependencies (spec/* ::id))
(spec/def ::function-id #{:min :max :weighted})
(spec/def ::function (spec/keys :req [::function-id
                                      ::dependencies
                                      ::type]
                                :opt [::parameters]))

(spec/def ::timestamp integer?)
(spec/def ::measurement (spec/keys :req [::timestamp ::value]))
(spec/def ::measurements (spec/map-of ::id (spec/* ::measurement)))

(spec/def ::component-spec (spec/keys :req [::name
                                            (or ::type ::function)]
                                      :opt [::default-value]))
(spec/def ::component (spec/keys :req [::id
                                       ::name
                                       (or ::type ::function)]
                                 :opt [::default-value]))

(spec/def ::next-id ::id)
(spec/def ::components (spec/map-of ::id ::component))
(spec/def ::configuration (spec/keys :req [::next-id
                                           ::components]))

(spec/def ::values (spec/map-of ::id ::value))
(spec/def ::state (spec/keys :req [::measurements
                                   ::values]))

;; Read-only interface
(defprotocol Component
  (id [self] "Gets the id of the component")
  (derived? [self] "Determines whether the component is an external data point or derived")
  (component-name [self] "Gets the component name")
  (dependencies [self] "Gets the dependencies, if any, of the component")
  (typ [self] "Gets the type of the component"))

(extend-protocol Component
  clojure.lang.IPersistentMap
  (id [self] (::id self))
  (derived? [self] (nil? (::type self)))
  (component-name [self] (::name self))
  (dependencies [self]
    (if (derived? self)
      (set (get-in self [::function ::dependencies]))
      #{}))
  (typ [self]
    (if (derived? self)
      (t/fn-range (get-in self [::function ::type]))
      (::type self))))

;; Read-write interface
(defprotocol StatusSystem
  (create-component [self spec] "Returns id of component created")
  (get-component [self id] "Returns the component spec")
  (components [self] "Get all components")
  (capture [self id value] "Adds a new measurement")
  (value [self id] "Gets the current value of a component")
  (measurements [self id] "Get list of measurements for component"))

(defmulti component-function #(get-in % [::function ::function-id]))
(defmethod component-function :min [spec] min)
(defmethod component-function :max [spec] max)
(defmethod component-function :weighted [spec]
  (let [weights (get-in spec [::function ::parameters ::weights])]
    (fn [& inputs]
      (reduce + (map * inputs weights)))))

(defmulti validate-function-spec #(get-in % [::function-id]))
(defmethod validate-function-spec :default [spec] true)
(defmethod validate-function-spec :weighted [fspec]
  (when-not (= (count (::dependencies fspec))
               (count (get-in fspec [::parameters ::weights])))
    (throw (IllegalArgumentException. (str "Number of inputs and weights does not match: "
                                           (::dependencies fspec)
                                           " " (get-in fspec [::parameters ::weights]))))))

(defn- get-value [state id]
  (if-let [v (get-in state [::values id])]
    (if (fn? v)
      (v state)
      v)))

(defn- component-value [spec]
  (let [default (::default-value spec)]
    (if (derived? spec)
      (let [f (component-function spec)]
        (fn [state]
          (or (let [vals (map (partial get-value state)
                              (get-in spec [::function ::dependencies]))]
                (when (every? #(not= nil %) vals)
                  (apply f vals)))
              default)))
      default)))

(defn- validate-component [cfg spec]
  (when (derived? spec)
    (let [ftype (get-in spec [::function ::type])
          arg-types (apply t/tuple-type (map #(typ (get-in cfg [::components %]))
                                             (get-in spec [::function ::dependencies])))]
      (when (not (t/fn-applicable? ftype arg-types))
        (throw (IllegalArgumentException.
                (str "Input types do not match, expected: "
                     (t/fn-domain ftype) ", got: " arg-types))))

      (validate-function-spec (::function spec)))))

(defrecord InMemoryStatusSystem [config-ref state-ref]
  StatusSystem
  (create-component [self spec]
    (validate-component @config-ref spec)
    (dosync
     (let [id (::next-id @config-ref)]
       (alter config-ref
              (fn [cfg]
                (-> cfg
                    (assoc-in [::components id] (assoc spec ::id id))
                    (assoc ::next-id (inc id)))))
       (alter state-ref
              assoc-in [::values id] (component-value spec))
       id)))

  (get-component [self id]
    (get-in @config-ref [::components id]))

  (components [self]
    (vals (::components @config-ref)))

  (capture [self id value]
    (when-not (contains? (::components @config-ref) id)
      (throw (IllegalArgumentException. (str "Undefined component: " id))))
    (dosync
     (alter state-ref
            (fn [state]
              (-> state
                  (assoc-in [::values id] value)
                  (update-in [::measurements id ]
                             (fn [vals]
                               (conj vals {::value value
                                           ::timestamp (System/currentTimeMillis)}))))))))

  (value [self id]
    (get-value @state-ref id))

  (measurements [self id]
    (get-in @state-ref [::measurements id])))

(defn new-system
  ([] (new-system {::next-id 0 ::components {}}))
  ([cfg]
   (let [sys (InMemoryStatusSystem. (ref {::next-id 0 ::components {}})
                                    (ref {::measurements {} ::values {}}))]
     (doseq [c (sort-by id (vals (::components cfg)))]
       (create-component sys c))
     sys)))



(defn merge-systems [sys1 sys2]
  (first
   (reduce
    (fn [idmapping component]
      (let [re-mapped (if (derived? component)
                        (update-in component [::function ::dependencies]
                                   (fn [ids] (map idmapping ids)))
                        component)
            new-id (create-component sys1 re-mapped)]
        (assoc idmapping
               (id component)
               new-id)))
    {}
    (sort-by id (components sys2))))
  sys1)

