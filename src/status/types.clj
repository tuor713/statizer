(ns status.types
  "Type-safety for the domain. If possible should actually reuse machinery from clojure.core.typed or plumatic/schema"
  (:require [clojure.string :as s]))

(defprotocol Type
  (substitutes? [self other] "Can other be substituted for self?"))

;; Primitive types

(def TAny ::any)
(def TNumber ::number)

(derive ::number ::any)

(defrecord RangeType [type lower upper lower-inclusive? upper-inclusive?]
  Type
  (substitutes? [_ other]
    (if (instance? RangeType other)
      (and (substitutes? type (:type other))
           (or (< lower (:lower other))
               (and (= lower (:lower other))
                    (or lower-inclusive?
                        (not (:lower-inclusive? other)))))
           (or (> upper (:upper other))
               (and (= upper (:upper other))
                    (or upper-inclusive?
                        (not (:upper-inclusive? other))))))
      false))

  Object
  (toString [_]
    (str (if lower-inclusive? "[" "]")
         type ": "
         lower ".." upper
         (if upper-inclusive? "]" "["))))

(defn range-type
  ([lower upper]
   (range-type TNumber lower upper true true))
  ([type lower upper lower-inclusive? upper-inclusive?]
   (RangeType. type lower upper lower-inclusive? upper-inclusive?)))

(def TIndicator (range-type 0 1))

(extend-protocol Type
  clojure.lang.Keyword
  (substitutes? [self other]
    (cond
      (= self ::any)
      true

      (instance? RangeType other)
      (substitutes? self (:type other))

      :else
      (and (keyword? other) (isa? other self)))))

(defrecord VectorType [type]
  Type
  (substitutes? [_ other]
    (if (instance? VectorType other)
      (substitutes? type (:type other))
      false))

  Object
  (toString [_]
    (str "[" type "*" "]")))

(defn vector-type [t] (VectorType. t))

(defn substitute-all? [atypes btypes]
  (and (= (count atypes) (count btypes))
       (every? (fn [[a b]] (substitutes? a b))
               (map vector atypes btypes))))

(defrecord TupleType [types]
  Type
  (substitutes? [_ other]
    (if (instance? TupleType other)
      (substitute-all? types (:types other))
      false))

  Object
  (toString [_]
    (str "(" (s/join ", " (map str types)) ")")))

(defn tuple-type [& ts]
  (TupleType. ts))

(defrecord Varargs [types var-type]
  Type
  (substitutes? [_ other]
    (cond
      (instance? TupleType other)
      (and (<= (count types) (count (:types other)))
           (substitute-all? types (take (count types) (:types other)))
           (every? #(substitutes? var-type %) (drop (count types) (:types other))))

      (instance? Varargs other)
      (and (<= (count types) (count (:types other)))
           (substitute-all? types (take (count types) (:types other)))
           (every? #(substitutes? var-type %) (drop (count types) (:types other)))
           (substitutes? var-type (:var-type other)))

      :else false))

  Object
  (toString [_]
    (str "(" (s/join ", " (map str types))
         (when (seq types) ", ")
         var-type "*"
         ")")))

(defn varargs-type [types var-type]
  (Varargs. types var-type))

;; Function types

(defrecord FunctionType [domain range]
  Type
  (substitutes? [_ other]
    (if (instance? FunctionType other)
      (and (substitutes? (:domain other) domain)
           (substitutes? range (:range other)))
      false))

  Object
  (toString [_]
    (str domain " -> " range)))

(defn fn-type [domain range]
  (when-not (or (instance? TupleType domain) (instance? Varargs domain))
    (throw (IllegalArgumentException. "Domain needs to be tuple or varargs type")))
  (FunctionType. domain range))

(defn fn-range [ftype]
  (:range ftype))

(defn fn-domain [ftype]
  (:domain ftype))

(defn fn-applicable? [ftype input-type]
  (substitutes? (fn-domain ftype) input-type))

