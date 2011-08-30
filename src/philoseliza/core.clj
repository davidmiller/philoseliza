(ns philoseliza.core
  (:use [clojure.contrib.seq-utils :only (positions)]))

(def fail
;  "Indicates Pattern match failure"
  nil)

;;
;; Predicates
;;

(defn atomic?
  "Is X atomic? e.g. not a sequence"
  [x] (not (seq? x)))

(defn starts-with?
  "Is X a sequence whose first element is Y?"
  [x y] (and (seq? x) (= (first x) y)))

(defn all?
  "Are all elements of X true ?"
  [x] (= 0 (count (filter false? x))))

(defn variable?
  "Is X a variable (a symbol beginning with ?)?"
  [x] (and (symbol? x) (= (first (str x)) \?)))

(defn segment?
  "Is X a segment (a sequence whose first element is ?*)?"
  [x] (and (seq? x) (starts-with? (first x) '?*)))

;;
;; Sequences
;; 
(defn positions-after
  "Return the positions where PRED is true for items in COLL where that position is
after START"
  [pred coll start]
  (filter (fn [x] (>= x start)) (positions pred coll)))

;"Pattern match success with no vars"
(def no-bindings {} )

(defn match-variable
  "Does VAR match INPUT? Updates and returns BINDINGS"
  [var input bindings]
  (cond (not (contains? bindings var)) (assoc bindings var input)
        (= (bindings var) input) bindings
        true fail))

(defn match-segment
  "Match a segmented PATTERN (?* ?V) against INPUT adding any matches to the
hash-map BINDINGS"
  ([pattern input bindings] (match-segment pattern input bindings 0))
  ([pattern input bindings start]
     (let [var (second (first pattern))
           pat (rest pattern)]
       (if (= pat ())
         (match-variable var input bindings)
         ;; Start with a constant
         (let [pos (first (positions-after #{(first pat)} input start))]
           (if (= nil pos)
             fail                       ;; First Item not here - bail
             (let [newbindings (pat-match pat (take pos input) bindings)]
               ;; Go longer if this fails, else sanitize
               (if (= newbindings fail)
                 (match-segment pattern input bindings (+ pos 1))
                 (match-variable var (take pos input) newbindings)))))))))

(defn pat-match
  "Does pattern match input? Any variable matches anything"
  ([pattern input] (pat-match pattern input no-bindings))
  ([pattern input bindings]
     (cond (= bindings fail fail) fail
           (variable? pattern) (match-variable pattern input bindings)
           (segment?  pattern) (match-segment pattern input bindings)
           (= pattern input) bindings
           (and (seq? pattern) (seq? input)) (pat-match
                                              (rest pattern) (rest input)
                                              (pat-match (first pattern)
                                                         (first input)
                                                         bindings))
           true fail)))
