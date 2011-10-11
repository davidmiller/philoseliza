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
  "Is X a segment ((?* var) pat)?"
  [x] (and (seq? x) (starts-with? (first x) '?*)))

;;
;; Sequences
;;
(defn positions-after
  "Return the positions where PRED is true for items in COLL where that position is
after START"
  [pred coll start]
  (filter (fn [x] (>= x start)) (positions pred coll)))

(defn random-elt
  "Take a random element from SEQ"
  [seq]
  (first seq))

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
             (let [newbindings (pat-match pat
                                          (map (fn [x] x) (subvec (vec input) pos))
                                          (match-variable
                                           var
                                           (map (fn [x] x) (subvec (vec input) 0 pos))
                                           bindings))]
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
           (= pattern input) bindings
           (segment?  pattern) (match-segment pattern input bindings)
           (and (seq? pattern) (seq? input)) (pat-match
                                              (rest pattern) (rest input)
                                              (pat-match (first pattern)
                                                         (first input)
                                                         bindings))
           true fail)))

(defn subst
  "For each key in the hash map VALS, replace all instance of that key
in TARGET with it's value."
  [vals target]
  (map (fn [x] (or (x vals) x)) target))

;;
;; Rules
;;
(defn make-rule
  "From the args to this function, massage a map with PATTERN as
:pattern and RESPONSES as the vector values of RESPONSES"
  [pattern & responses]
  {:pattern pattern :responses responses})


(def Rules
  '[
   {:pattern ((?* ?X) hello (?* ?y))
    :responses [(How do you do. Please state your problem)]}
   {:pattern ((?* ?x) I want (?* ?y))
    :responses [(What would it mean to you if you got ?y ?)
                (Why do you want ?y ?)
                (Suppose you got ?y soon...)]}
   {:pattern ((?* ?x) if (?* ?y))
    :reposnses [(Do you really think its likely that ?y ?)
                (Do you wish that ?y ?)
                (What do you think about ?y ?)
                (Really-- if ?y ?)]}
   {:pattern ((?* ?x) no (?* ?y))
    :responses [(Why not?)
                (You are being a bit negative)
                (Are you saying NO just to be negative ?)]}
   {:pattern ((?* ?x) I was (?* ?y))
    :responses [(Were you really?)
                (Perhaps I already knew you were ?y)
                (Why do you tell me you were ?y now ?)]}
   {:pattern ((?* ?x) I feel (?* ?y))
    :responses [(Do you often feel ?y ?)]}
   {:pattern ((?* ?x) I felt (?* ?y))
    :responses [(What other feelings do you have ?)]}
   ])

(defn use-eliza-rules
  "Find a rule with which to transform INPUT into a response"
  [input]
  (random-elt
   (filter
   (fn [x] x)
   (map (fn [rule]
         (let
             [result (pat-match (:pattern rule) input)]
           (if result
             (subst result (random-elt (:responses rule))))))
       Rules))))
