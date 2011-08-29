(ns philoseliza.core)

(defn atomic?
  "Is X atomic? e.g. not a sequence"
  [x] (not (seq? x)))

(defn all?
  "Are all elements of X true ?"
  [x] (= 0 (count (filter false? x))))

(defn simple-equal
  "Check to see if x and y are equal."
  [x y]
  (if (or (atomic? x) (atomic? y))
    (= x y)
    (and (simple-equal (first x) (first y))
         (simple-equal (rest x) (rest )))))

(defn variable?
  "Is X a variable (a symbol beginning with ?)?"
  [x] (= (first (str x)) \?))

(defn pattern-match
  "Does pattern match input? Any variable matches anything"
  [pattern input]
  (if (variable? pattern)
    true
    (if (or (atomic? pattern) (atomic? input))
      (= pattern input)
      (all?
       (map (fn [p i] (pattern-match p i)) pattern input)))))
