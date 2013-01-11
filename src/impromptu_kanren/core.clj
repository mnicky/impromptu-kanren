(ns impromptu-kanren.core)

(defn fail
  [x]
  '())

(defn succeed
  [x]
  (list x))

(defn disj
  [f1 f2]
  (fn [x] (flatten (list* (f1 x) (f2 x)))))

(defn conj
  [f1 f2]
  (fn [x] (flatten (apply list* (map f2 (f1 x))))))

(comment
  
((disj
  (disj fail succeed)
  (conj
   (disj (fn [x] (succeed (+ x 1)))
         (fn [x] (succeed (+ x 10))))
   (disj succeed succeed)))
 100)

)

(defn lvar
  [x]
  [x])

(defn lvar?
  [x]
  (vector? x))

(def empty-subst {})

(defn ext-s
  [var val subst]
  (assoc subst var val))

(defn lookup
  [lvar subst]
  (cond
   (not (lvar? lvar)) lvar
   (contains? subst lvar) (recur (get subst lvar) (dissoc subst lvar))
   :else lvar))

(defn unify
  [t1 t2 subst]
  (let [t1 (lookup t1 subst)
        t2 (lookup t2 subst)]
    (cond
     (= t1 t2) subst
     (lvar? t1) (ext-s t1 t2 subst)
     (lvar? t2) (ext-s t2 t1 subst)
     (every? seq? [t1 t2])
       (let [subst (unify (first t1) (first t2) subst)]
         (and subst (unify (rest t1) (rest t2) subst)))
     :else false)))

(defn ==
  [t1 t2]
  (fn [subst]
    (let [unified (unify t1 t2 subst)]
      (cond
       unified (succeed unified)
       :else (fail subst)))))

(defn run
  [goal]
  (goal empty-subst))

(defn membero
  [el seq]
  (if (empty? seq)
    fail
    (disj
     (== el (first seq))
     (membero el (rest seq)))))

(defn common-el
  [seq1 seq2]
  (conj
   (membero (lvar 'x) seq1)
   (membero (lvar 'x) seq2)))

(defn conso
  [a b l]
  (== (cons a b) l))