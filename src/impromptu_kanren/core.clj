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
   (contains? subst lvar) (recur (get subst lvar) subst)
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

(defn lookup*
  [lvar subst]
  (let [val (lookup lvar subst)]
    (cond
     (lvar? val) val
     (and (seq? val) (empty? val)) val
     (seq? val) (list* (lookup* (first val) subst)
                       (lookup* (rest val) subst))
     :else val)))

(defn run
  ([goal]
     (goal empty-subst))
  ([qlvar goal]
     (map (fn [subst] (lookup* qlvar subst)) (goal empty-subst))))

(defn conj*
  [& goals]
  (reduce conj goals))

(defn disj*
  [& goals]
  (reduce disj goals))

(defmacro defrel
  [name & pairs]
  `(defn ~name
     [q#]
     (condp = q#
       ~@pairs)))

(defn relato
  [rel a b]
  (== (rel a) b))

(defmacro deffact
  [name & indivs]
  `(defn ~name
     [q#]
     (contains? #{~@indivs} q#)))

(defn facto
  [fact ind]
  (== (fact ind) true))



;; example goals

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




(comment

(def vx (lvar 'x))
(def vy (lvar 'y))
(def vq (lvar 'q))

(run vq
     (conj* (== vq (list vx vy))
            (conso vx (list 2 3) (list 1 2 3))
            (conso vy (list 5 6) (list 4 5 6))))
;=> ((1 4))

(run (common-el (list 1 2 3) (list 4 3 2)))
;=> ({[x] 2} {[x] 3})

;; this should return ({[y] (1 2)}) , but it doesn't:
(run (conso 1 vx (list 1 2 3)))
;=> ()


(deffact myparent :father :mother)
(run (facto myparent :father))
;=> ({})

;; this should return ({[x] :father} {[x] :mother}) - how to achieve that?
(run (facto myparent vx))
;=> ()


(defrel parent :mike :andy :josh :ted)
(run (relato parent :mike :andy))
;=> ({})

(run (relato parent :mike vx))
;=> ({[x] :andy})

;; this should return '({[x] :mike}) - how to achieve that?
(run (relato parent vx :andy))
;IllegalArgumentException No matching clause: [x]  impromptu-kanren.core/parent


)