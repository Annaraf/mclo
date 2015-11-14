(ns mclo.core)

(def aa [1 2 3 4 5])

(def mvec [ 1 2 3 4 5 6 7 8])

(defn cube
  [x]
  (* x x x))

(defn pk 
  [a b c] 
  (fn [x] (+ (* a x x)
             (* b x)
             (* c x))))

(defn mutlak
  [x]
  (if (pos? x) x (- x)))

(defn cek
  [a b c]
  (if (> a b) "Agede" (if (> b c) "Bgede" "nolboy")))

(defn ngecek
  [x]
  (cond (> x 1000 ) "Geudeee"
    (> x 10) "nicenice"
    (= x 10) "pas"
    (< x 10) "keuceill"))

(defn nthgue
  [x y]
  (cond 
    (= 0 y) (first x)
    :else (nthgue (rest x) (dec y))))

(defn pkgue
  [a b c x?]
  (let [ diskriminan (Math/sqrt (- (* b b) (* 4 a c)))]
    [(/ (+ (- b) (if (= x? 1) diskriminan (- diskriminan))) (* 2 a))]))

(defn countgue 
  [x] 
  (if 
    (= (drop 1 x) '() ) 1 (+ 1 (countgue (drop 1 x)))))

(defn countguee
  [x] (reduce + (map #(- % (- % 1)) x )))


(fn sumgue [x] (reduce + x))

(defn dropgue
  [x y] (if (= x 0) y (dropgue (- x 1) (rest y))))

(fn lastgue [x] (first (reverse x)))

(fn oddgue [x & xs] (if (not= (rem x 2) 0) ()))

(defn maxgue 
  [x] (cond 
        (= x nil) '() 
        (= (second x) nil) (first x)
        :else (if (>= (first x) (second x)) 
                (maxgue (conj (rest (rest x)) (first x))) 
                (maxgue (rest x)))))

(fn reversegue [x] (if (= x nil) '() (conj (reversegue (butlast x)) (last x))))


(defn mantap [b]
  "Yeay"
  12
  "Biji")



(map
  #(when-not (= %2 %3) [%1 %2 %3])
    (iterate inc 0)   ; a lazy list of indecies
    [:a :b :c]
    [:a :a :a])

(defn dripi [x] 
  (let [u (seq x)] 
    (rest x)))

;; (#(when-not (= %2 %3) [%1 %2 %3]) 1 2 3)

; 1. (when-not (= 2 3) [1 2 3]) 1 2 3
; 2. (when-not (= :b :c) [:a :b :c]) [:a :b :c]
; 3. (when-not (= :a :a) [:a :a :a]) nil


(defn foo [people nama]
  (let [jumpa-leetan (first (filter #(= (:name %) nama) people))]
    (if (nil? jumpa-leetan)
      "kosong njing"
      {:name nama :data jumpa-leetan})))

(defn primague
  [n]
  (let [ akar-n (Math/sqrt n)
        iter (fn iter [i]
               (cond (> i akar-n) true
                 (zero? (rem n i)) false
                 :else (iter (+ i 2))))]
    (cond (<= n 1) false
      (= n 2) true
      (even? n) false
      :else (iter 3))))


(defn jumlah-dari-0-sampe-n
  [n]
  (loop [i 0 rest 0]
    (if (> i n) rest 
      (recur (inc i) (+ rest i)))))


(defn expogue [x y]
  (loop [i y res 1]
    (if (= 0 i) res
      (recur (dec i) (*' res x)))))

(defn faktgue 
  [x] 
  (if (= x 1) 
    1 
    (* x (faktgue (dec x)))))

 

(defn lala [a b] (nil? (a b)))

