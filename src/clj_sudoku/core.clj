(ns clj-sudoku.core
  (:require [clojure.spec.alpha :as s]
            [clojure.zip :as z]
            [com.rpl.specter :as specter]
            [clojure.java.io :as jio]))

(defn uniques [ns]
  (= (count ns) (count (set ns))))

(defn columns
  ([sudoku] (columns [] sudoku))
  ([tmp sudoku]
   (if (<= (apply min (map count sudoku)) 0)
     tmp
     (let [heads (map first sudoku)]
       (recur (conj tmp (vec heads)) (map rest sudoku))))))

(defn uniques-rows-and-cols [sudoku]
  (and (every? uniques sudoku)
       (every? uniques (columns sudoku))))

(defn rand-lsquare
  ([]
   (let [s (shuffle (range 1 10))]
     (->> (partition 3 (vec s))
          (into []))))
  ([possibles]))

(defn rand-subpermutation [ns size]
  (->> (shuffle ns)
       (take size)
       (into [])))

(defn is-lsquare? [s]
  (and (= 3 (count s))
       (not-any? #(not= 3 %) (map count s))))


(defn set-board-pos [rows i j value]
  (-> rows
      (assoc-in [j i] value)))

(defprotocol BoardCoords
  (row [this i] "returns the i'th row")
  (col [this j] "returns the j'th column")
  (subboard [this i j] "returns the sub-board that has coord i, j as upper left corner")
  (insert-subboard [this board i j] "insert subboard"))

(defprotocol Positions
  (get-position [this x y] "gets value at position x y")
  (set-position [this x y value] "sets value"))

(defn subboard? [s]
  (and (vector? s)
       (= 9 (count s))
       (uniques s)
       (every? integer? s)
       (every? #(> % 0) s)))

(defn get-pos [rows i j]
  (get-in rows [j i]))

(defn possible-values [rows i j]
  (let [all-vals (set (range 1 10))
        row      (filter #(> % 0) (nth rows j))
        col      (filter #(> % 0) (nth (columns rows) i))]
    (clojure.set/difference all-vals row col)))

(defn possible-values-matrix [rows i j]
  (let [positions (for [x (range i (+ 3 i))
                        y (range j (+ 3 j))]
                    [x y])]
    (map (fn [pos]
           {:x             (first pos)
            :y             (second pos)
            :possible-vals (possible-values rows (first pos) (second pos))})
         positions)))

(defn valid-possibility-matrix? [m]
  (let [possible (reduce clojure.set/union (map :possible-vals m))]
    (= #{} (clojure.set/intersection possible (set (range 1 10))))))

(defn valid-subboard-coord? [i j]
  (let [positions #{0 3 6}]
    (and (positions i) (positions j))))

(defrecord SubBoard [vals]
  BoardCoords
  (row [this i]
    (nth (:vals this) i))
  (col [this j]
    (->> (partition 3 (:vals this))
         (map #(nth % j))))
  (subboard [this i j]
    this)

  (insert-subboard [this board i j] board))

(defn build-subboard [vals] (->SubBoard vals))

(defrecord Sudoku
           [rows]
  BoardCoords
  (insert-subboard
    [this board i j]
    ;; {:pre [(valid-subboard-coord? i j)
    ;;        (s/valid? subboard? board)]}
    (println "hola")
    (let [ps       (for [x (range i (+ 3 i))
                         y (range j (+ 3 j))]
                     [x y])
          new-rows (loop [positions ps
                          rows2     rows]
                     (if (empty? positions)
                       rows2
                       (let [[x1 y1] (first positions)
                             board-pos (+ (* (- x1 i) 3) (- y1 j))]
                         (recur (rest positions)
                                (set-board-pos rows2 y1 x1
                                               (nth board board-pos))))))]
      (->Sudoku new-rows)))
  (row [this i]
    (nth (:rows this) i))
  (col [this i]
    (nth (columns (:rows this)) i))
  (subboard [this i j]
    (let [i1 (long (/ i 3))
          j1 (long (/ j 3))
          values (for [i2 (range 3)
                       j2 (range 3)
                       :let [i3 (+ i2 i1)
                             j3 (+ j2 j1)]]
                   (get-position this i3 j3))]
      (build-subboard (into [] values))))
        ;; (nth (row this j3) i3))))
  Positions
  (get-position [this x y] (get-in rows [y x]))
  (set-position [this x y value]
    (->Sudoku (assoc-in rows [y x] value))))

(defn empty-sudoku []
  (->Sudoku
   (->> (repeat 9
                (->> (repeat 9 0)
                     (into [])))
        (into []))))

(defn cell-neighbors [rows i j]
  ;; (println rows i j)
  (let [r    (into #{} (row rows j))
        c    (into #{} (col rows i))
        subx (long (/ i 3))
        suby (long (/ j 3))]
    (disj (clojure.set/union r c (into #{} (subboard rows i j))) 0)))

(defn available-values [^Sudoku rows i j]
  (let [neighbors (into #{} (cell-neighbors rows i j))
        all       (into #{} (range 1 10))]
    (clojure.set/difference all neighbors)))

(defn neighbors [pos x y]
  (filter #(or (= x (:x %)) (= y (:y %))) pos))

(defn new-point [i j] {:x i :y j})

(defn rand-sudoku2
  ([]
   (let [ps (for [i (range 9) j (range 9)] (new-point i j))
         e  (empty-sudoku)]
     (loop [sud           e
            remaining-pos ps
            ready-pos     []]
       (println sud)
       (if (empty? remaining-pos)
         sud
         (let [p       (first remaining-pos)
               avail   (if (and (:x p) (:y p))
                         (available-values sud (:x p) (:y p)) [])]
           ;; (println "point is " p)
           (if-not (empty? avail)
             (let  [new-val (first (shuffle avail))]
               (recur (set-position sud (:x p) (:y p) new-val)
                      (rest remaining-pos)
                      (cons p ready-pos)))
             (let [x (:x p)
                   y (:y p)
                   neis       (neighbors (into #{} ready-pos) x y)
                   sels       (into #{} neis)
                   new-remain (clojure.set/union (into #{} remaining-pos) (into #{} sels))
                   new-ready  (clojure.set/difference  (into #{} ready-pos) (into #{} sels))
                   new-sud    (loop [s  sels
                                     sd sud]
                                (if (empty? s)
                                  sd
                                  (let [p (first s)]
                                  ;; (println "point " p)
                                    (recur (rest s) (set-position sd
                                                                  (:x p) (:y p)
                                                                  0)))))]
               (recur new-sud  new-remain new-ready)))))))))

(def emty-s (empty-sudoku))

(defn insert-sb [sud sb i j]
  (let [ps (for [sb-x (range 3)
                 sb-y (range 3)
                 :let [sud-x (+ i sb-x)
                       sud-y (+ j sb-y)
                       sb-pos (+ (* 3 sb-y) sb-x)]]
             {:sb-pos sb-pos
              :sb-val (nth sb sb-pos)
              :x sud-x
              :y sud-y})]
    (reduce (fn [res p]
              (set-position
               res
               (:x p)
               (:y p)
               (:sb-val p))) sud ps)))

(def diag-s (-> emty-s
                (insert-sb (rand-subpermutation (range 1 10) 9) 0 0)
                (insert-sb (rand-subpermutation (range 1 10) 9) 3 3)
                (insert-sb (rand-subpermutation (range 1 10) 9) 6 6)))

(defn sb-freedom [sud i j]
  (let [ps (for [sb-x (range 3)
                 sb-y (range 3)
                 :let [sud-x (+ i sb-x)
                       sud-y (+ j sb-y)
                       sb-pos (+ (* 3 sb-y) sb-x)]]
             {:sb-pos sb-pos
              :sb-val (available-values sud sud-x sud-y)
              :x sud-x
              :y sud-y})]
    ps))

(defn clear-sb [sud i j]
  (println "clearing " i " " j)
  (-> sud
      (insert-sb (build-subboard (repeat 9 0)) i j)))

(defn range-covered? [freedom]
  (empty? (clojure.set/difference
           (into #{} (range 1 10))
           (into #{} (apply concat (map :sb-val freedom))))))

(defn enough-freedom? [freedom]
  (and
   (every? (comp not empty? :sb-val) freedom)
   (range-covered? freedom)))

(defn posible-sb [sud i j]
  (let [freedom (sb-freedom sud i j)

        pos-val (sort-by
                 :pos
                 (for [f freedom
                       v (:sb-val f)]
                   {:pos (:sb-pos f)
                    :value v}))]
    (assert (enough-freedom? freedom))
    (loop [result []
           tries 0]

      (if (and (= 9 (count result)) (< tries 10))
        (->> result
             (sort-by :pos)
             (map :value)
             (into []))
        (recur
         (reduce
          (fn [res c]
            (if (not-any?
                 #(or (= (:value c) (:value %))
                      (= (:pos c) (:pos %)))
                 res)
              (conj res c)
              res))
          []
          (shuffle pos-val))
         (inc tries))))))

(defn is-freedom-node [id]
  (fn [n]
    (= id (:sb-pos n))))
(defn freedom-vals [f]
  (:sb-val f))

(defn freedom-tree [freedom]
  (when-not (empty? freedom)))

(defn ftree [fd]
  (let [fz (z/seq-zip fd)]))


(defn sku []
  (let [s (empty-sudoku)
        initial (-> s
                    (insert-sb (rand-subpermutation (range 1 10) 9) 6 6))
                    ;; (insert-sb (rand-subpermutation (range 1 10) 9) 3 3)
                    ;; (insert-sb (rand-subpermutation (range 1 10) 9) 6 6))
        build-sb (fn [sud i j]
                   (println "building sb " i " " j)
                   (insert-sb sud (posible-sb sud i j) i j))
        final   (-> initial
                    (build-sb 3 3)
                    (build-sb 6 3)
                    (build-sb 0 0)
                    (build-sb 0 3)
                    (build-sb 0 6)
                    (build-sb 3 6)
                    (build-sb 6 0)
                    (build-sb 3 0))]
    final))

(defn maybe-sku []
  (try
    {:result :ok
     :data (sku)}
    (catch AssertionError _
      {:result :error})))

(defonce sudokus (atom {}))


(defn save-sudoku 
  [res path]
  (let [s (get-in res [:data :rows])]
    (with-open [ w (jio/writer path)]
       (doseq [r (doall s) :let [line (apply str r)]]
         (-> w
             (.write (str line "\n")))))))
(defn cache-sudoku [^Sudoku s]
  (println s)
  (let [
        id (str (java.util.UUID/randomUUID))
        created (java.util.Date.)]
    (->> {:id id
          :sudoku s
          :created created}
         (swap! sudokus assoc id))))

(defn sku-with-retries []
  (loop [retries 50
         res (maybe-sku)]
    (println "try: " (- 50 retries))
    (if (< retries 0)
      res
      (if (= :ok (:result res))
        (do
          (->> res
               (:data)
               (cache-sudoku)
               (swap! sudokus conj))
          res)
        (recur (dec retries) (maybe-sku))))))

(save-sudoku (first @sudokus) "archivo.txt")

(defn print-sudoku [s]
  (doseq [r (doall (:rows s))
          :let [line (clojure.string/join "   " r)]]
    (println line) 
    (println)))

