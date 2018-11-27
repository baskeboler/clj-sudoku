(ns clj-sudoku.core)

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


(defn rand-lsquare []
  (let [s (shuffle (range 1 10))]
    (->> (partition 3 (vec s))
         (into []))))

(defn rand-subpermutation [ns size]
  (take size (shuffle ns)))

(defn is-lsquare? [s]
  (and (= 3 (count s))
       (not-any? #(not= 3 %) (map count s))))
#_(defn rand-lsquare-row []
    (let [s         (rand-lsquare)
          ns        (set (range 1 10))
          avail     (mapv #(apply disj ns %) s)
          new-s     (mapv #(rand-subpermutation % 3) avail)
          new-avail (mapv #(apply disj))]
      [s new-s]))

(def past-sudokus (atom []))

(defn rand-sudoku
  ([]
   (let [s (rand-sudoku 9 [] #{} #{} #{})]
     (swap! past-sudokus conj s)
     s))
  ([n rows sub1 sub2 sub3]
   (if (= n 0)
     rows
     (let [r (shuffle (range 1 10))
           [p1 p2 p3] (partition 3 r)]
       (if (and
             (not-any? sub1 (vec p1))
             (not-any? sub2 (vec p2))
             (not-any? sub3 (vec p3))
             (uniques-rows-and-cols (conj rows r)))
         (recur (dec n) (conj rows r)
                (if (= 6 (count sub1)) #{} (into sub1 p1))
                (if (= 6 (count sub2)) #{} (into sub2 p2))
                (if (= 6 (count sub3)) #{} (into sub3 p3)))
         (recur n rows sub1 sub2 sub3))))))


(defprotocol BoardCoords
  (row [this i])
  (col [this j])
  (subboard [this i j]))

(defrecord Sudoku [rows]
  BoardCoords
  (row [this i]
    (nth rows i))
  (col [this i]
    (nth (columns rows) i))
  (subboard [this i j]
    (for [i (range i (+ 3 i))
          j (range j (+ 3 j))]
      (nth (row this i) j))))


