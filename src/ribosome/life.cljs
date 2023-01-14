(require '[clojure.set :as set]
         '[quil.core :as q]
         '[quil.middleware :as m])

;; board will be represented as a set of pair vectors showing
;; alive cells

(def glider-board #{[1 0] [2 1] [0 2] [1 2] [2 2]})

(defn random-board [num width height]
  (set (take num (shuffle (for [x (vec (range width)) y (vec (range height))] (vector x y))))))

(defn neighbors [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] 
        :when (not= (vector dx dy) (vector 0 0))]
    [(+ x dx) (+ y dy)]))

;; note about this: 
;; neighbors explicitly does not include itself
;; that means each cell is only counted here if it has a neighbor
;; that should be fine, because singular cells will die anyways
(defn all-neighbors [board]
  (reduce set/union (map set (for [cell board]
                           (neighbors cell)))))

(defn alive-neighbors [board [x y]]
  (count (filter #(contains? board % ) (neighbors [x y]))))

(defn alive? [board [x y]]
  (let [n (alive-neighbors board [x y])]
    (if (contains? board [x y])
      (or (= n 2) (= n 3))
      (= n 3))))

(defn tick [board] 
 (set (filter #(alive? board %) (all-neighbors board))))



(def scale 4)

(defn setup []
  (q/frame-rate 20)
  (q/color-mode :hsb)
  (q/no-stroke)
  (q/no-smooth)
  {:board (random-board 6000 200 200)})

(defn update-state [state]
  {:board (tick (:board state))})

(defn draw-state [state]
  (q/background 10)
  (doseq [[x y] (:board state)]
    (q/fill (int (* 255 (/ y 200))) 255 255)
    (q/rect (* x scale) (* y scale) scale scale)))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(q/defsketch GoL
  :title "Game of Life"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
