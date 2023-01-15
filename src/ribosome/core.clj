(require '[clojure.set :as set]
         '[quil.core :as q]
         '[quil.middleware :as m])

(def width 200)
(def height 200)
(def scale 4)



(defn to-board-index [x y]
  (+ x (* y width)))

(defn board-index-x [board-index]
  (mod board-index width))

(defn board-index-y [board-index]
  (int (/ board-index width)))

(defn from-board-index [board-index]
  [(board-index-x board-index) (board-index-y board-index)])

(defn board-index-in-dir [board-index [dx dy]]
  (to-board-index (+ dx (board-index-x board-index)) (+ dy (board-index-y board-index))))

;;todo, could use board-index-in-dir to simplify this
(defn get-neighbors [board-index]
  (let [x (board-index-x board-index) y (board-index-y board-index)] 
    (for [dx [-1 0 1] dy [-1 0 1]
        :when (not= (vector dx dy) (vector 0 0))] 
       (to-board-index (+ x dx) (+ y dy)))))
  
(defn neighbor-cells [board board-index]
  (filter identity (for [n (get-neighbors board-index)]
    (get board n))))



(defn get-actions [board]
  (vec (filter seq (for [[board-index cell] board]
    ((cell :func) board board-index)))))

(defn apply-action [board action]
  (if (= (action :type) "spawn")
    (assoc board (action :pos) (action :cell))
    board))

(def ^:dynamic *running-board* {})
(defn apply-actions [board raw-actions]
  (binding [*running-board* board]
    (let [actions (shuffle raw-actions)]
      (doseq [action actions]
        (set! *running-board* (apply-action *running-board* action))))
    *running-board*))

(defn tick [board]
  (apply-actions board (get-actions board)))

;;these return actions
(defn cell-grower [board board-index]
  (let [pos (board-index-in-dir board-index [1 0])]
    (if (nil? (get board pos))
      {:type "spawn" :pos pos :cell {:func cell-grower}}
      {})))

(def test-board {0 {:func cell-grower}, 1 {:func cell-grower}, 
                200 {:func cell-grower}, 201 {:func cell-grower}})


;;cell colors
(def cell-colors {cell-grower [255 0 0]})

(defn get-color [cell]
  (get cell-colors (cell :func)))


(defn setup []
  (q/frame-rate 20)
  (q/color-mode :rgb)
  (q/no-stroke)
  (q/no-smooth)
  ;; {:board (random-board 6000 200 200)})
  {:board test-board})

(defn update-state [state]
  {:board (tick (:board state))})

(defn draw-state [state]
  (q/background 10) 
  (doseq [[pos cell] (:board state)] 
    (let [x (board-index-x pos) y (board-index-y pos)] 
      (q/fill (get-color cell))
      (q/rect (* x scale) (* y scale) scale scale))))


#_{:clj-kondo/ignore [:unresolved-symbol]}
(q/defsketch GoL
  :title "Game of Life"
  :size [(* width scale) (* height scale)]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
