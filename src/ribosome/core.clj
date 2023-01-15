(require '[clojure.set :as set]
         '[quil.core :as q]
         '[quil.middleware :as m])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN VARS

(def width 200)
(def height 200)
(def scale 4)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOARD & CELL FUNCS

(defn to-board-index [x y]
  (+ x (* y width)))

(defn random-index []
  (rand-int (* width height)))

(defn board-index-x [board-index]
  (mod board-index width))

(defn board-index-y [board-index]
  (int (/ board-index width)))

(defn random-dir []
  (first (shuffle [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])))

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

(defn count-neighbors [board board-index]
  (count (neighbor-cells board board-index)))

(defn count-neighbors-of-func [board board-index func]
  (count (filter #(= func (% :func)) (neighbor-cells board board-index))))

(defn func-at-index [board board-index]
  (if (some? (get board board-index))
    ((get board board-index) :func)
    nil))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ACTIONS

(defn get-actions [board]
  (vec (filter seq (for [[board-index cell] board]
    ((cell :func) board board-index cell)))))

(defn apply-action [board action] 
  (if (true? (action :kill))
    (dissoc board (action :pos))
    (assoc board (action :pos) (action :cell)))) 

(def ^:dynamic *running-board* {})
(defn apply-actions [board raw-actions]
  (binding [*running-board* board]
    (let [actions (shuffle raw-actions)]
      (doseq [action actions]
        (set! *running-board* (apply-action *running-board* action))))
    *running-board*))

(defn tick [board]
  (apply-actions board (get-actions board)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CELLS

(defn cell-stationary [_ _ _]
  {})

(defn cell-grower [board board-index cell]
  (let [facing (board-index-in-dir board-index (cell :dir))]
    (if (nil? (get board facing))
      {:pos facing :cell {:func cell-grower :dir (cell :dir)}}
      {:pos board-index :cell {:func cell-stationary :dir (cell :dir)}})))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOARDS & HELPERS
;;note, board positions are hard-coded to the width/height of the board

;;assumes symmetrical board, width == height
(def ^:dynamic *running-boarder* {})
(defn apply-board-boarder [board]
  (binding [*running-boarder* board]
    (doseq [n (range width)]
      (set! *running-boarder* (merge *running-boarder* {(to-board-index n 0) {:func cell-stationary :dir [0 0]}}))
      (set! *running-boarder* (merge *running-boarder* {(to-board-index 0 n) {:func cell-stationary :dir [0 0]}}))
      (set! *running-boarder* (merge *running-boarder* {(to-board-index n (- height 1)) {:func cell-stationary :dir [0 0]}}))
      (set! *running-boarder* (merge *running-boarder* {(to-board-index (- width 1) n) {:func cell-stationary :dir [0 0]}})))
    *running-boarder*))

(defn random-grower []
  (assoc {} (random-index) {:func cell-grower :dir (random-dir)}))

(def test-board {0 {:func cell-grower :dir [-1 -1]}, 1 {:func cell-grower :dir [1 0]}, 
                200 {:func cell-grower :dir [0 1]}, 201 {:func cell-grower :dir [1 1]}})

(defn random-board [] (merge (random-grower) (random-grower) (random-grower) (random-grower) (random-grower)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RENDERING & QUIL

;;cell colors
(def cell-colors {cell-grower [255 0 0]
                  cell-stationary [100 100 100]})

(defn get-color [cell]
  (get cell-colors (cell :func)))


(defn setup []
  (q/frame-rate 60)
  (q/color-mode :rgb)
  (q/no-stroke)
  (q/no-smooth)
  ;; {:board (random-board 6000 200 200)})
  {:board (apply-board-boarder (random-board))})

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
