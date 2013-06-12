(ns client.utils
  (:use [blancas.kern.core]
        [client.config]))

(defn coord-to-cell
  "Converts coordinate to cell"
  [[x y]]
  [(int (/ y square-dim))
   (int (/ x square-dim))])

(defn cell-center-coord
  "Returns center coordinate of the cell"
  [[board-row board-col]]
  (let [half-square-dim (int (/ square-dim 2))]
    [(+ (* board-col square-dim) half-square-dim)
     (+ (* board-row square-dim) half-square-dim)]))
  
(def chess-square-parser
  (bind [[f r] (<*> letter dec-num)]
    (return [(- (int f) 97) (- 8 r)  ])))

(defn chess-square-to-cell-center
  "Converts the string representation of chess square to center 
  coordinates of corresponding square on the board"
  [chess-square]
  (let [[board-col board-row] (:value (parse chess-square-parser 
                                                 chess-square))]
    (cell-center-coord [board-row board-col])))

(defn coord-to-chess-square
  "Converts coordinate to string representation of chess square, e.g g5"
  [coord]
  (let [[row col] (coord-to-cell coord)]
    (format "%c%d" (char (+ col 97)) (- 8 row))))

