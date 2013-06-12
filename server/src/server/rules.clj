(ns server.rules
   (:use [clojure.set :as set :only (union)]))

;====================================================================
; Utility 
;====================================================================

(defn abs [x]
  (if (pos? x) x (- x)))

(defn with-higher-rank? [[_ r1] [_ r2]] (> r1 r2))

(defn with-higher-file? [[f1 _] [f2 _]] (> f1 f2))

(defn max-by-rank [sq1 sq2] 
  (if (with-higher-rank? sq1 sq2) sq1 sq2))

(defn min-by-rank [sq1 sq2] 
  (if (with-higher-rank? sq1 sq2) sq2 sq1))

(defn clean-horizontal-path?
  "Checks if move between two sqaures is horizontal and without any pieces across."
  [[fr-file fr-rank] [to-file to-rank] piece-squares]
  (if-not (= fr-rank to-rank)
    false
    (let [min-f (min fr-file to-file)
          max-f (max fr-file to-file)
          path (set (for [step (range 1 (- max-f min-f))] 
                      [(+ step min-f) fr-rank]))]
      (not (some path piece-squares)))))

(defn clean-vertical-path?
  "Check if move between two sqaures is vertical and without any pieces along."
  [[fr-file fr-rank] [to-file to-rank] piece-squares]
  (if-not (= fr-file to-file)
    false
    (let [min-r (min fr-rank to-rank)
          max-r (max fr-rank to-rank)
          path (set (for [step (range 1 (- max-r min-r))] 
                      [fr-file (+ step min-r)]))]
      (not (some path piece-squares)))))

(defn clean-diagonal-path?
  "Check if move between two sqaures is diagonal and without any pieces along."
  [[fr-file fr-rank :as from] [to-file to-rank :as to] piece-squares]
  (let [df (abs (- fr-file to-file))
        dr (abs (- fr-rank to-rank))]
  (if-not (= df dr)
    false
    (let [[f r :as min-rank-square] (min-by-rank from to)
          max-rank-square (max-by-rank from to)
          left-diag? (with-higher-file? min-rank-square max-rank-square)
          f-dir (if left-diag? -1 1)
          path (set (for [step (range 1 dr)]
                      [(+ (* f-dir step) f) (+ step r)]))]
      (not (some path piece-squares))))))

;====================================================================
; Common functions for all pieces
;====================================================================

(defn all-piece-squares 
  "Returns square pieces of both p1 and p2"
  [p1 p2]
  (let [p1-ps (:pieces @p1)
        p2-ps (:pieces @p2)]
    ;; keys of p1-ps are same as p2-ps
    (reduce (fn [all p]
              (set/union all (p p1-ps) (p p2-ps))) 
            #{} 
            (keys p1-ps))))

(defn piece-on-square [square piece-owner]
  "Returns the piece-owner's piece at given square"
  (let [ps (:pieces @piece-owner)]
    (first
      (filter #((% ps) square) (keys ps)))))

(defn valid-square? [[file rank]]
  (and (>= rank 0) (<= rank 7) (>= file 0) (<= file 7)))

(defn piece-on-square-moved?
  "Checks if piece-owner's piece on square was previously moved."
  [piece square piece-owner]
  {:pre [(= piece (piece-on-square square piece-owner))]}
  (some (fn [action]
          (when-let [move (or (:castling action) 
                              (:move action))]
            (= square (:to move))))
        (:actions @piece-owner)))

(declare can-move-piece?)

(defn square-under-attack?
  "Check if square is under attack by opponent's piece"
  [square player opponent]
  (let [opponent-pieces (:pieces @opponent)]
    (some (fn [p]
            (some #(can-move-piece? p % square opponent player) 
                  (p opponent-pieces)))
          (keys opponent-pieces))))

;====================================================================
; Pawn Utility
;====================================================================

(defn pawn-promotion-square? [[_ rank] pawn-color]
  (if (= :black pawn-color)
    (= rank 0)
    (= rank 7)))

(def promotion-pieces #{:queen :knight :rook :bishop})

(defn promote-pawn-at-square [square pawn-owner promotion-piece]
  (when (promotion-pieces promotion-piece)
    (dosync
     (alter pawn-owner update-in [:pieces :pawn] disj square)
     (alter pawn-owner update-in [:pieces promotion-piece] conj square)
     {:promotion {:from square
                  :piece promotion-piece}})))

(defn pawn-start-square? [[_ rank] pawn-color]
  (if (= :black pawn-color)
    (= rank 6)
    (= rank 1)))

(defn pawn-next-rank [curr-r pawn-color]
  (if (= :black pawn-color) 
    (dec curr-r)
    (inc curr-r)))

(defn pawn-prev-rank [curr-r pawn-color]
  (if (= :black pawn-color) 
    (inc curr-r)
    (dec curr-r)))

(defn passant-condition?
  "Checks for an 'en-passant' move. Assumes this is a pawn move."
  [[fr-file fr-rank :as from] [to-file to-rank :as to] pawn-owner opponent]
  (let [pawn-color (:pieces-color @pawn-owner)
        rank-ahead-fr (pawn-next-rank fr-rank pawn-color)]
    ;; Pawn should be moved on the adjacent diagonal square
    ;; and that square must be empty
    (when (and (= rank-ahead-fr to-rank)                                      
               (= 1 (abs (- fr-file to-file)))
               (not (piece-on-square to opponent)))
      (let [opponent-color (:pieces-color @opponent)
            square-ahead-to [to-file (pawn-next-rank to-rank pawn-color)]
            square-behind-to [to-file (pawn-prev-rank to-rank pawn-color)]]
        (when (and (pawn-start-square? square-ahead-to opponent-color)       ;; 1. Square ahead of the square which pawn moved to is a start position for enemy pawns
                   (= :pawn (piece-on-square square-behind-to opponent)))    ;; 2. There is an enemy pawn on square behind
          (when-let [last-move (-> (:actions @opponent) peek :move)]         ;; 3. The enemy pawn was moved there on opponent's last move
            (and (= :pawn (:piece last-move))
                 (= square-behind-to (:to last-move)))))))))
 
;====================================================================
; King Utility
;====================================================================

(defn castling-rook-square 
  "Return the square on which should be the rook in order to preform castling
  on given King's move. Assumes squares of the move are valid for castling."
  [[fr-file fr-rank] [to-file to-rank]]
  (if (> to-file fr-file) [7 fr-rank] [0 fr-rank]))

(defn castling-condition?
  "Checks for castling condition. Assumes the from - to squares are move made by the King"
  [[fr-file fr-rank :as from] [to-file to-rank :as to] king-owner opponent]
  (when (and (-> (- to-file fr-file) abs (= 2))                       ;; 1. King moved two files left or right
             (clean-horizontal-path? from                             ;; 2. The King's move is horizontal and with no pieces between
                                     to 
                                     (all-piece-squares king-owner 
                                                        opponent))
             (not (piece-on-square-moved? :king from king-owner))     ;; 3. That is the King's first move
             (not (square-under-attack? from king-owner opponent))    ;; 4. King is not currently in check
             (not (square-under-attack? to king-owner opponent)))     ;; 5. King does not end up in check
    (let [rook-square (castling-rook-square from to)
          min-file (min to-file fr-file)
          passed-square [(inc min-file) fr-rank]]
      (and (= :rook (piece-on-square rook-square king-owner))         ;; 6. There is a Rook for castling 
           (not (piece-on-square-moved? :rook 
                                        rook-square                   ;; 7. Castling Rook was never moved
                                        king-owner))                   
           (not (square-under-attack? passed-square                   ;; 8. The King doesn't pass through square that is under attack
                                      king-owner 
                                      opponent))))))

;====================================================================
; Rules of making a move
;====================================================================

(defn can-move-piece-dispatch
  "Returns piece. If the move uses invalid squares 
  or tries to position two pieces with same color on one square 
  or there is no such piece on from square belonging to piece owner 
  returns :no"
  [piece from to piece-owner opponent]
  (let [piece-keyword (keyword piece)]
	  (if (or (not (valid-square? from))
	          (not (valid-square? to))
	          (not= piece-keyword (piece-on-square from piece-owner)) 
	          (piece-on-square to piece-owner))
	    :no
	    piece-keyword)))

(defmulti can-move-piece? #'can-move-piece-dispatch)

(defmethod can-move-piece? :pawn
  [piece [fr-file fr-rank :as from] [to-file to-rank :as to] piece-owner opponent]
  (let [color (:pieces-color @piece-owner)
        rank-ahead (pawn-next-rank fr-rank color)
        second-rank-ahead (pawn-next-rank rank-ahead color)]
    (or
      ;; Pawn moved two squares ahead from it's initial position
      (and (pawn-start-square? from color)
           (= second-rank-ahead to-rank)
           (clean-vertical-path? from to (all-piece-squares piece-owner opponent)))
      ;; Pawn moved one rank ahead
      (and (= to-rank rank-ahead)
           (or
             ;; Move to unoccupied square immediately infront of it
             (and (= fr-file to-file)
                  (not (piece-on-square to opponent)))
             ;; Move to adjacent diagonal square
             (and (= 1 (abs (- fr-file to-file))) 
                  (or
                    ;; 'En-passant' move 
                    (passant-condition? from to piece-owner opponent)
                    ;; Move which attacks enemy piece
                    (piece-on-square to opponent))))))))
   
(defmethod can-move-piece? :rook
  [piece from to piece-owner opponent]
  (let [piece-squares (all-piece-squares piece-owner opponent)]
    (or (clean-horizontal-path? from to piece-squares)
        (clean-vertical-path? from to piece-squares))))

(defmethod can-move-piece? :king
  [piece [fr-file fr-rank :as from] to piece-owner opponent]
  (let [allowed-squares (->> 
                          (for [df [-1 0 1] dr [-1 0 1] 
                                :when (not= df dr)]
                            [(+ fr-file df) (+ fr-rank dr)])
                          (filter valid-square?)
                          set)]
    (or (allowed-squares to)
        (castling-condition? from to piece-owner opponent))))

(defmethod can-move-piece? :bishop
  [piece from to piece-owner opponent]
  (let [piece-squares (all-piece-squares piece-owner opponent)]
    (clean-diagonal-path? from to piece-squares)))

(defmethod can-move-piece? :knight
  [piece [fr-file fr-rank] to piece-owner opponent]
  (let [allowed-squares (->> 
                          (for [df [2 -2 1 -1] dr [2 -2 1 -1]
                                :when (not= (abs dr) (abs df))]
                            [(+ fr-file df) (+ fr-rank dr)])
                          (filter valid-square?)
                          set)]
        (allowed-squares to)))
          
(defmethod can-move-piece? :queen
  [piece from to piece-owner opponent]
  (let [piece-squares (all-piece-squares piece-owner opponent)]
    (or (clean-diagonal-path? from to piece-squares)
        (clean-horizontal-path? from to piece-squares)
        (clean-vertical-path? from to piece-squares))))
    
(defmethod can-move-piece? :no
  [& _] false)

;====================================================================
; Player makes a move functions
;====================================================================

(defn save-player-action [action player]
  (dosync 
    (alter player update-in [:actions] conj action)
    action))
  
(defn take-piece-from-square
  "Takes piece on given square from it's owner and returns the result as action"
  [square piece-owner]
  (when-let [p (piece-on-square square piece-owner)]
    (alter piece-owner update-in [:pieces p] disj square)
    {:take {:piece p
            :from square}}))

(defn do-move-piece
  "Moves piece from square to square and returns the result as action" 
  [piece from to piece-owner]
  (alter piece-owner update-in [:pieces piece] disj from)
  (alter piece-owner update-in [:pieces piece] conj to)
  {:move {:piece piece 
          :from from
          :to to}})

(defn move-piece
  "Moves piece from square to square and returns the outcome as action"
  [piece from to piece-owner opponent]
  (dosync
    (let [moved (do-move-piece piece from to piece-owner)]
      (if-let [taken (take-piece-from-square to opponent)]
        (merge moved taken)
        moved))))

(def move-hierarchy (-> (make-hierarchy)
                        (derive :rook :common)
                        (derive :bishop :common)
                        (derive :queen :common)
                        (derive :knight :common)))

(defn make-move-dispatch
  [piece from to piece-owner opponent]
  (if (can-move-piece? piece from to piece-owner opponent)
    (keyword piece)
    :invalid))

(defmulti make-move 
  #'make-move-dispatch
  :hierarchy #'move-hierarchy)

(defmethod make-move :pawn
  [piece [fr-file fr-rank :as from] [to-file to-rank :as to] piece-owner opponent]
  (let [color (:pieces-color @piece-owner)
			  action (move-piece piece from to piece-owner opponent)]
    (cond
      ;; check if pawn has moved to promotion
      (pawn-promotion-square? to color)
      (do
        (assoc action :promote {:piece :pawn :from to}))
			   
      ;; If no piece is taken and the move was diagonal this must be an 'en-passant'
      (and (not (:take action))
           (= 1 (abs (- fr-file to-file))))
      (dosync
        (let [square-behind [to-file (pawn-prev-rank to-rank color)]
              taken (take-piece-from-square square-behind opponent)]
          ;; Just in case
          (assert (and taken (= :pawn (-> taken :take :piece))))
          (merge action taken)))
     
      :else
      action)))

(defmethod make-move :king
  [piece [fr-file fr-rank :as from] [to-file to-rank :as to] piece-owner opponent]
  (let [action (move-piece piece
                           from 
                           to 
                           piece-owner 
                           opponent)]
    (if (-> (- to-file fr-file) abs (= 2))
      ;; we have castling
	    (let [rook-square (castling-rook-square from to)
	          rook-new-square (if (> to-file fr-file)
	                            [(dec to-file) to-rank]
	                            [(inc to-file) to-rank])]
       ;; Just in case
       (assert (= :rook (piece-on-square rook-square piece-owner)))
       (assert (not (piece-on-square rook-new-square piece-owner)))
       ;; This is kinda hack. Change the :move key for the castling action.
       (->>
         (move-piece :rook 
                     rook-square 
                     rook-new-square 
                     piece-owner 
                     opponent)
         (:move)
         (assoc action :castling)))
      ;; else
      action)))

(defmethod make-move :common
  [piece from to piece-owner opponent]
  (move-piece piece from to piece-owner opponent))

(defmethod make-move :invalid
  [& _]
  {:invalid-move true})

