(ns server.game
  (:use [clojure.set :as set :only (union)]
        [cheshire.core :only (parse-string generate-string)]
        [blancas.kern.core :as kern]))

(import '(java.io PrintWriter InputStreamReader BufferedReader))

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

(defn clean-horizontal-path? [[from-f from-r] [to-f to-r] piece-squares]
  (if-not (= from-r to-r)
    false
    (let [min-f (min from-f to-f)
          max-f (max from-f to-f)
          path (set (for [step (range 1 (- max-f min-f))] 
                      [(+ step min-f) from-r]))]
      (not (some path piece-squares)))))

(defn clean-vertical-path? [[from-f from-r] [to-f to-r] piece-squares]
  (if-not (= from-f to-f)
    false
    (let [min-r (min from-r to-r)
          max-r (max from-r to-r)
          path (set (for [step (range 1 (- max-r min-r))] 
                      [from-f (+ step min-r)]))]
      (not (some path piece-squares)))))

(defn clean-diagonal-path? [from to piece-squares]
  (let [[from-f from-r] from
        [to-f to-r] to
        delta-f (abs (- from-f to-f))
        delta-r (abs (- from-r to-r))]
  (if-not (= delta-f delta-r)
    false
    (let [min-rank-square (min-by-rank from to)
          max-rank-square (max-by-rank from to)
          left-diag? (with-higher-file? min-rank-square max-rank-square)
          f-dir (if left-diag? -1 1)
          [f r] min-rank-square
          path (set (for [step (range 1 delta-r)]
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

(defn piece-at-square [square piece-owner]
  (let [ps (:pieces @piece-owner)]
    (first
      (filter #((% ps) square) (keys ps)))))

(defn valid-square? [[file rank]]
  (and (>= rank 0) (<= rank 7) (>= file 0) (<= file 7)))

(defn piece-on-square-moved?
  "Checks if player made a move from given square"
  [square piece-owner]
  (some (fn [action]
          (when-let [move (:move action)]
            (= square (:from move))))
        (:actions @piece-onwer)))

(declare can-move-piece?)

(defn square-under-attack?
  "Check if square is under attack by opponent"
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

;; TODO change to acquire the promotion piece as argument.
(defn promote-pawn-at-square [square pawn-owner]
  (dosync
    (alter pawn-owner update-in [:pieces :pawn] disj square)
    (alter pawn-owner update-in [:pieces :queen] conj square)))

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
  "Returns true if opponent last move was pawn two ranks ahead at passant square."
  [[file rank :as passant-square] opponent]
  (let [color (:color @opponent)
        two-ranks-back (-> (pawn-prev-rank rank color) 
                           (pawn-prev-rank color))]
    (when (and (pawn-start-square? [file two-ranks-back] color)
               (= :pawn (piece-at-square passant-square opponent)))
      (let [last-move (-> (:actions @opponent) peek :move)]
        (and (= :pawn (:piece last-move))
             (= passant-square (:to last-move)))))))

;====================================================================
; King Utility
;====================================================================

(defn castling-rook-square 
  "Return the square on which should be the rook in order to preform castling
  on given move. Assumes squares of the move are valid for castling."
  [[fr-file fr-rank] [to-file to-rank]]
  (if (> to-file fr-file) [7 fr-rank] [0 fr-rank]))

(defn castling-condition?
  "Checks for castling condition. Assumes the from - to squares are move made by the King"
  [[fr-file fr-rank :as from] [to-file to-rank :as to] king-owner opponent]
  (when (and (-> (- to-file fr-file) abs (= 2))                       ;; 1. King moved two files left or right
             (clean-horizontal-path? from                             ;; 2. The King's move is horizontal and with no pieces between
                                     to 
                                     (all-piece-squares piece-owner 
                                                        opponent))
             (not (piece-on-square-moved? from king-owner))           ;; 3. That is the king's first move
             (not (square-under-attack? from king-owner opponent))    ;; 4. King is not currently in check
             (not (square-under-attack? to king-owner opponent))      ;; 5. King does not end up in check
    (let [rook-square (castling-rook-square from to)
          min-file (min to-file fr-file)
          passed-square #{[(+ min-file 1) fr-rank]}]
      (and (not (piece-on-square-moved? rook-square                   ;; 5. Castling Rook was never moved
                                        king-owner))                   
           (not (square-under-attack? passed-square                   ;; 6. The King doesn't pass through square that is under attack
                                      king-owner 
                                      opponent)))))))     

;====================================================================
; Rules of making a move
;====================================================================

(defn can-move-piece-dispatch
  "Returns piece keyword. If the move tries to position two pieces 
  with same color on one square returns :no"
  [piece from to piece-owner opponent]
  (if (or (not (valid-square? from))
          (not (valid-square? to))
          (piece-at-square to piece-owner))
    :no
    piece))

(defmulti can-move-piece? #'can-move-piece-dispatch)

(defmethod can-move-piece? :pawn
  [piece [fr-file fr-rank :as from] [to-file to-rank :as to] piece-owner opponent]
  (let [color (:color @piece-owner)
        rank-ahead (pawn-next-rank fr-rank color)
        second-rank-ahead (pawn-next-rank rank-ahead color)]
    (if (and (pawn-start-square? from color)
             (= second-rank-ahead to-rank)
             (clean-vertical-path? from to (all-piece-squares piece-owner opponent)))
     ;; pawn moved two ranks ahead on it's first move
     true
     (->> (for [df [-1 0 1]] [(+ fr-file df) rank-ahead])
          (filter valid-square?)
          set
          to))))

(defmethod can-move-piece? :rook
  [piece from to piece-owner _]
  (let [piece-squares (all-piece-squares piece-owner opponent)]
    (or (clean-horizontal-path? from to piece-squares)
        (clean-vertical-path? from to piece-squares))))

(defmethod can-move-piece? :king
  [piece [fr-file fr-rank :as from] to piece-owner opponent]
  (or 
    (->> (for [df [-1 0 1] dr [-1 0 1] :when (not= df dr)] 
             [(+ fr-file df) (+ fr-rank dr)])
         (filter valid-square?)
         set
         to)
    (castling-condition? from to piece-owner opponent)))

(defmethod can-move-piece? :bishop
  [piece from to piece-owner opponent]
  (let [piece-squares (all-piece-squares piece-owner opponent)]
    (clean-diagonal-path? from to piece-squares)))

(defmethod can-move-piece? :knight
  [piece [fr-file fr-rank] to piece-owner opponent]
  (->> 
    (for [df [2 -2 1 -1] dr [2 -2 1 -1]
          :when (not= (abs dr) (abs df))]
      [(+ fr-file df) (+ fr-rank dr)])
    (filter valid-square?)
    set
    to))
          
(defmethod make-move :queen
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
    (if-not (:invalid-move action)
      (do
        (alter player update-in [:actions] conj action)
        action)
      nil)))
  
(defn take-piece-from-square
  "Takes piece on given square from it's owner and returns the result as action"
  [square piece-owner]
  (when-let [p (piece-at-square square piece-owner)]
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
  [piece from to piece-owner opponent]
  (dosync
    (let [moved (do-move-piece piece from to piece-owner)]
      (if-let [taken (take-piece-from-square to opponent)]
        (merge moved taken)
        moved))))

(defn make-move-dispatch
  [piece from to piece-owner opponent]
  (if (can-move-piece? piece from to piece-onwer opponent)
    piece
    :invalid-move))

(defmulti make-move #'make-move-dispatch)

(defmethod make-move :pawn
  [piece from [to-file to-rank :as to] piece-owner opponent]
  (let [color (:pieces-color @piece-owner)
			  action (move-piece piece from to piece-owner opponent)]
    (cond
      ;; check if pawn has moved to promotion
      (pawn-promotion-square? to color)
      (do
				(promote-pawn-at-square to piece-owner)
				;; TODO wait for the player to choose type of the promotion
				(assoc action :promotion {:square to :piece :queen}))
			   
			  ;; no piece is taken from opponent - check for an 'en-passant'
			  (not (:take action)) 
			  (let [passant-square [to-file (pawn-prev-rank to-rank color)]]
			    (if (passant-condition? passant-square opponent)
			      (dosync
			        (let [taken (take-piece-from-square passant-square opponent)]
			          (merge action taken)))
			      action))
     
			  :else
			  action)))

(defmethod make-move :king
  [piece from to piece-owner opponent]
  (let [action (move-piece piece
                           from 
                           to 
                           piece-owner 
                           opponent)]
    (if (-> (- to-file fr-file) abs (= 2))
      ;; we have castling
	    (let [rook-square (castling-rook-square from to)
	          rook-new-square (if (> to-file from-file)
	                            [(dec to-file) to-rank]
	                            [(inc to-file) to-rank])]
       (merge action
              (move-piece :rook 
                          rook-square 
                          rook-new-square 
                          piece-onwer 
                          opponent)))
    ;; else
    action)))

(defmethod make-move :common
  [piece from to piece-owner opponent]
  (make-move piece from to piece-onwer opponent))

(defmethod make-move :invalid
  [& _]
  {:invalid-move true})


;====================================================================
; Parsing, Protocol communication
;====================================================================

(def chess-square
  (bind [[f r] (<*> letter dec-num)]
    (return [(- (int f) 97) (dec r)])))

(defn to-indexed-square 
  "Converts chess square to indexed square, e.g. a6 to [0 5]"
  [input]
  (kern/parse chess-square input))

(defn to-chess-square 
  "Converts indexed cell to chess position, e.g. [1 5] to b6"
  [[f r]]
  (format "%c%d" (char (+ f 97)) (inc r)))

(defn generate-invalid-move-message [piece from to]
  (generate-string {:type "invalid-move"
                    :content {:error "Invalid move"}}))

(defn generate-init-message [player]
  (let [pieces (:pieces @player)
        formatted-pieces (reduce (fn [m p]
                                   (assoc m 
                                          p 
                                          (vec 
                                            (map to-chess-square
                                                 (p pieces)))))
                                 {}
                                 (keys pieces))]
  (generate-string 
    {:type "init"
     :content {:pieces formatted-pieces
               :pieaces-color (:pieces-color @player)}})))

(defn send-message [player msg]
  (-> (:out-stream @player) (. println msg)))

(defn receive-message [player] 
  (-> (:in-stream @player) (. readLine) (parse-string true)))

;====================================================================
; Game
;====================================================================

(defmulti process-message
  (fn [msg & _] (keyword (:type msg))))

(defmethod process-message :move
  [msg sender receiver]
  (let [content (:content msg)
        piece (:piece content)
        from (to-indexed-square (:from content))
        to (to-indexed-square (:to content))
        result (make-move piece from to sender receiver)]
    (condp = result
      :king (do 
              (println "game over todo close the sockets"))
      :invalid (send-message sender
                    generate-invalid-move-message [piece from to])
      (send-message msg receiver))))

(defn create-chess-pieces [color]
  (let [front-rank (if (= color :black) 6 1)
        back-rank (if (= color :black) 7 0)]
  {:pawn   (set (for [file (range 8)] [file front-rank])) 
   :rook   #{[0 back-rank] [7 back-rank]} 
   :knight #{[1 back-rank] [6 back-rank]} 
   :bishop #{[2 back-rank] [5 back-rank]} 
   :queen  #{[3 back-rank]              } 
   :king   #{[4 back-rank]              }}))

(defn create-player [socket pieces-color]
  (let [in (new BufferedReader 
                (new InputStreamReader (. socket getInputStream)))
        out (new PrintWriter (. socket getOutputStream) true)
        pieces (create-chess-pieces pieces-color)]
    (ref {:socket socket
          :in-stream in 
          :out-stream out 
          :pieces-color pieces-color
          :pieces pieces
          :actions []})))
      
(defn start-game [player-white player-black]
  (send-message player-white 
                (generate-init-message player-white))
  (send-message player-black 
                (generate-init-message player-black))
  (loop [player player-white 
         opponent player-black]
    (when-not (. (:socket @player) isClosed)
      (let [message (receive-message player)]
        (process-message message player opponent)
        (recur opponent player)))))

