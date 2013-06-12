(ns server.rules-test
  (:use clojure.test
        server.rules))

(defn invalid-move? [action] (:invalid-move action))

(defn moved? [piece from to action] 
  (when-let [move (:move action)]
    (and (= from (:from move)) 
         (= to (:to move))
         (= piece (:piece move)))))

(defn taken? [piece from action] 
  (when-let [t (:take action)]
    (and (= from (:from t)) 
         (= piece (:piece t)))))

(defn promote? [square action]
  (when-let [promo (:promote action)]
    (= square (:from promo))))

(defn castling? [from to action]
  (when-let [castling (:castling action)]
    (= from (:from castling))
    (= to (:to castling))))

(deftest basic-moves
  (let [p-w (ref {:pieces-color :white 
                  :pieces  {:pawn   #{[0 1] [1 1] [2 2]}}})
        p-b (ref {:pieces-color :black
                  :pieces  {:pawn   #{[0 6] [2 5]}}})]
    
    (is (invalid-move? (make-move :king [31 1] [4 14] p-w p-b)) "Inalid squares")
    (is (invalid-move? (make-move :pawn [3 1] [4 1] p-w p-b)) "Unexpected piece on square")
    (is (invalid-move? (make-move :pawn [0 1] [1 1] p-w p-b)) "Overlap with friend piece")))

(deftest pawn-moves
  (let [p-w (ref {:pieces-color :white 
                  :pieces  {:pawn   #{[0 1] [1 1] [6 1] [2 2] [4 1] [7 6]}
                            :bishop #{[3 4]}}})
        p-b (ref {:pieces-color :black
                  :pieces  {:pawn   #{[0 6] [1 6] [2 5] [5 3]} 
                            :bishop #{[1 2] [1 3]}}})]
    ;; forbidden moves
    (is (invalid-move? (make-move :pawn [1 1] [1 3] p-w p-b)) "Jump over enemy bishop")
    (is (invalid-move? (make-move :pawn [1 1] [1 2] p-w p-b)) "White move to occupied square infront of it")
    (is (invalid-move? (make-move :pawn [0 1] [0 0] p-w p-b)) "White move backwards")
    (is (invalid-move? (make-move :pawn [0 6] [0 7] p-b p-w)) "Black move backwards")
    (is (invalid-move? (make-move :pawn [2 2] [3 2] p-w p-b)) "Pawn right move")
    (is (invalid-move? (make-move :pawn [2 5] [1 5] p-b p-w)) "Pawn left move")
    (is (invalid-move? (make-move :pawn [2 2] [2 4] p-w p-b)) "Moved white pawn two ranks ahead")
    (is (invalid-move? (make-move :pawn [2 5] [2 3] p-b p-w)) "Moved black pawn two ranks ahead")
    (is (invalid-move? (make-move :pawn [2 2] [3 3] p-w p-b)) "Diagonal white pawn move without enemy piece on target")
    (is (invalid-move? (make-move :pawn [0 6] [1 5] p-b p-w)) "Diagonal black pawn move without enemy piece on target")
    
    ;; permited moves
    (is (moved? :pawn [0 1] [0 2] (make-move :pawn [0 1] [0 2] p-w p-b)) "White pawn forward")
    (is (moved? :pawn [0 6] [0 5] (make-move :pawn [0 6] [0 5] p-b p-w)) "Black pawn forward")
    (is (moved? :pawn [6 1] [6 3] (make-move :pawn [6 1] [6 3] p-w p-b)) "White pawn first move two ranks ahead")
    (is (moved? :pawn [1 6] [1 4] (make-move :pawn [1 6] [1 4] p-b p-w)) "Black pawn first move two ranks ahead")
    (let [action (make-move :pawn [2 2] [1 3] p-w p-b)]
      (is (and (moved? :pawn [2 2] [1 3] action)
               (taken? :bishop [1 3] action)) "White pawn takes enemy bishop with diagonal move"))
    (let [action (make-move :pawn [2 5] [3 4] p-b p-w)]
      (is (and (moved? :pawn [2 5] [3 4] action)
               (taken? :bishop [3 4] action)) "Black pawn takes enemy bishop with diagonal move"))
    (do
      (-> (make-move :pawn [4 1] [4 3] p-w p-b) (save-player-action p-w))
      (let [action (make-move :pawn [5 3] [4 2] p-b p-w)]
        (is (and (moved? :pawn [5 3] [4 2] action)
                 (taken? :pawn [4 3] action)) "Black en-passant")))
    (is (promote? [7 7] (make-move :pawn [7 6] [7 7] p-w p-b)) "White pawn promotion")))

(deftest king-moves
  (let [p-w (ref {:pieces-color :white 
                  :pieces  {:king #{[4 0]}
                            :rook #{[7 0]}
                            :queen #{[7 5]}
                            :knight #{[3 5]}}})
        p-b (ref {:pieces-color :black
                  :pieces  {:king #{[4 6]}
                            :rook #{[0 7] [7 7]}
                            :queen #{}
                            :knight #{}}})]
    
    (is (moved? :king [4 6] [4 7] (make-move :king [4 6] [4 7] p-b p-w)) "Black king square back")
    (is (moved? :king [4 7] [4 6] (make-move :king [4 7] [4 6] p-b p-w)) "Black king square forward")
    (is (moved? :king [4 6] [5 6] (make-move :king [4 6] [5 6] p-b p-w)) "Black king square digonal forward")
    (is (moved? :king [5 6] [4 6] (make-move :king [5 6] [4 6] p-b p-w)) "Black king square digonal backwards")
    (is (invalid-move? (make-move :king [4 6] [4 4] p-b p-w)) "Black king two squares forward")
    (is (invalid-move? (make-move :king [4 0] [2 0] p-w p-b)) "White king two squares left")
    (let [action (-> (make-move :king [4 0] [6 0] p-w p-b) (save-player-action p-w))]
      (is (and (moved? :king [4 0] [6 0] action) (castling? [7 0] [5 0] action)) "White near rook castling"))
    (do
      ;; Move black king to it's starting square and save the move
      (-> (make-move :king [4 6] [4 7] p-b p-w) (save-player-action p-b))
      (is (invalid-move? (make-move :king [4 7] [2 7] p-b p-w)) "Black far rook castling when king was moved before"))
    (do
      ;; Black king is on it's starting square [4 7] due to previous assert
      ;; Erase any history
      (dosync (alter p-b assoc-in [:actions] []))
      (is (invalid-move? (make-move :king [4 7] [6 7] p-b p-w)) "Black near rook castling when enemy queen is attacking the passed square"))
    (do 
      (-> (make-move :rook [0 7] [0 4] p-b p-w) (save-player-action p-b))
      (-> (make-move :rook [0 4] [0 7] p-b p-w) (save-player-action p-b))
      (is (invalid-move? (make-move :king [4 7] [2 7] p-b p-w)) "Black far rook castling when king rook was moved before"))
    (do 
      ;; Black King is in check by white knight
      (is (invalid-move? (make-move :king [4 7] [2 7] p-b p-w)) "Black far rook castling when king is in check"))))

(deftest rook-moves
  (let [p-w (ref {:pieces-color :white 
                  :pieces  {:king #{[4 0]}
                            :rook #{[7 0]}
                            :queen #{[7 5]}
                            :knight #{[3 5]}}})
        p-b (ref {:pieces-color :black
                  :pieces  {:king #{[4 6]}
                            :rook #{[0 7] [7 7]}
                            :queen #{}
                            :knight #{}}})]
     
    (is (invalid-move? (make-move :rook [7 0] [7 6] p-w p-b)) "White rook jump over piece")
    (is (invalid-move? (make-move :rook [7 0] [6 1] p-w p-b)) "White rook diagonal move")
    (is (moved? :rook [0 7] [0 4] (make-move :rook [0 7] [0 4] p-b p-w)) "Black rook vertical move")
    (is (moved? :rook [0 4] [5 4] (make-move :rook [0 4] [5 4] p-b p-w)) "Black rook horizontal move")))


(deftest bishop-moves
  (let [p-w (ref {:pieces-color :white 
                  :pieces  {:pawn #{[5 3]}
                            :bishop #{[2 0]}}})
        p-b (ref {:pieces-color :black
                  :pieces  {:king #{[4 6]}
                            :bishop #{[2 7]}}})]
     
    (is (invalid-move? (make-move :bishop [2 0] [6 4] p-w p-b)) "White rook jump over piece")
    (is (invalid-move? (make-move :bishop [2 0] [5 0] p-w p-b)) "White rook horizontal move")
    (is (invalid-move? (make-move :bishop [2 7] [2 4] p-b p-w)) "Black rook vertical move")
    (is (moved? :bishop [2 7] [6 3] (make-move :bishop [2 7] [6 3] p-b p-w)) "Black rook left diagonal move")
    (is (moved? :bishop [2 0] [4 2] (make-move :bishop [2 0] [4 2] p-w p-b)) "White rook right diagonal move")))

(deftest knight-moves
  (let [p-w (ref {:pieces-color :white 
                  :pieces  {:knight #{[3 3]}
                            :bishop #{[2 0]}}})
        p-b (ref {:pieces-color :black
                  :pieces  {:knight #{[5 0]}
                            :bishop #{[5 1]}}})]
     
    (is (invalid-move? (make-move :knight [3 3] [5 5] p-w p-b)) "Knight diagonal move")
    (is (invalid-move? (make-move :knight [3 3] [5 3] p-w p-b)) "Knight horizontal move")
    (is (invalid-move? (make-move :knight [3 3] [3 0] p-w p-b)) "Knight vertical move")
    (is (moved? :knight [5 0] [6 2](make-move :knight [5 0] [6 2] p-b p-w)) "Black knight jumps over piece")
    (doseq [f (range 8) r (range 8)]
      (if (#{[1 4] [2 5] [4 5] [5 4] [1 2] [2 1] [4 1] [5 2]} [f r])
        (is (can-move-piece? :knight [3 3] [f r] p-w p-b))
        (is (not (can-move-piece? :knight [3 3] [f r] p-w p-b)))))))


