(ns server.game
  (:use [server.rules :only (promote-pawn-at-square save-player-action make-move)]
        [cheshire.core :only (parse-string generate-string)]
        [blancas.kern.core :as kern]
        [clojure.walk]))

(import '(java.io PrintWriter InputStreamReader BufferedReader))

(def chess-square
  (bind [[f r] (<*> letter dec-num)]
    (return [(- (int f) 97) (dec r)])))

(defn to-indexed-square 
  "Converts chess square to indexed square, e.g. a6 to [0 5]"
  [input]
  (:value (kern/parse chess-square input)))

(defn to-chess-square 
  "Converts indexed cell to chess position, e.g. [1 5] to b6"
  [[f r]]
  (format "%c%d" (char (+ f 97)) (inc r)))

(defn format-squares [format-fn m]
  "Calls format-fn for value of every square entry with key :from or :to in map m"
  (postwalk (fn [form]
              (if (and (vector? form)
                       (-> form first #{:from :to}))
                [(first form) (format-fn (second form))] 
                form))
            m))

(defn pieces-with-chess-squares [player]
  (let [pieces (:pieces @player)]
    (reduce (fn [res p]
              (->> (mapv to-chess-square (p pieces))
                   (assoc res p)))
            {} 
            (keys pieces))))
  
(defn send-message [content type receiver]
  (let [msg (cond-> {}
                    ((complement nil?) type)
                    (assoc :type type)
                    
                    ((complement nil?) content) 
                    (assoc :content
                           (format-squares to-chess-square 
                                           content))
                    
                    true (generate-string))]
    (.println (:out-stream @receiver) msg)
    (printf "Message sent to %s: %s\n" (:pieces-color @receiver) msg)))

(defn receive-message [sender]
  (let [msg (-> (.readLine (:in-stream @sender)) 
                (parse-string true))]
    (printf "Message received from %s: %s\n" (:pieces-color @sender) msg)
    (format-squares to-indexed-square msg)))
                    

(defmulti process-message
  (fn [msg & _] (keyword (:type msg))))

(defmethod process-message :promotion
  [msg sender opponent]
  (let [content (:content msg)
        piece (keyword (:piece content))
        square (:from content)]
    (if-let [action (promote-pawn-at-square square sender piece)]
      (do
        (send-message {:ok "Valid promotion"} nil sender)
        (-> (save-player-action action sender)
            :promotion
            (send-message :opponent-promotion opponent))) 
      (do 
        (send-message {:error "Invalid promotion"} nil sender)
        (-> (receive-message sender) 
            (process-message sender opponent))))))

(defn handle-invalid-move [move-maker opponent]
  (send-message {:error "Invalid move"} nil move-maker)
  (-> (receive-message move-maker)
      (process-message move-maker opponent)))

(defn handle-pawn-promotion [properties promoter opponent]
  (send-message properties :promote promoter)
  (-> (receive-message promoter)
      (process-message promoter opponent)))

(defn game-over [winner looser]
  (send-message nil :victory winner)
  (.close (:socket @winner))
  (send-message nil :defeat looser)
  (.close (:socket @looser)))

(defmethod process-message :move
  [msg sender opponent]
  (let [content (:content msg)
        piece (keyword (:piece content))
        from (:from content)
        to (:to content)
        action (make-move piece from to sender opponent)]
    (if (:invalid-move action)
      (handle-invalid-move sender opponent)
      (let [action-without-promotion (-> (dissoc action :promote)
                                         (save-player-action sender))]
        (send-message action-without-promotion nil sender)
        (send-message action-without-promotion :opponent-move opponent)
        ;; If the King is taken by the pawn we don't care for the promotion
        (if (= :king (-> (:take action) :piece))
          (game-over sender opponent)
          (when-let [promotion (:promote action)]
            (handle-pawn-promotion promotion sender opponent)))))))

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
  (println "Game started")
  (let [white-pieces (pieces-with-chess-squares player-white)
        black-pieces (pieces-with-chess-squares player-black)]
    (send-message {:player   {:pieces-color :white
                              :pieces white-pieces}
                   :opponent {:pieces-color :black
                              :pieces black-pieces} }
                  :start-game
                  player-white)
    (send-message {:player   {:pieces-color :black
                              :pieces black-pieces}
                   :opponent {:pieces-color :white
                              :pieces white-pieces}}
                  :start-game
                  player-black)
    (loop [player player-white 
           opponent player-black]
      (when-not (.isClosed (:socket @player))
        (-> (receive-message player)
            (process-message player opponent))
        (recur opponent player)))))
