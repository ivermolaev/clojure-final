 (ns client.core
  (:gen-class)
  (:use [client board player]
        [seesaw.invoke]))

(import '(java.net Socket)
        '(java.io PrintWriter InputStreamReader BufferedReader))

;;=====================================================================
;;  Process messages
;;=====================================================================

(defn display-turn []
  (display-message 
    (if (my-turn?)
      (format "[%s] Make your move." (get-my-color))
      (format "[%s] Wait for opponent's move." (get-my-color)))))

(defmulti process-message
  (fn [msg] 
    (keyword (:type msg)))
  :default nil)

(defmethod process-message nil
  [msg]
  ;; NO OP
  nil)

(defmethod process-message :start-game
  [msg]
  (let [cnt (:content msg)
        player-config (:player cnt)
        player-color (:pieces-color player-config)
        opponent-config (:opponent cnt)
        opponent-color (:pieces-color opponent-config)]
    (swap! player 
           assoc  
           :pieces-color player-color 
           :turn (= "white" player-color))
    (add-pieces-to-board (:pieces player-config) 
                         player-color
                         true)
    (add-pieces-to-board (:pieces opponent-config) 
                         opponent-color
                         false)
    (display-turn)))

(defmethod process-message :victory
  [msg]
  (clear-board)
  (display-message (format "[%s] Victory" (get-my-color)))
  (dispose-player!))

(defmethod process-message :defeat
  [msg]
  (clear-board)
  (display-message (format "[%s] Defeat" (get-my-color)))
  (dispose-player!))

(defmethod process-message :opponent-move
  [msg]
  (handle-actions (:content msg))
  (change-trurn))

(defmethod process-message :opponent-promotion
  [msg]
  (handle-action :promotion 
                 (assoc (:content msg)
                        :piece-color (get-opponent-color)
                        :mine false)))
  
(defmethod process-message :promote
  [msg]
  (let [cnt (:content msg)
        piece "queen"
        from (:from cnt)
        promotion-props {:piece piece :from from}
        handler (fn [resp]
                  (if (:ok (:content resp))
                    (do
                      (handle-action :promotion 
                                     (assoc promotion-props
                                            :piece-color (get-my-color)
                                            :mine true)))
                    (throw (RuntimeException.))))]
  (send-message promotion-props :promotion handler)))

(defn process-next-message [_]
  (when-let [msg (receive-message)]
    (invoke-later 
      (process-message msg))
    (send-off reader #'process-next-message)
     msg))

  
;;=====================================================================
;;  Main
;;=====================================================================
(def end-point "localhost")
(def port 5553)

(defn start-client [end-point port]
  (let [socket (Socket. end-point port)]
    (set-player! (create-player socket))
    (add-player-listener! :turn-listener display-turn)
    (send-off reader process-next-message)))

(defn -main
  [& args]
  (create-board)
  (start-client end-point port))
