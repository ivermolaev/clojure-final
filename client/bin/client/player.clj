(ns client.player
  (:use  [client.utils]
         [clojure.walk :only (postwalk)]
         [cheshire.core :only (parse-string generate-string)]))

(import '(java.net Socket)
        '(java.io PrintWriter InputStreamReader BufferedReader))

;;=====================================================================
;;  Protocol messages formatting
;;=====================================================================

(defn format-pieces-squares [pieces format-fn]
  (reduce (fn [res p]
            (->> (mapv format-fn (p pieces))
              (assoc res p)))
          {} 
          (keys pieces)))

(defn format-squares [format-fn m]
  "Calls format-fn for value of every square representation in map m"
  (postwalk (fn [form]
              (if (vector? form)
                (let [[f s] form]
                  (cond 
                    (#{:from :to} f)
                    [f (format-fn s)]
                    
                    (= :pieces f)
                    [f (format-pieces-squares s format-fn)]
                    
                    :else form))
                form))
            m))

;;=====================================================================
;;  Player
;;=====================================================================

(def player (atom nil))

(defn set-player! [p]
  (reset! player p))

(defn dispose-player! []
  (.close (:socket @player))
  (set-player! nil))

(defn add-player-listener! [type listener]
  (swap! player update-in [type] conj listener)) 

(defn create-player [socket]
  (let [out (PrintWriter. (. socket getOutputStream) true)
        in (BufferedReader. 
             (InputStreamReader. 
               (. socket getInputStream)))]
    {:socket socket 
     :in-stream in 
     :out-stream out}))

(defn change-trurn []
  (swap! player update-in [:turn] not)
  (doseq [listener (:turn-listener @player)]
    (listener)))

(defn get-my-color [] (:pieces-color @player))

(defn get-opponent-color []
  (if (= "white" (get-my-color)) "black" "white"))

(defn my-turn? [] (:turn @player))

(defn my-color? [color] (= (get-my-color) color))


;;=====================================================================
;;  Sending/ Receiving messages
;;=====================================================================

(def reader (agent nil))

(defn receive-message []
  (let [msg (-> (.readLine (:in-stream @player)) 
                (parse-string true))
        to-receive (format-squares chess-square-to-cell-center msg)]
    to-receive))

(defn send-message
  "Sends message with givent content and type. 
  Blocks untill response is received and handled"
  [content type response-handler]
  (let [msg-promise (promise)]
    ;; Reader should be waiting to receive message
    ;; Make sure we are next in it's queue before it is in waiting state again
    (send-off reader (fn [msg]
                       (deliver msg-promise msg)
                       nil))
    (let [to-send (-> {:type type}
                      (assoc :content 
                             (format-squares coord-to-chess-square 
                                             content))
                      (generate-string))]
      (.println (:out-stream @player) to-send)
      (response-handler @msg-promise))))

(defn make-move
  "Attempts to move a piece. Uppon success success-fn is called,
   failure-fn called otherwise"
  [piece from to success-fn failure-fn]
  (let [handler (fn [resp] 
                  (let [cnt (:content resp)]
                    (if-let [move (:move cnt)]
                      (do
                        (success-fn cnt)
                        (change-trurn))
                      (failure-fn))))]
  (send-message {:piece piece :from from :to to} :move handler)))

