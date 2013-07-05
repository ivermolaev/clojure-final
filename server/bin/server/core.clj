(ns server.core
  (:use [server.game :only (create-player start-game)])
  (:gen-class))

(import '(java.net ServerSocket Socket SocketException))

(def awaiting-socket (atom nil))

(def running? true)

(defn accept-connection [server-socket]
  (try (. server-socket accept)
       (catch SocketException e)))

(defn start-server [port]
  (with-open [server-socket (new ServerSocket port)]
    (println "Server started on port: " port)
    (while running? 
      (let [player-socket (accept-connection server-socket)]
        (if-let [opponent-socket @awaiting-socket]
          (future 
            (start-game 
              (create-player opponent-socket :white)
              (create-player player-socket :black)))
          (reset! awaiting-socket player-socket))))))

(defn -main
  [& args]
  (start-server 5553))
