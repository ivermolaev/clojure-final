(ns client.core
  (:gen-class))

(import '(java.net Socket)
        '(java.io PrintWriter InputStreamReader BufferedReader))

(def player (atom {}))

(defn start-client [port]
    (let [socket (Socket. "localhost" port)
          out (PrintWriter. (. socket getOutputStream) true)
          in (BufferedReader. (InputStreamReader. (. socket getInputStream)))]
      (swap! player assoc :socket socket :in-stream in :out-stream out)))

(defn stop-client []
  (-> (:socket @player) (. close)))

(defn send-message [m]
  (do
    (-> (:out-stream @player) (. println m))
    (-> (:in-stream @player) (. readLine) println)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
