(ns client.board
 (:use [seesaw core graphics layout behave]
       [client utils config]
       [client.player :only (make-move my-turn?)])
 (:require [clojure.java.io :as io])
 (:import [javax.swing SwingUtilities]))

(def board-image-src "resources/board.png")
(defonce board-image (-> (io/file board-image-src) icon .getImage))

;;=====================================================================
;; Patch for components with no layout (@see seesaw.layout)
;;=====================================================================

(extend-protocol LayoutManipulation
  nil
  (add!* [layout target widget constraint]
    (add-widget target widget))
  (get-constraint* [layout container widget] nil))

;;=====================================================================
;; Utilities
;;=====================================================================

(defn align-location
  "Alignes the widget location so it's center will be x y"
  [widget [x y]]
  [(- x (int (/ (width widget) 2)))
   (- y (int (/ (height widget) 2)))])

;;=====================================================================
;; Board actions 
;;=====================================================================

(declare board-panel)
(declare deepest-widget-at)
(declare widget-at)
(declare add-piece-to-board)

(defmulti handle-action
  (fn [action properties] (keyword action)))

(defn handle-actions [map-of-actions]
  (doseq [a (seq map-of-actions)]
    (handle-action (key a) (val a))))

(defmethod handle-action :take
  [action properties]
  (when-let [w (deepest-widget-at (:from properties))]
    (remove! board-panel w)))

(defmethod handle-action :move
  [action properties]
  (when-let [w (widget-at (:from properties))]
    (let [to (:to properties)]
      (move! w :to-front)
      (move! w :to (align-location w to))))) 

(defmethod handle-action :castling
  [action properties]
  (handle-action :move properties))

(defmethod handle-action :promotion
  [action properties]
  (let [p (:piece properties)
        from (:from properties)
        color (:piece-color properties)
        mine? (:mine properties)]
    (println "Promotion:" properties)
    (handle-action :take properties)
    (add-piece-to-board color p from mine?)))

;;=====================================================================
;;  Chess Board
;;=====================================================================

(defn to-vec [^java.awt.Point p]
  [(.x p) (.y p)])


(defn widget-at [[x y]]
 (.getComponentAt board-panel x y))

(defn deepest-widget-at 
  "Returns the widget with lowest z-index which contains x y"
  [[x y]]
  (last 
    (filter (fn [c] 
              (.contains c (- x (.getX c)) (- y (.getY c)))) 
            (.getComponents board-panel))))

(defn can-move-widget-by?
  "Checks if widget will go out of bounds if moved by dx dy"
  [widget [dx dy]]
  (let [loc (config widget :location)
        min-x (+ (.x loc) dx)
        max-x (+ min-x (width widget))
        min-y (+ (.y loc) dy)
        max-y (+ min-y (height widget))]
    (and (>= min-x 0) (>= min-y 0) 
         (<= max-x board-dim) (<= max-y board-dim))))
                                
;(defn my-turn? [] true)

(defn make-movable [widget with-piece]
  (let [start-point (java.awt.Point.)
        ref-point (java.awt.Point.)]
    (when-mouse-dragged widget
      :start (fn [e]
               (when (my-turn?)
                 (move! e :to-front)
                 (.setLocation ref-point (.getPoint e))
                 (.setLocation start-point 
                               (SwingUtilities/convertPoint (.getSource e)
                                                            (.getPoint e)
                                                            board-panel))))
      
      :drag (fn [e _]
              (when (my-turn?)
                (let [p (.getPoint e)
                      delta [(- (.x p) (.x ref-point)) 
                             (- (.y p) (.y ref-point))]]
                  (when (can-move-widget-by? widget delta)
                    (move! e :by delta)))))
      
      :finish (fn [e]
                (when (my-turn?)
                  (let [from (-> start-point
                               (to-vec))
                        to (-> (SwingUtilities/convertPoint (.getSource e)
                                                            (.getPoint e)
                                                            board-panel)
                             (to-vec))
                        success (fn [actions]
                                  (let [to-aligned (->> (coord-to-cell to)
                                                     (cell-center-coord)
                                                     (align-location widget))]
                                    (move! e :to to-aligned)
                                    (handle-actions (dissoc actions :move))))
                        failure (fn []
                                  (let [fr-aligned (->> (coord-to-cell from)
                                                     (cell-center-coord)
                                                     (align-location widget))]
                                    (move! e :to fr-aligned)))]
                    (make-move with-piece from to success failure)))))
    widget))

(defn piece-image-path [color name]
  (format "resources/%s_%s.png" name color))

(defn add-piece-to-board
  "Adds widget with image of the corresponding piece and color"
  [color name loc movable?]
  (let [icon (-> (piece-image-path color name) io/file icon)
        widget (doto (label :icon icon)
                     (config! :bounds :preferred))]
    (config! widget :location (align-location widget loc))
    (add! board-panel (if movable? 
                        (make-movable widget name)
                        widget))))

(defn add-pieces-to-board [pieces color movable?]
  (doseq [piece-entry (seq pieces)]
    (doseq [loc (val piece-entry)]
      (add-piece-to-board color 
                          (name (key piece-entry))
                          loc
                          movable?))))

(defn draw-board [c g] (.drawImage g board-image 0 0 nil))

(defn make-board-panel []
  (xyz-panel
    :paint {:after draw-board}
    :id :xyz
    :size [board-dim :by board-dim]
    :preferred-size [board-dim :by board-dim]))

(def board-panel (make-board-panel))

(defn make-frame []
  (frame :title   "Chess"
         :content (border-panel
                    :north (flow-panel
                             :items [(label :text "Waiting to join a game..." 
                                            :class :info
                                            :size [350 :by 20]
                                            :halign :center
                                            :h-text-position :center)])
                    :center board-panel)))

(def the-frame (make-frame))

(defn create-board [] 
  (native!)
  (config! the-frame :resizable? false)
  (pack! (show! the-frame)))

(defn clear-board []
  (apply remove! board-panel (.getComponents board-panel)))

(defn display-message [msg]
  (config! (select the-frame [:.info]) :text msg))

