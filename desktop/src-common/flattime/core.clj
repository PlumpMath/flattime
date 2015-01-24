(ns flattime.core
  (:use midje.sweet)
  (:require [play-clj.core :refer :all]
            [play-clj.math :refer :all]
            [play-clj.ui :refer :all]))

(def ^:const world-width 40)
(def ^:const world-height 40)
(def ^:const ppb 32)

(defn create-flatlander [x y]
  (assoc (shape :filled
                :set-color (color :white)
                :rect 0 0 ppb ppb)
        :x (* x ppb)
        :y (* y ppb)
        (comment "unitless width and height")
        :x-feet x
        :y-feet y
        :width ppb
        :alive? true
        ))

(def flatlander-a
  {:x 1 :y 2 :x-feet 1 :y-feet 1 :alive? true :id :a }
  )

(def flatlander-b
  {:x 2 :y 2 :x-feet 1 :y-feet 1 :alive? true :id :b }
  )

(def flatlander-c
  {:x 20 :y 20 :x-feet 1 :y-feet 1 :alive? true :id :b }
  )



(defn create-flatlanders [width height]
  (for [x (range 0 width 2)
        y (range 0 height 2)]
    (create-flatlander x y)))


(defn entity-rect
  "take an entity and builds a rectangle around it
   at it's current location"
  [{:keys [x y x-feet y-feet]} min-distance]
  (rectangle
   x
   y
   (+ min-distance x-feet)
   (+ min-distance y-feet)))

(fact "entity-rect should return a rectange that is useable"
      (entity-rect flatlander-a 0) => (rectangle 1 2 1 1))

(defn near-entitie?
  [e1 e2]
  (and (not= (:id e1) (:id e2))
       (rectangle! (entity-rect e1 1) :overlaps (entity-rect e2 1)) ))

(fact "near-entite? should return true is anything is within
      the min-distance"
      (near-entitie? flatlander-a flatlander-b) => true
      (near-entitie? flatlander-a flatlander-c) => false)

(defn nearest-entities
  [entities entity min-distance]
  (filter #(near-entitie? % entity) entities))

(defn update-flatlander [entity screen]
  (if (near-entitie? entity)
    (if  (mod (rand-int 10) 3)
      (assoc entity :alive? false))))



(defscreen main-screen
  :on-show
  (fn [screen entities]
    (create-flatlanders 10 10)
    (update! screen :renderer (stage) :camera (orthographic)))

  :on-render
  (fn [screen entities]
    (clear!)
    (->> entities
      (comment  (map #(update-flatlander %)))
      (comment  (filter #(:alive? %)))
      (render! screen)))

  :on-resize
  (fn [screen entities]
    (height! screen 600)))

(defgame flattime
  :on-create
  (fn [this]
    (set-screen! this main-screen)))

(defscreen blank-screen
  :on-render
  (fn [screen entities]
    (clear!)))

(set-screen-wrapper! (fn [screen screen-fn]
                       (try (screen-fn)
                            (catch Exception e
                              (.printStackTrace e)
                              (set-screen! flattime blank-screen)))))
