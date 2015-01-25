(ns flattime.core
  (:use midje.sweet)
  (:require [play-clj.core :refer :all]
            [play-clj.math :refer :all]
            [play-clj.ui :refer :all]))

(declare flattime main-screen overlay-screen)

(def ^:const world-width 40)
(def ^:const world-height 40)
(def ^:const ppb 32)
(def ^:const grid-factor 10)
(defn create-flatlander [x y]
  (assoc (shape :filled
                :set-color (color :green)
                :rect 0 0 ppb ppb)
        :x x
        :y y
        :x-feet ppb
        :y-feet ppb
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

(def landers [flatlander-a flatlander-b flatlander-c])

(defn create-flatlanders [width height]
  (vec (for [x (range 0 width 2)
             y (range 0 height 2)]
         (create-flatlander (* grid-factor x) (* grid-factor y)))))


(defn entity-rect
  "take an entity and builds a rectangle around it
   at it's current location"
  [{:keys [x y x-feet y-feet]} range]
  (rectangle
   x
   y
   (+ range x-feet)
   (+ range y-feet)))


(fact "entity-rect should return a rectange that is useable"
      (entity-rect flatlander-a 0) => (rectangle 1 2 1 1))

(defn near-entitie?
  [e1 e2 range]
  (and (not= (:id e1) (:id e2))
       (rectangle! (entity-rect e1 range) :overlaps (entity-rect e2 range)) ))

(fact "near-entite? should return true is anything is within
      the min-distance"
      (near-entitie? flatlander-a flatlander-b 2) => true
      (near-entitie? flatlander-a flatlander-c 2) => false)

(defn nearest-entities
  [entities entity range]
  (filter #(near-entitie? % entity range) entities))

(fact "nearest-entites returns all of the entiies within range of another givin enties"
      (nearest-entities landers flatlander-a 2) => [flatlander-b]
      (nearest-entities landers flatlander-a 20) => [flatlander-b flatlander-c])


(defn update-flatlander [entity entities]
  (let [near  (nearest-entities entities entity 2)]

    (if (< (count near) 5)
      (if (mod (rand-int 10) 3)
        (assoc entity :alive? false)))))

(defscreen overlay-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
    [(assoc (label "0" (color :white))
             :id :fps
             :y 0
             :x 5)
     (assoc (label "0" (color :white))
            :id :entity-count
            :y 15
            :x 5)])

  :on-render
  (fn [screen entities]
    (->> (for [entity entities]
           (case (:id entity)
             :fps (doto entity (label! :set-text (str (game :fps))))
             entity))
         (render! screen)))

  :on-resize
  (fn [screen entities]
    (height! screen 600))


  :on-update-entity-count
  (fn [screen entities]
    (let [count (:entity-count screen)]
      (->> (for [e entities]
            (case (:id e)
            :entity-count (doto e (label! :set-text (str count)))
            e))
           (render! screen)))))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage) :camera (orthographic))
    (create-flatlanders 10 10))

  :on-key-down
  (fn [screen entities]
    (case (:key screen)
      (key-code :r) (set-screen! flattime main-screen overlay-screen)
        (conj entities (create-flatlander
           (rand-int (width screen) )
           (rand-int (height screen) )))))

  :on-render
  (fn [screen entities]
    (clear!)
    (screen! overlay-screen :on-update-entity-count :entity-count (count entities))
    (->> entities
;         (map #(update-flatlander % entities))
         (render! screen)))

  :on-resize
  (fn [screen entities]
    (height! screen 600)))

(defgame flattime
  :on-create
  (fn [this]
    (set-screen! this main-screen overlay-screen)))

(defscreen blank-screen
  :on-render
  (fn [screen entities]
    (clear!)))

(set-screen-wrapper! (fn [screen screen-fn]
                       (try (screen-fn)
                            (catch Exception e
                              (.printStackTrace e)
                              (set-screen! flattime overlay-screen)))))
