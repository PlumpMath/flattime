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
         :x (- x (rem x ppb))
        :y (- y (rem y ppb))
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
  (vec (for [x (range 0 width 1)
             y (range 0 height 1)]
         (create-flatlander (rand-int width) (rand-int height)))))


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

(defn down-from
  [{:keys [x y y-feet x-feet]}]
  {:y (+ y y-feet)
   :x x
   :x-feet x-feet
   :y-feet y-feet})

(defn up-from
  [{:keys [x y y-feet x-feet]}]
   {:x x
    :y (- y y-feet)
    :x-feet x-feet
    :y-feet y-feet})

(defn left-from
  [{:keys [x y x-feet y-feet]}]
  {:x (- x x-feet)
   :y y
   :x-feet x-feet
   :y-feet y-feet})

(defn right-from
  [{:keys [x y x-feet y-feet]}]
  {:x (+ x x-feet)
   :y y
   :x-feet x-feet
   :y-feet y-feet})

(def up-left-from
  (comp up-from left-from))

(def up-right-from
  (comp up-from right-from))

(def down-left-from
  (comp down-from left-from))

(def down-right-from
  (comp down-from right-from))

(def directions
  [up-from down-from
   left-from right-from
   up-left-from up-right-from
   down-left-from down-right-from])

(defn get-at
  [{:keys [x y]} coll]
  (nth (nth coll y) x))

(let [grid [[1 2 3]
            [4 5 6]
            [7 8 9]]
      point {:x 1 :y 1 :x-feet 1 :y-feet 1}
      out [2 8 4 6 1 3 7 9]]
  (fact "applying the directions to a point in
         a ordered 3 by three grid should give
         you a list of numbers"
        (map #(get-at (apply % [point]) grid) directions) => out ))

(defn collides?
  [e1 e2]
  (and (= (:x e1) (:x e2))
       (= (:y e1) (:y e2))))


(defn can-grow?
  "brute force can grow, this will tell you
  if a place is empty and has room for an
   atanama to grow... It should partition
   and index the entities so that it is a
   constant operation but currently just
   checks all of the known near entities"
  [place es]
  (reduce (fn [cur val]
            (if (not (collides? place val))
              true
              (reduced false)))
          true es))

(tabular
 (fact "takes two enities and checks to see
       if their x and y corrds are ="
       (collides? ?e1 ?e2) => ?result)
 ?e1          ?e2         ?result
 {:x 1 :y 2}  {:x 1 :y 2} truthy
 {:x 2 :y 2}  {:x 1 :y 2} falsey)

(defn filter-seeds
  "determines a cell could grow here"
  [places coll]
  (filter #(can-grow? % coll) places))

(fact "function should take a collection of
       points and entities and see if any
       of the points take by the entities"
      (let [places [{:x 1 :y 2}
                    {:x 3 :y 4}]
            es [{:x 3 :y 4} {:x 2 :y 2}]]
        (filter-seeds places es) => '({:x 1 :y 2})))

(fact "takes a place and a set of entities to
       check for collisions with and returns
       true if the seed can grow and false
       otherwise"
      (let [es [{:x 3 :y 4} {:x 2 :y 2}]]
        (can-grow? {:x 3 :y 4} es) => false)
      (let [es [{:x 3 :y 4} {:x 2 :y 2}]]
        (can-grow? {:x 2 :y 4} es) => true))

(defn seeds
  "generates possible children"
  [e]
  (map #(apply % [e]) directions))

(defn spawn-child
  "Takes an entity and the entities around it
   and find a place to reproduce then creates a new entitie there"
  [e ne-coll]
  (let [child (rand-nth (filter-seeds (seeds e) ne-coll))]
    (create-flatlander (:x child ) (:y child))))

(defn try-spawn
  [es e]
  (let [near (nearest-entities es e 2)
        count (count near)]
    (when (< count 4)
      (spawn-child e near))))

(defn spawn-children
  "finds places where entities need to spawn"
  [es]
  (concat (map (partial try-spawn es) es) es))

(defn update-flatlander [entity entities]
  (let [near (nearest-entities entities entity 2)]
    (if (> (count near) 3)
        (assoc entity :alive? false)
        entity)))

(defscreen overlay-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
    [(assoc (label "0" (color :white))
             :id :fps
             :y 0
             :x 5)

     (assoc (label "0" (color :white))
            :id :tic-count
            :y 30
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


  :on-update-counts
  (fn [screen entities]
    (let [count (:entity-count screen)
          tic-count (:tic-count screen)]
      (->> (for [e entities]
            (case (:id e)
            :entity-count (doto e (label! :set-text (str count)))
            :tic-count (doto e (label! :set-text (str tic-count)))
            e))
           (render! screen)))))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen
             :renderer (stage)
             :camera (orthographic)
             :tic-count 0)
    (vec (take 5
               (repeatedly #(create-flatlander
                        (rand-int (width screen)) (rand-int (height screen)))))))

  :on-key-down
  (fn [screen entities]
    (when (key-pressed? :r)
      (clear!)
      (set-screen! flattime main-screen overlay-screen))
    (when (key-pressed? :k)
      (remove-timer! screen :tic))
    (when (key-pressed? :s)
      (add-timer! screen :tic 1 1))
    (when (key-pressed? :a)
           (conj entities (create-flatlander
                           (rand-int (width screen) )
                           (rand-int (height screen) )))))

  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :tic (do
             (update! screen :tic-count (inc (:tic-count screen)))
             (->> entities
                  (map #(update-flatlander % entities))
                  (filter :alive?)
                  (spawn-children)))))

  :on-render
  (fn [screen entities]
    (clear!)
    (screen! overlay-screen :on-update-count
             :entity-count (count entities)
             :tic-count (:tic-count screen))
    (->> entities
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
