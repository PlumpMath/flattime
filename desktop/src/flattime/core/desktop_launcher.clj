(ns flattime.core.desktop-launcher
  (:require [flattime.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. flattime "flattime" 800 600)
  (Keyboard/enableRepeatEvents true))
