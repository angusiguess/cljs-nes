(ns app.renderer
  (:require [reagent.core :as r]))

(defn some-component []
  [:div
   [:h3 "I am a component!"]
   [:p.someclass
    "I have " [:strong "bold"]
    [:span {:style {:color "red"}} " and 0x0000"]
    " text."]])

(defn init []
  (r/render-component [some-component]
                      (.-body js/document))
  (js/console.log "Starting Application"))
