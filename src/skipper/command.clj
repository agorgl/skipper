(ns skipper.command
  (:require
   [clojure.core.match :as match]
   [skipper.app :as app]))

(defn dispatch [command]
  (match/match command
    [:app-create app] (app/create app)
    [:app-list] (run! println (app/list))
    [:app-build app] (app/build app)
    [:app-deploy app] (app/deploy app)
    [:app-logs app _] (app/logs app)
    [:app-delete app] (app/delete app)))
