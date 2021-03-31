;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.workspace.path.tools
  (:require
   [app.main.data.workspace.common :as dwc]
   [app.main.data.workspace.path.changes :as changes]
   [app.main.data.workspace.path.common :as common]
   [app.main.data.workspace.path.state :as st]
   [app.util.geom.path :as ugp]
   [beicon.core :as rx]
   [potok.core :as ptk]))

(defn change-edit-mode [mode]
  (ptk/reify ::change-edit-mode
    ptk/UpdateEvent
    (update [_ state]
      (let [id (get-in state [:workspace-local :edition])]
        (cond-> state
          id (assoc-in [:workspace-local :edit-path id :edit-mode] mode))))

    ;;ptk/WatchEvent
    ;;(watch [_ state stream]
    ;;  (let [id (st/get-path-id state)]
    ;;    (cond
    ;;      (and id (= :move mode)) (rx/of (common/finish-path "change-edit-mode"))
    ;;      (and id (= :draw mode)) (rx/of (start-draw-mode))
    ;;      :else (rx/empty))))
    ))

(defn stop-path-edit []
  (ptk/reify ::stop-path-edit
    ptk/UpdateEvent
    (update [_ state]
      (let [id (get-in state [:workspace-local :edition])]
        (update state :workspace-local dissoc :edit-path id)))))

(defn start-path-edit
  [id]
  (ptk/reify ::start-path-edit
    ptk/UpdateEvent
    (update [_ state]
      (let [edit-path (get-in state [:workspace-local :edit-path id])]

        (cond-> state
          (or (not edit-path) (= :draw (:edit-mode edit-path)))
          (assoc-in [:workspace-local :edit-path id] {:edit-mode :move
                                                      :selected #{}
                                                      :snap-toggled true})

          (and (some? edit-path) (= :move (:edit-mode edit-path)))
          (assoc-in [:workspace-local :edit-path id :edit-mode] :draw))))

    ptk/WatchEvent
    (watch [_ state stream]
      (let [mode (get-in state [:workspace-local :edit-path id :edit-mode])]
        (rx/concat
         (rx/of (change-edit-mode mode))
         (->> stream
              (rx/take-until (->> stream (rx/filter (ptk/type? ::start-path-edit))))
              (rx/filter #(= % :interrupt))
              (rx/take 1)
              (rx/map #(stop-path-edit))))))))


(defn make-corner []
  (ptk/reify ::make-corner
    ptk/WatchEvent
    (watch [_ state stream]
      (let [id (st/get-path-id state)
            page-id (:current-page-id state)
            shape (get-in state (st/get-path state))
            selected-points (get-in state [:workspace-local :edit-path id :selected-points] #{})
            new-content (reduce ugp/make-corner-point (:content shape) selected-points)
            [rch uch] (changes/generate-path-changes page-id shape (:content shape) new-content)]
        (rx/of (dwc/commit-changes rch uch {:commit-local? true}))))))

(defn make-curve []
  (ptk/reify ::make-curve
    ptk/WatchEvent
    (watch [_ state stream]
      (let [id (st/get-path-id state)
            page-id (:current-page-id state)
            shape (get-in state (st/get-path state))
            selected-points (get-in state [:workspace-local :edit-path id :selected-points] #{})
            new-content (reduce ugp/make-curve-point (:content shape) selected-points)
            [rch uch] (changes/generate-path-changes page-id shape (:content shape) new-content)]
        (rx/of (dwc/commit-changes rch uch {:commit-local? true}))))))
