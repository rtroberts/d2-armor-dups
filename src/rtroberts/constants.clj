(ns rtroberts.constants
  (:require
   [clojure.math.combinatorics :as combo]
   ))

;; This was taken directly from the excel spreadsheet of plugs.
;; it was essentially the same for class stats as well as ability stats.
;; therefore, all combos of class stats should be:
;; (permutations plugs plugs)
(def plugs [
             [1 1 15]
             [1 15 1]
             [1 15 1]
             [1 11 5]
             [1 1 15]
             [1 5 11]
             [5 1 11]
             [5 11 1]
             [11 5 1]
             [11 1 5]
             [15 1 1]
             [15 1 1]
            ])

(def plugs-permutations
  (->> (for [i (range 0 (count plugs))
             j (range 0 (count plugs))
             :when (not= i j)
             :let [plug1 (get plugs i)
                   plug2 (get plugs j)
                   ;total (+ (apply + plug1) (apply + plug2))      ; to normalize the vals
                   ]]
         {:Mobility  (+ (nth plug1 0) (nth plug2 0))
          :Resilience (+ (nth plug1 1) (nth plug2 1))
          :Recovery (+ (nth plug1 2) (nth plug2 2))})
       distinct
       ))


  #_(->> (for [i (range 0 (count plugs))
             j (range 0 (count plugs))
             :when (not= i j)
             ai (range 0 (count plugs))
             aj (range 0 (count plugs))
             :when (not= ai aj)

             :let [plug1 (get plugs i)
                   plug2 (get plugs j)
                   aplug1 (get plugs ai)
                   aplug2 (get plugs aj)


                                        ;total (+ (apply + plug1) (apply + plug2))      ; to normalize the vals
                   ]]
         {:Mobility  (+ (nth plug1 0) (nth plug2 0))
          :Resilience (+ (nth plug1 1) (nth plug2 1))
          :Recovery (+ (nth plug1 2) (nth plug2 2))
          :Discipline (+ (nth aplug1 0) (nth aplug2 0))
          :Intellect (+ (nth aplug1 1) (nth aplug2 1))
          :Strength (+ (nth aplug1 2) (nth aplug2 2))
          }
         )
       distinct
       count
       );; => 900
