(ns rtroberts.d2-armor-dups
  (:require
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [taoensso.timbre :as log]
   [clojure.math :refer :all]
   [clustering.core.qt :as qt]
   [clustering.core.k-means :as k-means]
   [clustering.core.hierarchical :as hier]
   [clojure.math.combinatorics :as combo]
   [rtroberts.constants :as constants]
   )
  (:gen-class))

(def file
  (->> (clojure.java.io/file "/Users/ryanroberts/Downloads/")
       file-seq
       (filter #(.isFile %))
       (filter #(clojure.string/includes? (str %) "destinyArmor"))
       (sort-by #(.lastModified %))
       last
       str)
  )

(def cols
  {:Id :Id
   :Resilience_Base  :Resilience
   :Recovery_Base :Recovery
   :Mobility_Base :Mobility
   :Discipline_Base :Discipline
   :Intellect_Base :Intellect
   :Strength_Base :Strength
   :Total_Base :Total
   :Masterwork_Tier :Energy
   :Equippable :Equippable              ; this is what class can wear
   :Type :Type                          ; this is the armor slot
   :Tag :Tag
   :Tier :Tier
   :Name :Name
   :Artifice? :Artifice?})

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (map #(clojure.string/replace % #" " "_"))
            (map #(clojure.string/replace % #"[\(\)]" ""))
            (map keyword) ;; Drop if you want string keys instead
            repeat)
       (rest csv-data)))

(defn normalize [n]
  (/ (- n 2)
     28))

(def group-cols
  [:Mobility
   :Resilience
   :Recovery
   :Discipline
   :Intellect
   :Strength])

#_(let [armor {:Mobility "2"}]
    (apply + (map parse-long (vals (select-keys armor [:Mobility :Resilience :Recovery]))))
  )

(defn group-armor [armor]
  (let [a armor
        class-total (apply + (vals (select-keys a [:Mobility :Resilience :Recovery])))
        ability-total (apply + (vals (select-keys a [:Discipline :Intellect :Strength])))
        normie (fn [x denom] (/ (- x 2) (- denom 6)))
        ]
    [
     (some-> a :Mobility (normie class-total))
     (some-> a :Resilience (normie class-total))
     (some-> a :Recovery (normie class-total))
     (some-> a :Discipline (normie ability-total))
     (some-> a :Intellect (normie ability-total))
     (some-> a :Strength (normie ability-total))
     ]
    ))

(def armor-csv
  (with-open [reader (-> file
                         io/input-stream
                         io/reader)]
    (doall (->> (csv/read-csv reader)
                csv-data->maps
                (map (fn [x] (assoc x :Artifice? (some #(= % "Artifice Armor*") (->> x vals)))))
                (map #(select-keys % (keys cols)))
                (map #(clojure.set/rename-keys % cols))

                (filter #(= (:Tier %) "Legendary"))
                #_(filter #(not (clojure.string/starts-with? (:Name %) "Candescent")))
                #_(filter #(not (contains? #{"infuse" "junk"} (:Tag %))))
                (map (fn [x] (update-in x [:Total] #(parse-long %))))
                (map (fn [x] (update-in x [:Mobility] #(parse-long %))))
                (map (fn [x] (update-in x [:Resilience] #(parse-long %))))
                (map (fn [x] (update-in x [:Recovery] #(parse-long %))))
                (map (fn [x] (update-in x [:Discipline] #(parse-long %))))
                (map (fn [x] (update-in x [:Intellect] #(parse-long %))))
                (map (fn [x] (update-in x [:Strength] #(parse-long %))))
                (map (fn [x] (update-in x [:Energy] #(parse-long %))))
                (filter (fn [a] (> (:Total a) 0)))
                ))))

(defn stem2 [x]
(cond
    (<= 0.0 x 0.2) :ZERO
    (<= 0.2 x 0.4) :LOW
    (<= 0.4 x 0.6) :MED
    (<= 0.6 x 0.8) :HI
    (<= 0.8 x 1.0) :BONKERS
    )
  )

#_(mapv float (group-armor {:Mobility 20 :Recovery 10 :Resilience 2 :Discipline 2 :Intellect 2 :Strength 30}))

(defn stem [n]
  (-> n
      (/ 10)
      clojure.math/round))

(let [armor {:Mobility "10"}]
  ((apply juxt group-cols) armor))

#_(defn grouping [armor]
  (concat [(:Equippable armor) (:Type armor)]
          (mapv stem3 ((apply juxt group-cols) armor))))

(defn grouping2 [armor]
  (concat [(:Equippable armor) (:Type armor)]
          (mapv stem2 (group-armor armor))))

(defn wrap-ors [items]
  (str "(" items ")"))

(defn spit-clusters [filename armors]
  (spit filename "")
  (doseq [a armors
          :when (seq a)
          :let [res
                (->> a
                     (map :Id)
                     (map #(str "id:" %))
                     (interpose " or ")
                     (clojure.string/join " ")
                     (wrap-ors))]]
    (spit filename res :append true)
    (spit filename "\n" :append true)
    ))

(defn spit-results [filename coll]
  (->> coll
       (map :Id)
       (map #(str "id:" %))
       (interpose " or ")
       #_ (take 5)
       (clojure.string/join "\n")
       (wrap-ors)
       (spit filename)))

;; this is saying this is the threshold to consider a stat "spiky"
(def bonkers 0.70)

;; this is very obviously for two BIG spikes
#_(def spiky-permutations
    (let [orig [bonkers 0 0]]
      (set (for [class-perms (combo/permutations orig)
                 ability-perms (combo/permutations orig)]
             (concat class-perms ability-perms)))))

(def spiky-permutations
  (->> (combo/permutations [18 8 0])
       (map #(into [] %))
       (map #(concat % [0 0 0]))
       (map vec)))

(defn missing-permutations [armor]
  (remove
   (fn [p]
     (some #(= true %) (for [arm armor
                             :let [armor-vals ((apply juxt group-cols) arm)
                                   p-indices (keep-indexed #(when (>= %2 1) %1) p)]]
                         (every? #(>= (get armor-vals %)
                                      (get p %)) p-indices))))
   spiky-permutations))

#_(->> [
        {:Mobility 20 :Recovery 10 :Resilience 2 :Discipline 2 :Intellect 15 :Strength 15}
        {:Mobility 10 :Recovery 20 :Resilience 2 :Discipline 2 :Intellect 15 :Strength 15}
        {:Mobility 10 :Recovery 2 :Resilience 20 :Discipline 2 :Intellect 15 :Strength 15}
        {:Mobility 2 :Recovery 10 :Resilience 20 :Discipline 2 :Intellect 15 :Strength 15}
        ]
       missing-permutations
       (map #(zipmap group-cols %))
       )

;; Clustering

(defn distance [a b]
  (when (or (not (map? a))
            (not (map? b)))
    (println "umm not a map??" a b ))
  (let [av (group-armor a)
        bv (group-armor b)
        _ (assert (= (count av) (count bv)) "Umm the armor doesn't match?")]
    (->> av
         (map-indexed (fn [i el]
                        (if (and (nil? el) (nil? (get bv i)))
                          0
                          (let [diff (clojure.core/abs (- el (get bv i)))]
                            (if (>= diff 0.1)
                              (inc diff)
                              diff)))))
         (apply +))))

(defn average [armors]
  (let [avg-fn (fn [xs k]
                 (let [sum (->> xs (map k) (apply +))]
                   (/ sum (count xs))))]
    {:Mobility (some-> armors (avg-fn :Mobility))
     :Resilience (some-> armors (avg-fn :Resilience))
     :Recovery (some-> armors (avg-fn :Recovery))
     :Discipline (some-> armors (avg-fn :Discipline))
     :Intellect (some-> armors (avg-fn :Intellect))
     :Strength (some-> armors (avg-fn :Strength))
     }))

#_(->> [
        {:Mobility 20 :Recovery 10 :Resilience 2 :Discipline 2 :Intellect 15 :Strength 15}
        {:Mobility 10 :Recovery 20 :Resilience 2 :Discipline 2 :Intellect 15 :Strength 15}
        {:Mobility 10 :Recovery 2 :Resilience 20 :Discipline 2 :Intellect 15 :Strength 15}
        {:Mobility 2 :Recovery 10 :Resilience 20 :Discipline 2 :Intellect 15 :Strength 15}
        ]
       ;; average
       (k-means/init-means 3)
       )


(defn plug-stem [armor]
  (->> constants/plugs-permutations
       (sort-by #(distance (select-keys armor [:Mobility :Resilience :Recovery]) %))
       first))


#_(-> {:Mobility 28 :Resilience 2 :Recovery 2} plug-stem)

(defn cluster [coll]
  (for [cluster (qt/cluster distance coll 1 2)]
    (->> cluster
         (sort-by (juxt :Total :Artifice? :Energy))
         reverse
         #_(drop 1)))
  )

;; k-means clusters
;; This basically won't work properly because these items aren't truly averageable. Not a good fit.
(defn k-means-cluster [armors]
  (println (-> armors first (select-keys [:Equippable :Type])))
  (let [size (if (< (count armors) 20)
               (count armors)
               20)
        init (k-means/init-means size armors)
        clusters (k-means/cluster distance average armors init 0)
        _ (println (count clusters) " clusters")]
    (for [cluster clusters
          :when (> (count cluster) 1)]
            (->> cluster
                 (sort-by (juxt :Total :Artifice? :Energy))
                 reverse
                 )))
  )

;; Print out the Class/Slot for which I'm missing a spiky permutation
(->> armor-csv
     (group-by (juxt :Equippable :Type))
     (keep (fn [[k armor]]
             (for [missing-perms? (missing-permutations armor)
                   :when (seq missing-perms?)
                   :let [perm (zipmap group-cols missing-perms?)
                         filtered (->> perm
                                       (filter #(<= 1 (val %)))
                                       (into {}))]]
               (do #_(println perm)
                   {:Key k
                    :Missing filtered}))
             ))
     flatten
     distinct
     (filter #(or
               (every? #{:Mobility :Recovery} (keys (:Missing %)))
               (every? #{:Resilience :Recovery} (keys (:Missing %)))
               (every? #{:Mobility :Resilience} (keys (:Missing %)))
               ))
     (reduce (fn [acc val]
               (if (and (#{"Titan" "Warlock"} (first (:Key val)))
                        (some #{:Mobility} (keys (:Missing val))))
                 acc
                 (update acc (:Key val) conj (:Missing val))))
             {})
     (map pr-str)
     (clojure.string/join "\n")
     (spit "/Users/ryanroberts/more-armor.txt")
     )

;; using permutations of actual stems to group armor together
;; not really practical as there are 900 distinct permutations (conservatively)
;; so that's 900 per slot per character.
(let [filename "/Users/ryanroberts/primary-armors.txt"]
  (spit filename "")
  (->> armor-csv
       (group-by (juxt :Equippable :Type))
       vals
       (mapcat #(-> (group-by plug-stem %) vals))
       (filter #(> (count %) 1))
       (sort-by count)
       (spit-clusters filename)
       ))


(defn spit-dups
  "Spit out duplicate clustered armor"
  []
  (time
   (->> armor-csv
        (group-by (juxt :Equippable :Type))
        vals
        (mapcat cluster)
        #_(mapcat k-means-cluster)
        (sort-by count)
        #_(filter seq)
        (spit-clusters "/Users/ryanroberts/delete-armor.txt"))))

(defn -main
  [& args]
  ;; Print out duplicate clustered armor
  (spit-dups))
