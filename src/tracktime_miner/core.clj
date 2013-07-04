(ns tracktime-miner.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str :refer [split]]))

(def TASKS-FILE "/home/manuel/Sync/org/tasks.csv")

;; Possibile elaborare il tutto senza caricarlo in memoria? Oppure caricandolo progressivamente solo in parte, scartando le righe già elaborate?

(defn- load-file
  "Load a CSV file in memory as a vector (file) of vectors (rows)."
  [path]
  (with-open [in-file (io/reader path)]
    (doall
     (csv/read-csv in-file))))

(defrecord entry [start desc end duration min])

(defn- make-entry [row]
  (apply ->entry row))

(defn read-entries [path]
  (map make-entry (load-file path)))

(defn- start-date [entry]
  (first (split (:start entry) #" ")))

(defn- contains-pattern [substring]
  (re-pattern (str ".*" substring ".*")))

(defn desc?
  "Predicate that checks if the given entry has a substring s in his description."
  [entry s]
  (let [pattern (contains-pattern s)
        description (.desc entry)]
    (not (nil? (re-matches pattern description)))))

(defn group-by-desc [entries desc]
  (->> entries
       (filter #(desc? % desc))
       (group-by :desc)))

(defn by-desc-day
  "Groups the entries which descriptions contains the given desc as substring first by description then by date.

Returns a map that associates to every complete task description with
the given `desc` as substring, a map that associates to every date in
which a task with that description was tracked a vector of *entries*."
  [entries desc]
  (reduce #(assoc %1 (key %2) (group-by start-date (val %2)))
          {}
          (group-by-desc entries desc)))

;; "E3057"

(defn parse-int [s]
  (Integer/parseInt s))

(defn minutes-for-day
  "Takes a map (day->[entries]) and produces a map (day->minutes)."
  [day-map]
  (into {} (for [[day v] day-map]
             [day (reduce + (map (comp parse-int :min) v))])))

(defn sum-minutes
  "Produces a map {desc {day minutes ...} ...} from a list of entries
using `desc` as a substring to search their description."

  [entries desc]
  (into {} (for [[d m] (by-desc-day entries desc)]
             [d (minutes-for-day m)])))

(sum-minutes (read-entries TASKS-FILE) "E3057")
