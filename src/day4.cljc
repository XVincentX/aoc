(ns day4 (:require [clojure.spec.alpha :as s]
                   [clojure.string :refer [split ends-with?]]
                   [spec-tools.core :as st]))

(def input (split (clojure.core/slurp "./day4input.txt") #"\n\n"))
(def passports (->> input
                    (map #(re-seq #"(\S+):(\S+)" %))
                    (map (fn [value]
                           (st/coerce ::passport (reduce #(assoc %1 (keyword (second %2)) (nth %2 2)) {} value) st/string-transformer)))))

(s/def ::passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid] :opt-un [::cid]))

(defn remove-last-chars [s n]
  (subs s 0 (- (count s) n)))

(defn in-range [val x y]
  (and (>= val x) (<= val y)))

(defn string-in-range [val x y]
  (let [number (remove-last-chars val 2)
        converted (Integer. number)]
    (and (some? converted) (in-range converted x y))))

; Disable the following for part 1
(s/def ::byr (s/and integer? #(>= % 1920) #(<= % 2002)))
(s/def ::iyr (s/and integer? #(>= % 2010) #(<= % 2020)))
(s/def ::eyr (s/and integer? #(>= % 2020) #(<= % 2030)))

(s/def ::hgt
  (s/or
   :cm (s/and #(ends-with? % "cm")
              #(string-in-range % 150 193))
   :in (s/and #(ends-with? % "in")
              #(string-in-range % 59 76))))

(s/def ::hcl (s/and string? #(re-matches #"^\#[0-9a-f]{6}$" %)))
(s/def ::ecl #(some? (some #{%} #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})))
(s/def ::pid (s/and string? #(re-matches #"^[0-9]{9}$" %)))

(count (filter (partial s/valid? ::passport) passports))
