(ns readsvg
  (:use [clojure.string :only (split trim)]
        [clojure.java.io :only (reader)]))
       
(defn to-double [l]
  (vec (remove nil? (map #(let [s (split % #",")]
                             (when (= 2 (count s))
                               [(Double. ^String (first s)) (Double. ^String (second s))])) l))))

(defn process-lines [lines accu]
  (cond (empty? lines) (to-double (distinct (vec (drop-last accu))))
        :else 
        (let [line (trim (first lines))
              tokens (split (first (split line #"\"")) #"\s")]
          (cond 
          (= (tokens 0) "C") (recur (rest lines) (rest tokens))
          (> (count accu) 0) (recur (rest lines) (concat accu tokens))
          :else (recur (rest lines) accu)))))

;(defn read-svg2 [^FileHandle rdr]
;  (process-lines (line-seq (.reader rdr 8192)) nil))

(defn read-svg2 [file-name]
  (with-open [rdr (reader file-name)]
    (process-lines (line-seq rdr) nil)))

(defn read-svg [path]
  (fn [^String file-name & args]
    (let [values  (merge (apply hash-map args) {:polygon (read-svg2 (str path file-name))})]
      (if (pos? (.indexOf file-name "L"))
        (conj values {:make-loop true})
        values))))



