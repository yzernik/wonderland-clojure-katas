(ns alphabet-cipher.coder)

(defn to-int [c]
  (- (int c) (int \a)))

(defn from-int [n]
  (char (+ (int \a) n)))  

(defn get-offset [kw i]
  (let [j (mod i (count kw))]
    (to-int (.charAt kw j))))

(defn shift-char [c offset]
  (let [s (+ offset (to-int c))
        r (+ 1 (to-int \z))]
    (from-int (mod s r))))

(defn encode-char [kw]
  (fn [i c]
    (shift-char c (get-offset kw i))))

(defn decode-char [kw]
  (fn [i c]
    (shift-char c (- (get-offset kw i)))))

(defn encode [keyword message]
  (apply str (map-indexed (encode-char keyword) message)))

(defn decode [keyword message]
  (apply str (map-indexed (decode-char keyword) message)))
