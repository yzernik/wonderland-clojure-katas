(ns alphabet-cipher.coder)

(defn to_int [c]
  (- (int c) (int \a)))

(defn from_int [n]
  (char (+ (int \a) n)))  

(defn get_offset [kw i]
  (let [j (mod i (count kw))]
    (to_int (.charAt kw j))))

(defn shift_char [c offset]
  (let [s (+ offset (to_int c))
        r (+ 1 (to_int \z))]
    (from_int (mod s r))))

(defn encode_char [kw]
  (fn [i c]
    (shift_char c (get_offset kw i))))

(defn decode_char [kw]
  (fn [i c]
    (shift_char c (- (get_offset kw i)))))

(defn encode [keyword message]
  (apply str (map-indexed (encode_char keyword) message)))

(defn decode [keyword message]
  (apply str (map-indexed (decode_char keyword) message)))
