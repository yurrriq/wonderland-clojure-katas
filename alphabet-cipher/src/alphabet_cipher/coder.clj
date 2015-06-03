(ns alphabet-cipher.coder
  "Encode and decode messages using [The Alphabet Cipher][1].
  [1]: http://en.wikipedia.org/wiki/The_Alphabet_Cipher")

;;;; ## Transcoding

(defn- char->row
  "Given a character, returns the distance to it,
  going forward in the alphabet, from a."
  [c]
  (- (int c) (int \a)))

(defn- row<-char
  "Given a character, returns the distance from it,
  going backward in the alphabet, to a."
  [c]
  (- 26 (char->row c)))

(defn- transcode
  "Given function that takes a character and returns a row number, i.e.
  `char->row` or `row<-char`, returns a function that given a character
  and a cipher, returns the character in the given cipher at the row returned by
  applying the previously given function to the given character."
  [f]
  (fn [c cipher]
    (nth cipher (f c))))

(def ^:private encode'
  "Given a character and a cipher, returns a character by encoding
  the given character with the given cipher."
  (transcode char->row))

(def ^:private decode'
  "Given a character and a cipher, returns a character by decoding
  the given character with the given cipher."
  (transcode row<-char))


;;;; ## Ciphers

(defn- char->cipher
  "Given a starting character, returns the corresponding alphabet cipher."
  [c]
  (->> (cycle "abcdefghijklmnopqrstuvwxyz")
       (drop (char->row c))
       (take 27)))

(defn- ciphers
  "Given a secret keyword and a message length, returns a lazy sequence of the
  ciphers for each letter of the message."
  [keyword n]
  (->> (take n (cycle keyword))
       (map char->cipher)))


;;;;; ## Public API

(defn encode
  "Given a secret keyword and a message, encodes the message using the keyword
  and returns the encoded message."
  [keyword message]
  (->> (ciphers keyword (count message))
       (map encode' message)
       (apply str)))

(defn decode
  "Given a secret keyword and an encoded message, decodes the message
  using the keyword and returns the decoded message."
  [keyword message]
  (->> (ciphers message (count message))
       (map decode' (take (count message) (cycle keyword)))
       (apply str)))
