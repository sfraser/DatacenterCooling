(comment
  ; Parallelized solution to Quora Datacenter Cooling found at http://www.quora.com/challenges
  )

(ns org.frasers.quora.cooling.DatacenterCooling)
  ;(:use clojure.contrib.duck-streams))

;(def w 4)
;(def h 3)
;(def datacenterinput '(2 0 0 0 0 0 0 0 0 0 3 1))

(def w 4)
(def h 4)
(def datacenterinput '(2 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3))


;(def w 7)
;(def h 8)
;(def datacenterinput '(2 0 0 0 0 0 0
;0 0 0 0 0 0 0
;0 0 0 0 0 0 0
;0 0 0 0 0 0 0
;0 0 0 0 0 0 0
;0 0 0 0 0 0 0
;0 0 0 0 0 0 0
;3 0 0 0 0 1 1))


(def counter (atom 0))

(defn above [x y]
  ; I could probably remove the pos check or else to make this consistent check for overflow too
  (if (pos? y)
    [x (dec y)])
  )

(defn below [x y]
  (if (< y (dec h))
    [x (inc y)])
  )

(defn left [x y]
  (if (pos? x)
    [(dec x) y])
  )

(defn right [x y]
  (if (< x (dec w))
    [(inc x) y])
  )


(defn seek-end [datacentermap [x y] bc]
  ;(println (format "loc: %d %d" x y))
  ;(prn "bc = " bc)
  (if (= 3 (datacentermap [x y])) ; see if we are at the end
    (if (not (some zero? (vals datacentermap))) ; make sure we passed through all the "rooms"
      (swap! counter inc)) ; if at the end, update the counter
    (let [newmap (assoc datacentermap [x y] 9) ; update the map to show where we walked already
          paths (for [step [left right above below]
                  :when (or
                    (= 0 (newmap (step x y)))
                    (= 3 (newmap (step x y))))]
                  (step x y))]
      (dorun (map #(seek-end newmap [(first %) (second %)] (conj bc %)) paths))))
  )
    ; Step 1 - see where we are... are we done?
    ; Step 2 - loop through all the paths and kick off a new agent for each one
    ; Step 3 - could we just recur for one of the branches to avoid creating too many agents?


(defn -main [w h datacenterinput]
  (let [datacentermap (zipmap
                        (for [h (range h) w (range w)] [w h])
                        datacenterinput)
        ; now figure out what element had the starting point - hard coded for now
        startx 0
        starty 0
        ]
    ; validation: there should only be one 2 and one 3


    ; Now you have a map of [w h] to map entry, so for example the '2' is value for key [0 0]
    ;(prn datacentermap)
    (seek-end
      ; this assoc to "9" marks where we have walked
      datacentermap
      ; this tells seek-end where we currently are
      [startx starty] [[startx starty]])
    (prn (format "Counter = %d" @counter))
  ))

(time( -main w h datacenterinput))


