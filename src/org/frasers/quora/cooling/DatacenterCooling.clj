(comment
  ; Parallelized solution to Quora Datacenter Cooling found at http://www.quora.com/challenges
  )

(ns org.frasers.quora.cooling.DatacenterCooling)
  ;(:use clojure.contrib.duck-streams))

(def w 4)
(def h 3)
(def datacenterinput '(2 0 0 0 0 0 0 0 0 0 3 1))

;(def w 4)
;(def h 4)
;(def datacenterinput '(2 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3))


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

(def datacentermap
  (zipmap (for [h (range h) w (range w)] [w h])
                        datacenterinput))


; instead of hard coding these I need to calculate them based on the above datacentermap
(def startx 0)
(def starty 0)

; count the number of c in col
(defn count-num [x col]
  (count (filter #(= x %) col)))

; total rooms in the grid that we will need to pass through
(def tr (count-num 0 datacenterinput))

; this is used to count how many solutions we find
(def total-solutions (atom 0))

(defn above [[x y]]
  ; I could probably remove the pos check or else to make this consistent check for overflow too
  (if (pos? y)
    [x (dec y)]))

(defn below [[x y]]
  (if (< y (dec h))
    [x (inc y)]))

(defn left [[x y]]
  (if (pos? x)
    [(dec x) y]))

(defn right [[x y]]
  (if (< x (dec w))
    [(inc x) y]))

(defn determine-possible-next-directions [current-map coords]
  (filter #(not (nil? %))
    (for [step [left right above below]]
          (step coords)))
  )


; datacentermap - map of where we are at so far
; [x y] - coordinates of current locations
; ts - counter of total steps taken so far
(defn seek-end [datacentermap coords ts]
  ;(println (format "loc: %d %d" x y))
  ;(prn "bc = " bc)
  (if (= 3 (datacentermap coords)) ; see if we are at the end
    (if (= ts (inc tr)) ; redundant check - make sure we passed through all the rooms and are really done
      (swap! total-solutions inc)) ; if at the end, update the counter
    (let [newmap  (assoc datacentermap coords 9) ; update the map to show where we walked already
          paths   ; figure out what our possible valid next steps could be
                  (for [next-step (determine-possible-next-directions newmap coords)
                      :let [next-room-value (newmap next-step)]
                      :when
                        (or
                          (= 0 next-room-value) ; we can move into rooms still marked "0"
                          (and
                            (= ts tr) ; confirm before moving onto end room we have passed through all rooms
                            (= 3 next-room-value)))]
                  next-step)
          ]
      ;(prn paths)
      (if (> (count paths) 1)
        (dorun (map #(seek-end newmap % (inc ts)) (rest paths)))) ; kickoff new threads for other paths to leverage cores
      (if (pos? (count paths))
        (recur newmap (first paths) (inc ts)))) ; stay on this thread for the main path of execution
    )
  )
    ; Step 1 - see where we are... are we done?
    ; Step 2 - loop through all the paths and kick off a new agent for each one
    ; Step 3 - could we just recur for one of the branches to avoid creating too many agents?


(defn -main []
    ; validation: there should only be one 2 and one 3

    ; Now you have a map of [w h] to map entry, so for example the '2' is value for key [0 0]
    ; (prn datacentermap)
    (swap! total-solutions (fn [ignored] 0)) ; this feels wrong - what is an easier way?
    (seek-end
      ; this assoc to "9" marks where we have walked
      datacentermap
      ; this tells seek-end where we currently are
      [startx starty] 0)
    (prn (format "Total routes found: %d" @total-solutions))
  )

(dorun (for [x (range 10)] (time (-main))))

;(time (-main))


