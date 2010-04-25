(comment
  ; Parallelized solution to Quora Datacenter Cooling found at http://www.quora.com/challenges
  )

(ns org.frasers.quora.cooling.DatacenterCooling)
  ;(:use clojure.contrib.duck-streams))

(def w 4)
(def h 3)
(def datacenterinput '(2 0 0 0 0 0 0 0 0 0 3 1))

(defn above [x y]
  ; I could probably remove the pos check or else to make this consistent check for overflow too
  (if (pos? y)
    [x (dec y)])
  )

(defn below [x y]
  [x (inc y)]
  )

(defn left [x y]
  (if (pos? x)
  [(dec x) y])
  )

(defn right [x y]
  [(inc x) y]
  )


(defn seek-end [datacentermap x y]
  (let [paths (for [step [left right above below]
                :when (= 0 (datacentermap (step x y)))]
                (step x y))]
    ; I AM HERE - this is getting the right paths - need to map these into agents
    (prn datacentermap)
    (prn paths)

    ; Step 1 - see where we are... are we done?
    ; Step 2 - loop through all the paths and kick off a new agent for each one
    ; Step 3 - could we just recur for one of the branches to avoid creating too many agents?
  ))


(defn seek-end-old [datacentermap x y]
  (let [paths (for [dx [(dec x) x (inc x)] dy [(dec y) y (inc y)]
                   :when (and
                            (not (and (= dx x) (= dy y))) ; don't move onto the cell we already are on
                            (not (neg? dx)) ; don't move onto cells off the grid
                            (not (neg? dy)) ; ditto
                            ; I AM HERE ELIMINATE DIAGONAL MOVEMENT
                            (= 0 (datacentermap [dx dy]))) ; only move to cells with a 0
                   ]
                [dx dy])]                           
    ; I AM HERE - this is getting the right paths - need to map these into agents
    (prn datacentermap)
    (prn paths)

    ; Step 1 - see where we are... are we done?
    ; Step 2 - loop through all the paths and kick off a new agent for each one
    ; Step 3 - could we just recur for one of the branches to avoid creating too many agents?
    )
  )


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
    (prn datacentermap)
    (seek-end
      ; this assoc to "9" marks where we have walked
      (assoc datacentermap [startx starty] 9)
      ; this tells seek-end where we currently are
      startx starty)
  ))

(-main w h datacenterinput)


