(comment
  ; Parallelized solution to Quora Datacenter Cooling found at http://www.quora.com/challenges
  )

(ns org.frasers.quora.cooling.DatacenterCooling
  (:use clojure.contrib.duck-streams))

(def w 4)
(def h 3)
(def datacenterinput '(2 0 0 0 0 0 0 0 0 0 3 1))


(defn seek-end [datacentermap x y]
  (let [paths (for [dx [(dec x) x (inc x)] dy [(dec y) y (inc y)]
                   :when (and
                            (not (and (= dx x) (= dy y)))
                            (not (neg? dx))
                            (not (neg? dy))
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


