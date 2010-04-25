(comment
  "Parallelized solution to Quora Datacenter Cooling found at http://www.quora.com/challenges.

  Puts the datacentermap into a map of coordinates to map cell value. So for example is we are starting
  at x/y coordinates '0' and '1', there will be a key in the datacentermap of [0 1] whose value is 2.

  So throughout the convention is to treat 'coordinates' as a 2 member vector with the x value first.

  Also note that as per the challenge, the coordinate system starts with [0 0] being in the upper lefthand
  corner.

  Currently this solution is very slow on my laptop - I am going to guess because of the usage of maps and
  vectors. A more ideal solution might treat the datacentermap as a structure built on primitive types -
  arrays or even numbers with the values shifted into them. That would enable some short circuit logic that
  is not easily done with the current model. For example, if you want to figure out if an area of the map
  is now unsolveable if entered, you need to iterate the map, as opposed to being able to address it more
  simply as rows and columns."
  )

(ns org.frasers.quora.cooling.DatacenterCooling)
  ;(:use clojure.contrib.duck-streams))

;;;;
;;;; Challenge Inputs
;;;;

;(def w 4)
;(def h 3)
;(def datacenterinput '(2 0 0 0 0 0 0 0 0 0 3 1))

;(def w 4)
;(def h 4)
;(def datacenterinput '(2 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3))


(def w 7)
(def h 8)
(def datacenterinput '(2 0 0 0 0 0 0
0 0 0 0 0 0 0
0 0 0 0 0 0 0
0 0 0 0 0 0 0
0 0 0 0 0 0 0
0 0 0 0 0 0 0
0 0 0 0 0 0 0
3 0 0 0 0 1 1))

;;;;; Create a map of the datacenter to start with
(def datacentermap
  (zipmap (for [h (range h) w (range w)] [w h])
                        datacenterinput))

;;;;; Need to calculate these - for now they are hard coded

; starting coordinates
(def startx 0)
(def starty 0)

(defn count-num
  "Count the number of 'x' in col"
  [x col]
  (count (filter #(= x %) col)))

;Calculates the number of '0' values in the datacentermap, which are
;the 'rooms' of the datacenter we must pass through"
(def tr (count-num 0 datacenterinput))

; counter of how many solutions we find
(def total-solutions (atom 0))

(defn above
  "Returns coordinates of the cell ABOVE [x y], or nil if not applicable"
  [[x y]]
  (if (pos? y)
    [x (dec y)]))

(defn below
  "Returns coordinates of the cell BELOW [x y], or nil if not applicable"
  [[x y]]
  (if (< y (dec h))
    [x (inc y)]))

(defn left
  "Returns coordinates of the cell LEFT [x y], or nil if not applicable"
  [[x y]]
  (if (pos? x)
    [(dec x) y]))

(defn right
  "Returns coordinates of the cell RIGHT [x y], or nil if not applicable"
  [[x y]]
  (if (< x (dec w))
    [(inc x) y]))

(defn determine-possible-next-directions
  "Returns a requence of [x y] coordinates of all possible directions from the passed in coord"
  [coords]
  (filter #(not (nil? %))
    (for [step [left right above below]]
          (step coords)))
  )

; memoized wrapper for the above function - seems to help!
(def my-determine-possible-next-directions (memoize determine-possible-next-directions))

(defn seek-end
  "Primary function of this program. Recurs and calls itself via (p)map as it brute force tries
  to find solutions. We are at the datacentermap location specified by coords when called, and the
  ts counter is a 'total steps taken' counter we use as a shortcut to confirming that we have
  passed through every room (because it can be compared to tr)."
  [datacentermap coords ts]
  (if (= 3 (datacentermap coords)) ; see if we are at the end
    (if (= ts (inc tr)) ; redundant check - make sure we passed through all the rooms and are really done
      (swap! total-solutions inc)) ; if at the end, update the counter
    (let [newmap  (assoc datacentermap coords 9) ; update the map to show where we walked already
          paths   ; figure out what our possible valid next steps could be
                  (for [next-step (my-determine-possible-next-directions coords)
                      :let [next-room-value (newmap next-step)]
                      :when
                        (or
                          (= 0 next-room-value) ; we can move into rooms still marked "0"
                          (and
                            (= ts tr) ; confirm before moving onto end room we have passed through all rooms
                            (= 3 next-room-value)))]
                  next-step)
          ]
      ; this next section is structured to allow playing with some parallelism
      (if (> (count paths) 1)
        ; pmap will not be effective here without throttling the size of the thread pool
        (dorun (map #(seek-end newmap % (inc ts)) (rest paths)))) ; kickoff new threads for other paths to leverage cores
      (if (pos? (count paths))
        (recur newmap (first paths) (inc ts)))) ; stay on this thread for the main path of execution
    )
  )
(defn -main []
    ; validation: there should only be one 2 and one 3

    ; clear the total-colutions counter in case we are doing multiple runs
    (swap! total-solutions (fn [ignored] 0)) ; this feels wrong - what is an easier way?
    (seek-end datacentermap [startx starty] 0)
    (prn (format "Total routes found: %d" @total-solutions))
  )

;(dorun (for [x (range 10)] (time (-main))))

(time (-main))


