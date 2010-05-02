(comment
  "Solution to heart of Quora Datacenter Cooling problem found at http://www.quora.com/challenges.
  This is not a complete solution yet, and is not parallelized successfully either. Just playing right now.

  Puts the datacentermap into a map of coordinates to map cell value. So for example if we are starting
  at x/y coordinates '0' and '1', there will be a key in the datacentermap of [0 1] whose value is 2.

  So throughout the convention is to treat 'coordinates' as a 2 member vector with the x value first.

  Also note that as per the challenge, the coordinate system starts with [0 0] being in the upper lefthand
  corner.

  Currently this solution is very slow on my laptop. It needs some more intelligence regarding how and
  when it can short circuit a route and give up. Most CPU right now is related to map/hashing activity
  of the vectors. However that time is maybe only 10% of CPU, so I think what this program really needs
  next is some short-circuit logic.

  That being said, getting rid of the collections and using more primitive types would not hurt.
  A more ideal solution might treat the datacentermap as a structure built on primitive types -
  arrays or even numbers with the values shifted into them. That would facilitate some short circuit logic that
  is not easily done with the current model. For example, if you want to figure out if an area of the map
  is now unsolveable if entered, you might want to dynamically iterate the map, which takes CPU. If the model
  was all primitive types you could probably have a much simpler shorthand for calculating things about the map.

  In any case I will keep the model in maps for now, and attempt to build the short circuit logic on top of them
  using additional collections that keep track of things like what columns/rows have been completely filled up.

  I would also like to play with parallization a little more and see if I can get it to scale better over
  multiple threads. Right now just throwing the core function over pmap is not effective.
  "
  )

(ns org.frasers.quora.cooling.DatacenterCooling)
  ;(:use clojure.contrib.duck-streams))

(import '(java.util.concurrent Executors ExecutorService TimeUnit)
        '(java.util.concurrent.locks ReentrantReadWriteLock)
  )

;;;;
;;;; Challenge Inputs
;;;;

; small example from quora website
;(def w 4)
;(def h 3)
;(def datacenterinput '(2 0 0 0 0 0 0 0 0 0 3 1))

; my own slightly larger version of the above that has 4 solutions
;(def w 4)
;(def h 4)
;(def datacenterinput '(
;  2 0 0 0
;  0 0 0 0
;  0 0 0 0
;  0 0 1 3))

; another larger one I use on the core i7 - it has 38 solutions
(def w 5)
(def h 5)
(def datacenterinput '(
  2 0 0 0 0
  0 0 0 0 0
  0 0 0 0 0
  0 0 0 0 0
  0 0 3 1 1))

; Big example from quora website that they solve in C "in under 5 seconds on a 2.4GHz Pentium 4"!
; They note this 5 second time is their best case, and the coder should aim for 1-2 orders of magnitude
; of that score.
;(def w 7)
;(def h 8)
;(def datacenterinput '(
;2 0 0 0 0 0 0
;0 0 0 0 0 0 0
;0 0 0 0 0 0 0
;0 0 0 0 0 0 0
;0 0 0 0 0 0 0
;0 0 0 0 0 0 0
;0 0 0 0 0 0 0
;3 0 0 0 0 1 1))

(defn findkey
  "Find the Value v in Map m and return the Key"
  [m v]
  (first (first (filter #(= v (second %)) (seq m))))
  )

;;;;; Create a map of the datacenter to start with
(def datacentermap
  (zipmap (for [h (range h) w (range w)] [w h])
                        datacenterinput))

; Vectors to track how many rooms are filled in by row and column
; we only count 1's and 3's - we don't count the "2" where we start
(def origrowcounts (vec (for [h (range h)] (count (filter #(or (= 1 %) (= 3 %)) (for [w (range w)] (datacentermap [w h])))))))
(def origcolcounts (vec (for [w (range w)] (count (filter #(or (= 1 %) (= 3 %)) (for [h (range h)] (datacentermap [w h])))))))

; starting coordinates
(def startx (first (findkey datacentermap 2)))
(def starty (second (findkey datacentermap 2)))

; ending coordinates
(def endx (first (findkey datacentermap 3)))
(def endy (second (findkey datacentermap 3)))

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

(defn route-is-invalid
  "Determine if route represented in map m is invalid. Coords passed in are a hint as to
  where we are in the map currently. Returns true or false. Vectors with counts of filled
  in cells by row and col are passed into provide extra data to work with."
  [m [x y] colcounts rowcounts]
  (if (< 0 x (dec w))
    ; if the row or col we are on is populated fully, and there is a row or column to our right AND left
    ; not fully populated, this route may be short circuited    
    (if (= h (colcounts x))
      ;(prn (format "x: %d colcounts: %s" x colcounts))
      (if (and
          (> h (colcounts (dec x)))
          (> h (colcounts (inc x))))
        ;(prn "AXED - COL")
        true
        ))
    )
  (if (< 0 y (dec h))
    ; if the row or col we are on is populated fully, and there is a row or column to our right AND left
    ; not fully populated, this route may be short circuited
    (if (= w (rowcounts y))
      (if (and
          (> w (rowcounts (dec y)))
          (> w (rowcounts (inc y))))
        ;(prn "AXED - ROW")
        true
        ))
    )
    ; prn "NOT AXED")
    false
  )

; memoized wrapper for the above function - seems to help!
(def my-determine-possible-next-directions (memoize determine-possible-next-directions))

; figure out how many worker threads we should have
(def available-procs (.. java.lang.Runtime getRuntime availableProcessors))

; pool of worker threads
(def #^ExecutorService exec-service (Executors/newFixedThreadPool (inc available-procs)))

; ReadWriteLock to coordinate knowing when we are all done
(def rwlock (ReentrantReadWriteLock.))

(defn seek-end-with-lock)

(defn seek-end
  "Primary function of this program. Recurs and calls itself via (p)map as it brute force tries
  to find solutions. We are at the datacentermap location specified by coords when called, and the
  ts counter is a 'total steps taken' counter we use as a shortcut to confirming that we have
  passed through every room (because it can be compared to tr). The rowcounts and colcounts param
  let us track how many filled in rooms by row and col, which can be used by short circuit logic."
  [datacentermap coords ts colcounts rowcounts]
  (if (= 3 (datacentermap coords)) ; see if we are at the end
    (if (= ts (inc tr)) ; redundant check - make sure we passed through all the rooms and are really done
      (swap! total-solutions inc)) ; if at the end, update the counter
    (let [newmap  (assoc datacentermap coords 9) ; update the map to show where we walked already
          newcolcounts (assoc colcounts (first coords) (inc (colcounts (first coords))))
          newrowcounts (assoc rowcounts (second coords) (inc (rowcounts (second coords))))
          routeisbad (route-is-invalid newmap coords newcolcounts newrowcounts) ; is this current route lame?
          paths   ; figure out what our possible valid next steps could be
                  (for [next-step (my-determine-possible-next-directions coords)
                      :let [next-room-value (newmap next-step)]
                      :when
                        (and
                          (not routeisbad) ; need to find a quicker way to shortcircuit than in here, this is a kludge for now
                          (or
                            (= 0 next-room-value) ; we can move into rooms still marked "0"
                            (and
                              (= ts tr) ; confirm before moving onto end room we have passed through all rooms
                              (= 3 next-room-value)))
                          ;(not (route-is-invalid newmap next-step newcolcounts newrowcounts))
                          ; true
                          )]
                  next-step)
          ]
      ; this next section is structured to allow playing with some parallelism
      ;(prn rowcounts)
      ;(prn colcounts)

      (if (> (count paths) 1)
        ; pmap will not be effective here without throttling the size of the thread pool
        ;(dorun (map #(seek-end newmap % (inc ts) newcolcounts newrowcounts) (rest paths)))) ; kickoff new threads for other paths to leverage cores      
        (doseq [path (rest paths)]
          ; I AM HERE - ReadWriteLocks as coordination mechanism have many race defects - any one moment
          ; you might happen to have no read locks but there may still be work in queues. We clearly need
          ; an Atomic counter that is incremented at the moment of mapping the task as opposed to a construct
          ; accessed when the unit of work starts. Once that Atomic int is zero, we know we are done.
          (.execute exec-service #(seek-end-with-lock newmap path (inc ts) newcolcounts newrowcounts))))
      (if (pos? (count paths))
        (recur newmap (first paths) (inc ts) newcolcounts newrowcounts))) ; stay on this thread for the main path of execution
    )
  )

; wrapper to seek-end that tries to use ReadWriteLocks to determine when we are done
; this did not work as there is a race since lock is acquired later after task is mapped
(defn seek-end-with-lock [datacentermap coords ts colcounts rowcounts]
  (.. rwlock readLock lock)
  (seek-end datacentermap coords ts colcounts rowcounts)
  (.. rwlock readLock unlock)
  )



(defn -main []
    ; validation: there should only be one 2 and one 3

    ; clear the total-colutions counter in case we are doing multiple runs
    (swap! total-solutions (fn [ignored] 0)) ; this feels wrong - what is an easier way?
    (seek-end-with-lock datacentermap [startx starty] 0 origcolcounts origrowcounts)

    (.. rwlock writeLock lock)

    ; If needed to (which we don't) we would shutdown the executor service
    ;(.shutdown exec-service)
    ;(.awaitTermination exec-service 60 TimeUnit/SECONDS)

    (prn (format "Total routes found: %d" @total-solutions))
  )

(defmacro my-time
  "Evaluates expr and returns the time it took."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))

(prn (format "Average = %f ms." (/ (reduce + (for [x (range 1000)] (my-time (-main)))) 1000 )))

;(time (-main))


