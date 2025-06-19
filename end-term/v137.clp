
(deftemplate teacher (slot ID) (slot weight))
(deftemplate class (slot ID))
(deftemplate classroom (slot ID) (slot type))
(deftemplate lesson
    (slot ID)
    (slot state)
    (slot class)
    (slot teacher)
    (slot type)
    (multislot time)
    (multislot room))
(deftemplate favorite-time (slot teacher) (multislot time))
(deftemplate refuse-time (slot teacher) (multislot time))

(deftemplate best-move
    (slot type (type SYMBOL) (default none))
    (slot improvement (type NUMBER) (default 0))
    (slot id1 (type INTEGER) (default 0))
    (slot id2 (type INTEGER) (default 0))
    (multislot new_time)
    (multislot new_room)
)

(deffunction some-member$ (?multifield1 $?multifield2)
  "Checks if any item in multifield1 is a member of multifield2."
  (foreach ?item ?multifield1
    (if (member$ ?item ?multifield2) then (return TRUE)))
  (return FALSE)
)

(deffacts initial
    (alltime 101 102 103 104 105 106 107 108 109 110
             201 202 203 204 205 206 207 208 209 210
             301 302 303 304 305 306 307 308 309 310
             401 402 403 404 405 406 407 408 409 410
             501 502 503 504 505 506 507 508 509 510)
    (phase get-lesson))

(defrule assert-data
    (declare (salience 10000))
    =>
    (load-facts "data.txt"))

(defrule select-best-candidate-lesson-final
    (declare (salience 100))
    ?p <- (phase get-lesson)
    (not (select ?))
    (lesson (ID ?lesson) (state 0) (teacher ?teacher))
    (teacher (ID ?teacher) (weight ?w1))
    (refuse-time (teacher ?teacher) (time $?r-t1))
    (not (and 
            (lesson (state 0) (teacher ?other-teacher&~?teacher))
            (teacher (ID ?other-teacher) (weight ?w2))
            (refuse-time (teacher ?other-teacher) (time $?r-t2))
            (test (> (- (* (length$ ?r-t2) 10) (*(* ?w2 ?w2) ?w2)) 
                     (- (* (length$ ?r-t1) 10) (*(* ?w1 ?w1) ?w1)) 
                  ))
         ))
    =>
    (retract ?p)
    (assert (select ?lesson))
    (assert (phase schedule-3))
    (bind ?priority (- (* (length$ ?r-t1) 10) (* ?w1 ?w1)))
    (printout t "Strategy: Select the teacher with the highest overall priority " ?teacher " (lesson " ?lesson ", score " ?priority ")" crlf))

(defrule schedule-3-favorite-time
    (declare (salience 100))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (= (div ?t1 100) (div ?t3 100)))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room $? ?classroom $?)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 3)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
    (printout t "lesson " ?select " scheduled3個preffered-time: " ?t1 " " ?t2 " " ?t3 crlf))

(defrule schedule-2-favorite-consecutive
    (declare (salience 90))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (= (+ (if (member$ ?t1 $?fav-time) then 1 else 0) (if (member$ ?t2 $?fav-time) then 1 else 0) (if (member$ ?t3 $?fav-time) then 1 else 0)) 2))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (= (div ?t1 100) (div ?t3 100)))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room $? ?classroom $?)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 2)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
    (printout t "lesson " ?select " scheduled 2 preffered-time: " ?t1 " " ?t2 " " ?t3 crlf)
)

(defrule schedule-1-favorite
    (declare (salience 80))
     ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (= (+ (if (member$ ?t1 $?fav-time) then 1 else 0) (if (member$ ?t2 $?fav-time) then 1 else 0) (if (member$ ?t3 $?fav-time) then 1 else 0)) 1))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (= (div ?t1 100) (div ?t3 100)))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room $? ?classroom $?)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 1)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
    (printout t "lesson " ?select " scheduled 1 preffered-time: " ?t1 " " ?t2 " " ?t3 crlf)
)

(defrule schedule-0-favorite-safe
    (declare (salience 70))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (= (+ (if (member$ ?t1 $?fav-time) then 1 else 0) (if (member$ ?t2 $?fav-time) then 1 else 0) (if (member$ ?t3 $?fav-time) then 1 else 0)) 0))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (= (div ?t1 100) (div ?t3 100)))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room $? ?classroom $?)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight ?weight))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
    (printout t "lesson " ?select " scheduled 0 preffered-time: " ?t1 " " ?t2 " " ?t3 crlf)
)

(defrule schedule-3-any-time
    (declare (salience 60))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (= (div ?t1 100) (div ?t3 100)))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room $? ?classroom $?)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 5)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
    (printout t "lesson " ?select " emergency schedule: " ?t1 " " ?t2 " " ?t3 crlf)
)

(defrule handle-unschedulable-lesson
    (declare (salience 50))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (printout t "WARNING: Lesson " ?select " could not be scheduled in this cycle." crlf)
)

(defrule start-optimization-phase
    (declare (salience 40))
    (not (lesson (state 0)))
    (not (phase optimize))
    =>
    (assert (phase optimize))
    (assert (exchange-round 0))
    (printout t "=== auto lesson rearrange ===" crlf)
    
    (bind ?current-favorites 0)
    (bind ?current-violations 0)
    (do-for-all-facts ((?l lesson)) (= ?l:state 1)
        (bind ?teacher ?l:teacher)
        (do-for-fact ((?f favorite-time)) (= ?f:teacher ?teacher)
            (if ?l:time then
                (foreach ?time-slot ?l:time
                    (if (member$ ?time-slot ?f:time) then
                        (bind ?current-favorites (+ ?current-favorites 1))))))
        (do-for-fact ((?r refuse-time)) (= ?r:teacher ?teacher)
            (if ?l:time then
                (foreach ?time-slot ?l:time
                    (if (member$ ?time-slot ?r:time) then
                        (bind ?current-violations (+ ?current-violations 1)))))))
    
    (bind ?baseline-score (- (* 2 ?current-favorites) (* 10 ?current-violations)))
    (printout t "initial score: " ?baseline-score " (favorite:" ?current-favorites " Violation:" ?current-violations ")" crlf))

(defrule start-new-optimization-round
    (declare (salience 35))
    (phase optimize)
    ?round <- (exchange-round ?r&:(< ?r 10))
    (not (best-move))
    =>
    (retract ?round)
    (assert (exchange-round (+ ?r 1)))
    (assert (best-move (type none)))
    (printout t "=== The " (+ ?r 1) " optimization started ===" crlf))

(defrule evaluate-swap-lessons
    (declare (salience 30))
    (phase optimize)
    ?best <- (best-move (improvement ?current-improvement))
    
    ?l1 <- (lesson (ID ?id1) (teacher ?t1) (class ?c1) (type ?type) (time ?t11 ?t12 ?t13))
    ?l2 <- (lesson (ID ?id2&:(> ?id2 ?id1)) (teacher ?t2&~?t1) (class ?c2) (type ?type) (time ?t21 ?t22 ?t23))

    (not (lesson (teacher ?t1) (time $? ?t21|?t22|?t23 $?) (ID ~?id1)))
    (not (lesson (teacher ?t2) (time $? ?t11|?t12|?t13 $?) (ID ~?id2)))
    (not (lesson (class ?c1) (time $? ?t21|?t22|?t23 $?) (ID ~?id1)))
    (not (lesson (class ?c2) (time $? ?t11|?t12|?t13 $?) (ID ~?id2)))

    (favorite-time (teacher ?t1) (time $?fav1))
    (refuse-time (teacher ?t1) (time $?ref1))
    (favorite-time (teacher ?t2) (time $?fav2))
    (refuse-time (teacher ?t2) (time $?ref2))
    =>
    (bind ?fav-t1-old (+ (if (member$ ?t11 $?fav1) then 1 else 0) (if (member$ ?t12 $?fav1) then 1 else 0) (if (member$ ?t13 $?fav1) then 1 else 0)))
    (bind ?fav-t1-new (+ (if (member$ ?t21 $?fav1) then 1 else 0) (if (member$ ?t22 $?fav1) then 1 else 0) (if (member$ ?t23 $?fav1) then 1 else 0)))
    (bind ?fav-t2-old (+ (if (member$ ?t21 $?fav2) then 1 else 0) (if (member$ ?t22 $?fav2) then 1 else 0) (if (member$ ?t23 $?fav2) then 1 else 0)))
    (bind ?fav-t2-new (+ (if (member$ ?t11 $?fav2) then 1 else 0) (if (member$ ?t12 $?fav2) then 1 else 0) (if (member$ ?t13 $?fav2) then 1 else 0)))
    (bind ?fav-improvement (* 2 (+ (- ?fav-t1-new ?fav-t1-old) (- ?fav-t2-new ?fav-t2-old))))

    (bind ?ref-t1-old (+ (if (member$ ?t11 $?ref1) then 1 else 0) (if (member$ ?t12 $?ref1) then 1 else 0) (if (member$ ?t13 $?ref1) then 1 else 0)))
    (bind ?ref-t1-new (+ (if (member$ ?t21 $?ref1) then 1 else 0) (if (member$ ?t22 $?ref1) then 1 else 0) (if (member$ ?t23 $?ref1) then 1 else 0)))
    (bind ?ref-t2-old (+ (if (member$ ?t21 $?ref2) then 1 else 0) (if (member$ ?t22 $?ref2) then 1 else 0) (if (member$ ?t23 $?ref2) then 1 else 0)))
    (bind ?ref-t2-new (+ (if (member$ ?t11 $?ref2) then 1 else 0) (if (member$ ?t12 $?ref2) then 1 else 0) (if (member$ ?t13 $?ref2) then 1 else 0)))
    (bind ?ref-improvement (* -10 (+ (- ?ref-t1-new ?ref-t1-old) (- ?ref-t2-new ?ref-t2-old))))
    
    (bind ?total-improvement (+ ?fav-improvement ?ref-improvement))
    (if (> ?total-improvement ?current-improvement) then
        (modify ?best (type swap) (improvement ?total-improvement) (id1 ?id1) (id2 ?id2) (new_time) (new_room))
        (printout t "better solution: " ?id1 " <-> " ?id2 " (score improvement +" ?total-improvement ")" crlf)))

(defrule evaluate-move-to-empty-slot
    (declare (salience 29))
    (phase optimize)
    ?best <- (best-move (improvement ?current-improvement))

    ?l1 <- (lesson (ID ?id1) (teacher ?t1) (class ?c1) (type ?type) (time $?old_time))
    (refuse-time (teacher ?t1) (time $?ref1))
    (test (some-member$ ?old_time $?ref1))

    (alltime $? ?nt1 ?nt2 ?nt3&:(= (+ ?nt1 2) ?nt3) $?)
    (test (= (div ?nt1 100) (div ?nt3 100)))
    (classroom (ID ?new_room_id) (type ?type))
    (test (not (or (member$ ?nt1 $?ref1) (member$ ?nt2 $?ref1) (member$ ?nt3 $?ref1))))

    (not (lesson (teacher ?t1) (time $? ?nt1|?nt2|?nt3 $?)))
    (not (lesson (class ?c1) (time $? ?nt1|?nt2|?nt3 $?)))
    (not (lesson (time $? ?nt1|?nt2|?nt3 $?) (room $? ?new_room_id $?)))

    (favorite-time (teacher ?t1) (time $?fav1))
    =>
    (bind ?old_fav_count 0)
    (foreach ?slot ?old_time (if (member$ ?slot $?fav1) then (bind ?old_fav_count (+ ?old_fav_count 1))))
    (bind ?new_fav_count (+ (if (member$ ?nt1 $?fav1) then 1 else 0) (if (member$ ?nt2 $?fav1) then 1 else 0) (if (member$ ?nt3 $?fav1) then 1 else 0)))
    (bind ?fav-improvement (* 2 (- ?new_fav_count ?old_fav_count)))

    (bind ?old_ref_count 0)
    (foreach ?slot ?old_time (if (member$ ?slot $?ref1) then (bind ?old_ref_count (+ ?old_ref_count 1))))
    (bind ?ref-improvement (* -10 (- 0 ?old_ref_count)))

    (bind ?total-improvement (+ ?fav-improvement ?ref-improvement))
    (if (> ?total-improvement ?current-improvement) then
        (modify ?best (type move) (improvement ?total-improvement) (id1 ?id1) (id2 0) (new_time ?nt1 ?nt2 ?nt3) (new_room ?new_room_id ?new_room_id ?new_room_id))
        (printout t "better move: " ?id1 " -> " ?nt1 "," ?nt2 "," ?nt3 " (score improvement+" ?total-improvement ")" crlf)))
;; (defrule execute-best-move
;;     (declare (salience 28))
;;     (phase optimize)
;;     (exchange-round ?r)
;;     ?best <- (best-move (type ?type&:(neq ?type none))
;;                          (improvement ?imp&:(> ?imp 0))
;;                          (id1 ?id1)
;;                          (id2 ?id2)
;;                          (new_time $?new_time)
;;                          (new_room $?new_room))
;;     =>
;;     (if (eq ?type swap) then
;;         ; --- SWAP LOGIC ---
;;         (bind ?l1 (nth$ 1 (find-fact ((?l lesson)) (eq (fact-slot-value ?l ID) ?id1))))
;;         (bind ?l2 (nth$ 1 (find-fact ((?l lesson)) (eq (fact-slot-value ?l ID) ?id2))))
;;         ; Get values from facts before modifying
;;         (bind ?time1 (fact-slot-value ?l1 time))
;;         (bind ?room1 (fact-slot-value ?l1 room))
;;         (bind ?time2 (fact-slot-value ?l2 time))
;;         (bind ?room2 (fact-slot-value ?l2 room))
;;         ; Perform modifications
;;         (modify ?l1 (time ?time2) (room ?room2))
;;         (modify ?l2 (time ?time1) (room ?room1))
;;         (printout t "第 " ?r " 輪: 執行交換 " ?id1 " <-> " ?id2 " (分數改善+" ?imp ")" crlf)
;;     else if (eq ?type move) then
;;         ; --- MOVE LOGIC ---
;;         (bind ?l1 (nth$ 1 (find-fact ((?l lesson)) (eq (fact-slot-value ?l ID) ?id1))))
;;         (modify ?l1 (time ?new_time) (room ?new_room))
;;         (printout t "第 " ?r " 輪: 執行移動 " ?id1 " -> " (nth$ 1 ?new_time) " (分數改善+" ?imp ")" crlf)
;;     )
;;     (retract ?best))

(defrule no-improvement-found-and-finish
    (declare (salience 27))
    (phase optimize)
    ?round <- (exchange-round ?r&:(and (> ?r 0) (<= ?r 10)))
    ?best <- (best-move (improvement ?imp&:(<= ?imp 0)))
    =>
    (retract ?best ?round)
    (assert (exchange-round 11))
    (printout t ?r " round: end optimization for no improvement spaces" crlf))

(defrule finish-optimization-phase
    (declare (salience 20))
    (phase optimize)
    (exchange-round ?r&:(>= ?r 11))
    =>
    (assert (phase output)))

(defrule output-final-result
    (declare (salience -1000))
    (phase output)
    =>
    (bind ?final-favorites 0)
    (bind ?final-violations 0)
    (do-for-all-facts ((?l lesson)) (= ?l:state 1)
        (bind ?teacher ?l:teacher)
        (do-for-fact ((?f favorite-time)) (= ?f:teacher ?teacher)
            (if ?l:time then
                (foreach ?time-slot ?l:time
                    (if (member$ ?time-slot ?f:time) then
                        (bind ?final-favorites (+ ?final-favorites 1))))))
        (do-for-fact ((?r refuse-time)) (= ?r:teacher ?teacher)
            (if ?l:time then
                (foreach ?time-slot ?l:time
                    (if (member$ ?time-slot ?r:time) then
                        (bind ?final-violations (+ ?final-violations 1)))))))
    
    (bind ?final-score (- (* 2 ?final-favorites) (* 10 ?final-violations)))
    
    (printout t "=== result ===" crlf)
    (printout t "preffered-time total: " ?final-favorites crlf)
    (printout t "violations: " ?final-violations crlf)
    (printout t "predict scores: " ?final-score crlf)
    
    (open "result.txt" out "w")
    (do-for-all-facts ((?l lesson)) (= ?l:state 1)
        (printout out "(lesson (ID " ?l:ID ") (state 1) (class " ?l:class ") (teacher " ?l:teacher ") (type " ?l:type ") (time "
                       (nth$ 1 ?l:time) " " (nth$ 2 ?l:time) " " (nth$ 3 ?l:time) ") (room "
                       (nth$ 1 ?l:room) " " (nth$ 2 ?l:room) " " (nth$ 3 ?l:room) "))" crlf))
    (close out)
    (printout t "complete and save to \"result.txt\"" crlf))