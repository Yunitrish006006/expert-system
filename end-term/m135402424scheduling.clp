(deftemplate teacher (slot ID) (slot weight))
(deftemplate class (slot ID))
(deftemplate classroom (slot ID) (slot type))
(deftemplate lesson (slot ID) (slot state) (slot class) (slot teacher) (slot type) (multislot time) (multislot room)) 
(deftemplate favorite-time (slot teacher) (multislot time))
(deftemplate refuse-time (slot teacher) (multislot time))

(deffacts initial
    (alltime 
        101 102 103 104 105 106 107 108 109 110 
        201 202 203 204 205 206 207 208 209 210
        301 302 303 304 305 306 307 308 309 310
        401 402 403 404 405 406 407 408 409 410
        501 502 503 504 505 506 507 508 509 510)
    (phase load-data)
    (scheduling-attempt-count 0)  ; 添加排課嘗試計數器
)

(defrule load-data
    ?p <- (phase load-data)
    =>
    (retract ?p)
    (load-facts "data.txt")
    (assert (phase get-lesson))
    (printout t "Data loaded successfully" crlf)
)

(defrule select-lesson
    (declare (salience 100))
    ?p <- (phase get-lesson)
    ?count <- (scheduling-attempt-count ?attempts&:(< ?attempts 1000))  ; 限制最大嘗試次數
	(not (select ?))
	(teacher (ID ?teacher) (weight ?weight))
	(lesson (ID ?lesson) (state 0) (teacher ?teacher))  ; 只選擇 state 為 0 的課程（未排課且可排課）
	(not(and (teacher (ID ?else) (weight ?w2&:(< ?w2 ?weight)))
        (lesson (state 0) (teacher ?else))))
	=>
    (retract ?p ?count)
    (assert (scheduling-attempt-count (+ ?attempts 1)))
	(assert (select ?lesson))
    (assert (phase schedule-3))
    (printout t "Start scheduled lesson: " ?lesson " (Attempt: " (+ ?attempts 1) ")" crlf)
)

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
    (printout t "Lesson " ?select " setting into continuous 3 lessons: " ?t1 " " ?t2 " " ?t3 crlf)
)

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
    (printout t "Lesson " ?select " setting into continuous 2 lessons: " ?t1 " " ?t2 " " ?t3 crlf))

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
    (printout t "Lesson " ?select " setting into continuous 1 lessons: " ?t1 " " ?t2 " " ?t3 crlf))

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
    (printout t "Lesson " ?select " setting into continuous 0 lessons: " ?t1 " " ?t2 " " ?t3 crlf))

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
    (printout t "Lesson " ?select " emergency plan: " ?t1 " " ?t2 " " ?t3 crlf)
)

(defrule handle-unschedulable-lesson
    (declare (salience 50))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0))
    =>
    (retract ?p ?f1)
    (modify ?f2 (state -1))  ; 標記為無法排課
    (assert (phase get-lesson))
    (printout t "WARNING: Lesson " ?select " could not be scheduled and marked as unschedulable." crlf)
)


(defrule check-done
    ?p <- (phase get-lesson)
    (not (lesson (state 0)))  ; 檢查是否還有未排課且可排課的課程
    =>
    (retract ?p)
    (assert (phase save-results))
    (printout t "All schedulable lessons have been processed" crlf)
    ; 顯示無法排課的課程統計
    (if (> (length$ (find-all-facts ((?l lesson)) (= ?l:state -1))) 0) then
        (printout t "WARNING: Some lessons could not be scheduled:" crlf)
        (do-for-all-facts ((?l lesson)) (= ?l:state -1)
            (printout t "  Lesson " ?l:ID " (Teacher: " ?l:teacher ", Class: " ?l:class ")" crlf)))
)

; 添加安全規則：如果嘗試次數過多，強制結束
(defrule emergency-stop
    (declare (salience 200))
    ?p <- (phase get-lesson)
    ?count <- (scheduling-attempt-count ?attempts&:(>= ?attempts 1000))
    =>
    (retract ?p ?count)
    (assert (phase save-results))
    (printout t "EMERGENCY STOP: Too many scheduling attempts (" ?attempts "). Forcing termination." crlf)
    (printout t "This may indicate an infinite loop or overly complex scheduling constraints." crlf)
)

(defrule save-results
    ?p <- (phase save-results)
    =>
    (retract ?p)
    (save-facts "result.txt" local lesson)
    (printout t "Results saved to result.txt" crlf)
    (printout t "Scheduling complete! Run scoring.clp to evaluate the solution." crlf)
)