;; 模板定義
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

;; 初始事實
(deffacts initial
    (alltime 101 102 103 104 105 106 107 108 109 110
             201 202 203 204 205 206 207 208 209 210
             301 302 303 304 305 306 307 308 309 310
             401 402 403 404 405 406 407 408 409 410
             501 502 503 504 505 506 507 508 509 510)
    (phase get-lesson))

;; 讀取 data.txt
(defrule assert-data
    (declare (salience 10000))
    =>
    (load-facts "data.txt"))

;; 選擇未排課的課程（優先選擇權重最低的教師）
(defrule select-lesson
    ?p <- (phase get-lesson)
    (not (select ?))
    (teacher (ID ?teacher) (weight ?weight))
    (lesson (ID ?lesson) (state 0) (teacher ?teacher))
    (not (and (teacher (ID ?else) (weight ?w2&:(< ?w2 ?weight)))
              (lesson (state 0) (teacher ?else))))
    =>
    (retract ?p)
    (assert (select ?lesson))
    (assert (phase schedule-3)))

;; 三節喜好時段
(defrule schedule-3-favorite-time
    (declare (salience 100))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (= (div ?t1 100) (div ?t3 100)))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 3)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom)))

;; 兩節喜好時段 + 一節非喜好時段（情況 1：連續兩節喜好 + 一節非喜好）
(defrule schedule-3-two-favorite-first
    (declare (salience 90))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (member$ ?t1 $?fav-time))
    (test (member$ ?t2 $?fav-time))
    (test (not (member$ ?t3 $?fav-time)))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (= (div ?t1 100) (div ?t3 100)))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 2)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom)))

;; 兩節喜好時段 + 一節非喜好時段（情況 2：一節非喜好 + 連續兩節喜好）
(defrule schedule-3-two-favorite-last
    (declare (salience 90))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (not (member$ ?t1 $?fav-time)))
    (test (member$ ?t2 $?fav-time))
    (test (member$ ?t3 $?fav-time))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (= (div ?t1 100) (div ?t3 100)))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 2)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom)))

;; 兩節喜好時段 + 一節非喜好時段（情況 3：喜好 + 非喜好 + 喜好）
(defrule schedule-3-two-favorite-split
    (declare (salience 90))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (member$ ?t1 $?fav-time))
    (test (not (member$ ?t2 $?fav-time)))
    (test (member$ ?t3 $?fav-time))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (= (div ?t1 100) (div ?t3 100)))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 2)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom)))

;; 一節喜好時段 + 兩節非喜好時段（情況 1：連續兩節非喜好 + 一節喜好）
(defrule schedule-3-one-favorite-last
    (declare (salience 80))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (not (member$ ?t1 $?fav-time)))
    (test (not (member$ ?t2 $?fav-time)))
    (test (member$ ?t3 $?fav-time))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (= (div ?t1 100) (div ?t3 100)))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 1)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom)))

;; 一節喜好時段 + 兩節非喜好時段（情況 2：一節喜好 + 連續兩節非喜好）
(defrule schedule-3-one-favorite-first
    (declare (salience 80))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (member$ ?t1 $?fav-time))
    (test (not (member$ ?t2 $?fav-time)))
    (test (not (member$ ?t3 $?fav-time)))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (= (div ?t1 100) (div ?t3 100)))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 1)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom)))

;; 一節喜好時段 + 兩節非喜好時段（情況 3：非喜好 + 喜好 + 非喜好）
(defrule schedule-3-one-favorite-middle
    (declare (salience 80))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (not (member$ ?t1 $?fav-time)))
    (test (member$ ?t2 $?fav-time))
    (test (not (member$ ?t3 $?fav-time)))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (= (div ?t1 100) (div ?t3 100)))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 1)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom)))

;; 三節非喜好時段
(defrule schedule-3-non-favorite
    (declare (salience 70))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (not (member$ ?t1 $?fav-time)))
    (test (not (member$ ?t2 $?fav-time)))
    (test (not (member$ ?t3 $?fav-time)))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (= (div ?t1 100) (div ?t3 100)))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight ?weight))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom)))

;; 最後嘗試任何可用時段（包括拒絕時段）
(defrule schedule-3-any-time
    (declare (salience 60))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (= (div ?t1 100) (div ?t3 100)))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 5)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom)))

;; 無法排課時，跳過並增加權重
(defrule decide-lesson-fate-after-attempts
    (declare (salience 50))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (teacher ?teacher))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 10)))
    (printout t "WARNING: Lesson " ?select " for teacher " ?teacher " could not be scheduled by any rule and is skipped." crlf))

;; 輸出結果到 result.txt
(defrule output-result
    (declare (salience -1000))
    (not (lesson (state 0)))
    =>
    (open "result.txt" out "w")
    (do-for-all-facts ((?l lesson)) (= ?l:state 1)
        (printout out "(lesson (ID " ?l:ID ") (state 1) (class " ?l:class ") (teacher " ?l:teacher ") (type " ?l:type ") (time "
                      (nth$ 1 ?l:time) " " (nth$ 2 ?l:time) " " (nth$ 3 ?l:time) ") (room "
                      (nth$ 1 ?l:room) " " (nth$ 2 ?l:room) " " (nth$ 3 ?l:room) "))" crlf))
    (close out)
    (halt))