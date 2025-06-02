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
    (alltime 101 102 103 104 105 106 107 108 109 11077
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
    (test (= (div ?t1 100) (div ?t3 100))) ; 確保同一天
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 3))) ; 三節喜好時段，權重加 3
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

;; 非連續時段規則 - 嘗試安排非連續的三節課程
;; 當所有連續排課規則都失敗時，嘗試找出三個同日不連續的時段
(defrule schedule-3-non-consecutive
    (declare (salience 65)) ; 優先級略高於標記失敗的規則
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    ;; 從 alltime 找出三個時段，不要求連續，但要求在同一天
    (alltime $? ?t1 $?)
    (alltime $? ?t2&:(and (neq ?t1 ?t2) (= (div ?t1 100) (div ?t2 100))) $?)
    (alltime $? ?t3&:(and (neq ?t1 ?t3) (neq ?t2 ?t3) (= (div ?t1 100) (div ?t3 100))) $?)
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (refuse-time (teacher ?teacher) (time $?r-t))
    ;; 確保時段不被拒絕
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (classroom (ID ?classroom) (type ?type))
    ;; 確保沒有時間衝突
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (room $? ?classroom $?) (time $? ?t1 $?)))
    (not (lesson (room $? ?classroom $?) (time $? ?t2 $?)))
    (not (lesson (room $? ?classroom $?) (time $? ?t3 $?)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    ;; 計算權重加分：檢查有幾個喜好時段
    (bind ?bonus 0)
    (if (member$ ?t1 $?fav-time) then (bind ?bonus (+ ?bonus 1)))
    (if (member$ ?t2 $?fav-time) then (bind ?bonus (+ ?bonus 1)))
    (if (member$ ?t3 $?fav-time) then (bind ?bonus (+ ?bonus 1)))
    (modify ?f3 (weight (+ ?weight ?bonus)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
    (printout t "RESCUED: Scheduled lesson " ?select " in non-consecutive time slots " ?t1 " " ?t2 " " ?t3 " for teacher " ?teacher crlf))
    
;; 跨日期非連續時段排課規則
;; 當所有同日排課規則都失敗時，嘗試找出跨天的時段
(defrule schedule-3-multi-day
    (declare (salience 63)) ; 優先級略低於同日非連續規則，但高於標記失敗的規則
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    ;; 從 alltime 找出三個時段，可以跨天
    (alltime $? ?t1 $?)
    (alltime $? ?t2&:(neq ?t1 ?t2) $?)
    (alltime $? ?t3&:(and (neq ?t1 ?t3) (neq ?t2 ?t3)) $?)
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (refuse-time (teacher ?teacher) (time $?r-t))
    ;; 確保時段不被拒絕
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (classroom (ID ?classroom) (type ?type))
    ;; 確保沒有時間衝突
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (room $? ?classroom $?) (time $? ?t1 $?)))
    (not (lesson (room $? ?classroom $?) (time $? ?t2 $?)))
    (not (lesson (room $? ?classroom $?) (time $? ?t3 $?)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    ;; 計算權重加分：檢查有幾個喜好時段
    (bind ?bonus 0)
    (if (member$ ?t1 $?fav-time) then (bind ?bonus (+ ?bonus 1)))
    (if (member$ ?t2 $?fav-time) then (bind ?bonus (+ ?bonus 1)))
    (if (member$ ?t3 $?fav-time) then (bind ?bonus (+ ?bonus 1)))
    ;; 權重略低一點，因為跨天不太理想
    (modify ?f3 (weight (+ ?weight (- ?bonus 0.5))))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
    (printout t "LAST RESORT: Scheduled lesson " ?select " across different days " ?t1 " " ?t2 " " ?t3 " for teacher " ?teacher crlf))

;; 最後機會排課規則 - 採用更靈活的方式來搜尋可用時段
;; 這是最後一次嘗試在標記課程無法排課之前
(defrule schedule-3-last-chance
    (declare (salience 61)) ; 優先級比跨天時段規則低一點，但仍然高於標記失敗的規則
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    ;; 不限制時段模式，從所有時段中尋找可用時段
    (alltime $? ?t1 $?)
    (alltime $? ?t2 $?)
    (alltime $? ?t3 $?)
    ;; 允許重複時段選擇（例如周一和周二的相同時段）
    (test (or (neq ?t1 ?t2) (neq ?t2 ?t3) (neq ?t1 ?t3))) ; 至少有兩個不同的時段
    (refuse-time (teacher ?teacher) (time $?r-t))
    ;; 確保時段不被拒絕
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (classroom (ID ?classroom) (type ?type))
    ;; 確保沒有時間衝突
    (not (lesson (teacher ?teacher) (time $? ?t1 $?)))
    (not (lesson (teacher ?teacher) (time $? ?t2 $?)))
    (not (lesson (teacher ?teacher) (time $? ?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1 $?)))
    (not (lesson (class ?class) (time $? ?t2 $?)))
    (not (lesson (class ?class) (time $? ?t3 $?)))
    (not (lesson (room $? ?classroom $?) (time $? ?t1 $?)))
    (not (lesson (room $? ?classroom $?) (time $? ?t2 $?)))
    (not (lesson (room $? ?classroom $?) (time $? ?t3 $?)))
    (favorite-time (teacher ?teacher) (time $?fav-time))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    ;; 計算權重加分：檢查有幾個喜好時段
    (bind ?bonus 0)
    (if (member$ ?t1 $?fav-time) then (bind ?bonus (+ ?bonus 1)))
    (if (member$ ?t2 $?fav-time) then (bind ?bonus (+ ?bonus 1)))
    (if (member$ ?t3 $?fav-time) then (bind ?bonus (+ ?bonus 1)))
    ;; 權重調整為較低，因為這是最後機會
    (modify ?f3 (weight (+ ?weight (- ?bonus 1.0))))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
    (printout t "EMERGENCY: Last chance scheduling for lesson " ?select " with time slots " ?t1 " " ?t2 " " ?t3 " for teacher " ?teacher crlf))

;; 極端情況排課規則 - 最後嘗試，在宣告失敗前使用任何可能的時段和教室組合
(defrule schedule-3-extreme-fallback
    (declare (salience 59)) ; 比標記失敗規則略低優先級，但仍然高於最終失敗
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    ;; 嘗試找出任何三個可能的時段，甚至可以包括非喜好且接近拒絕的時段
    (alltime $? ?t1 $?)
    (alltime $? ?t2 $?)
    (alltime $? ?t3 $?)
    (refuse-time (teacher ?teacher) (time $?r-t))
    ;; 唯一的硬性要求是不在拒絕時段
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))    ;; 試圖找到任何可用的教室，無論類型是否匹配
    (classroom (ID ?classroom) (type ?classroom-type))
    ;; 確保沒有時間衝突
    (not (lesson (teacher ?teacher) (time $? ?t1 $?)))
    (not (lesson (teacher ?teacher) (time $? ?t2 $?)))
    (not (lesson (teacher ?teacher) (time $? ?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1 $?)))
    (not (lesson (class ?class) (time $? ?t2 $?)))
    (not (lesson (class ?class) (time $? ?t3 $?)))
    (not (lesson (room $? ?classroom $?) (time $? ?t1 $?)))
    (not (lesson (room $? ?classroom $?) (time $? ?t2 $?)))
    (not (lesson (room $? ?classroom $?) (time $? ?t3 $?)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    ;; 由於這是最後的嘗試，權重不變
    (modify ?f3 (weight ?weight))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
    (printout t "EXTREME FALLBACK: Scheduled lesson " ?select " with ANY possible time slots " ?t1 " " ?t2 " " ?t3 " in classroom " ?classroom " for teacher " ?teacher crlf))

;; 替換您原有的 mark-unschedulable 規則
;; 這個規則會在所有更高優先級的排課規則 (100-58) 都失敗後執行
(defrule decide-lesson-fate-after-attempts
    (declare (salience 58)) ; 確保在所有實際排課嘗試之後
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (teacher ?teacher)) ; 關鍵：課程仍然是 state 0
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    ; 在這裡，您可以選擇是直接標記為 state 2，還是暫時跳過（使其保持 state 0，之後可能還有機會）
    ; 根據您的輸出邏輯 (not (lesson (state 0)))，標記為 state 2 是讓流程結束的方式之一
    (modify ?f2 (state 2)) ; 標記為無法排課
    (modify ?f3 (weight (+ ?weight 10))) ; 懲罰權重
    (printout t "FINAL FAILURE: Lesson " ?select " for teacher " ?teacher " could not be scheduled by ANY rule and is marked state 2." crlf)
)

;; 輸出結果到 result.txt
(defrule output-result
    (declare (salience -1000))
    (not (lesson (state 0))) ; 所有課程都已處理（state 1 或 state 2）
    =>
    (open "result.txt" out "w")
    (do-for-all-facts ((?l lesson)) (= ?l:state 1)
        (printout out "(lesson (ID " ?l:ID ") (state 1) (class " ?l:class ") (teacher " ?l:teacher ") (type " ?l:type ") (time "
                      (nth$ 1 ?l:time) " " (nth$ 2 ?l:time) " " (nth$ 3 ?l:time) ") (room "
                      (nth$ 1 ?l:room) " " (nth$ 2 ?l:room) " " (nth$ 3 ?l:room) "))" crlf))
    (do-for-all-facts ((?l lesson)) (= ?l:state 2)
        (printout out "; Lesson " ?l:ID " could not be scheduled." crlf))
    (close out)
    (halt))