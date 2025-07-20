(deftemplate teacher (slot ID) (slot weight))
(deftemplate class (slot ID))
(deftemplate classroom (slot ID) (slot type))
(deftemplate lesson (slot ID) (slot state) (slot class) (slot teacher) (slot type) (multislot time) (multislot room)) 
(deftemplate favorite-time (slot teacher) (multislot time))
(deftemplate refuse-time (slot teacher) (multislot time))

; 添加優化相關的模板
(deftemplate best-move
    (slot type (type SYMBOL) (default none))
    (slot improvement (type NUMBER) (default 0))
    (slot id1 (type INTEGER) (default 0))
    (slot id2 (type INTEGER) (default 0))
    (multislot new_time)
    (multislot new_room)
)

; 防止重複評估相同交換方案的計數器
(deftemplate evaluation-counter 
    (slot pair-key)
    (slot count (default 0))
)

; 優化的輔助函數
(deffunction calculate-priority (?weight ?refuse-count)
  "計算老師的優先級：考慮權重和拒絕時間段數量"
  (return (- (* ?refuse-count 15) (* ?weight ?weight ?weight)))
)

(deffunction count-favorites (?time-slots ?favorite-list)
  "計算時間段中有多少個是偏好時間"
  (bind ?count 0)
  (foreach ?slot ?time-slots
    (if (member$ ?slot ?favorite-list) then
      (bind ?count (+ ?count 1))))
  (return ?count)
)

(deffunction count-violations (?time-slots ?refuse-list)
  "計算時間段中有多少個是拒絕時間"
  (bind ?count 0)
  (foreach ?slot ?time-slots
    (if (member$ ?slot ?refuse-list) then
      (bind ?count (+ ?count 1))))
  (return ?count)
)

(deffunction is-same-day (?t1 ?t2)
  "檢查兩個時間是否在同一天"
  (return (= (div ?t1 100) (div ?t2 100)))
)

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
	(lesson (ID ?lesson) (state 0) (teacher ?teacher))
	(teacher (ID ?teacher) (weight ?w1))
	(refuse-time (teacher ?teacher) (time $?r-t1))
	(not (and 
            (lesson (state 0) (teacher ?other-teacher&~?teacher))
            (teacher (ID ?other-teacher) (weight ?w2))
            (refuse-time (teacher ?other-teacher) (time $?r-t2))
            (test (> (calculate-priority ?w2 (length$ ?r-t2))
                     (calculate-priority ?w1 (length$ ?r-t1))))
         ))
	=>
    (retract ?p ?count)
    (assert (scheduling-attempt-count (+ ?attempts 1)))
	(assert (select ?lesson))
    (assert (phase schedule-3))
    (bind ?priority (calculate-priority ?w1 (length$ ?r-t1)))
    (printout t "Start scheduled lesson: " ?lesson " (Teacher: " ?teacher ", Priority: " ?priority ", Attempt: " (+ ?attempts 1) ")" crlf)
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
    (test (is-same-day ?t1 ?t3))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room $? ?classroom $?)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 4))) ; 增加獎勵
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
    (printout t "Lesson " ?select " scheduled in 3 consecutive favorite times: " ?t1 " " ?t2 " " ?t3 crlf)
)

(defrule schedule-2-favorite-consecutive
    (declare (salience 90))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (= (count-favorites (create$ ?t1 ?t2 ?t3) ?fav-time) 2))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (is-same-day ?t1 ?t3))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room $? ?classroom $?)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 3)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
    (printout t "Lesson " ?select " scheduled with 2 favorite times: " ?t1 " " ?t2 " " ?t3 crlf))

(defrule schedule-1-favorite
    (declare (salience 80))
     ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (= (count-favorites (create$ ?t1 ?t2 ?t3) ?fav-time) 1))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (is-same-day ?t1 ?t3))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room $? ?classroom $?)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 1)))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
    (printout t "Lesson " ?select " scheduled with 1 favorite time: " ?t1 " " ?t2 " " ?t3 crlf))

(defrule schedule-0-favorite-safe
    (declare (salience 70))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (favorite-time (teacher ?teacher) (time $?fav-time))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (= (count-favorites (create$ ?t1 ?t2 ?t3) ?fav-time) 0))
    (refuse-time (teacher ?teacher) (time $?r-t))
    (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))
    (test (is-same-day ?t1 ?t3))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room $? ?classroom $?)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight ?weight))
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
    (printout t "Lesson " ?select " scheduled in neutral time: " ?t1 " " ?t2 " " ?t3 crlf))

(defrule schedule-3-any-time
    (declare (salience 60))
    ?p <- (phase schedule-3)
    ?f1 <- (select ?select)
    ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type))
    ?f3 <- (teacher (ID ?teacher) (weight ?weight))
    (alltime $? ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3) $?)
    (test (is-same-day ?t1 ?t3))
    (classroom (ID ?classroom) (type ?type))
    (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
    (not (lesson (time $? ?t1|?t2|?t3 $?) (room $? ?classroom $?)))
    =>
    (retract ?p ?f1)
    (assert (phase get-lesson))
    (modify ?f3 (weight (+ ?weight 8))) ; 增加懲罰
    (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
    (printout t "Lesson " ?select " emergency scheduling: " ?t1 " " ?t2 " " ?t3 crlf)
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
    (assert (phase optimize))
    (assert (exchange-round 0))
    (printout t "All schedulable lessons have been processed" crlf)
    (printout t "=== 開始自動課程重新安排優化 ===" crlf)
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

; 優化輪次開始
(defrule begin-new-optimization-round
    (declare (salience 35))
    (phase optimize)
    ?round <- (exchange-round ?r&:(< ?r 15)) ; 15輪優化
    (not (best-move))
    =>
    (retract ?round)
    ; 清理舊的評估計數器
    (do-for-all-facts ((?ec evaluation-counter)) TRUE
        (retract ?ec))
    (assert (exchange-round (+ ?r 1)))
    (assert (best-move (type none)))
    (printout t "=== 第 " (+ ?r 1) " 輪優化開始 ===" crlf))

; 專門針對違規課程的積極移動規則 - 最高優先級
(defrule eliminate-schedule-violations
    (declare (salience 40)) ; 最高優先級
    (phase optimize)
    ?best <- (best-move (improvement ?current-improvement))
    
    ; 找到任何有違規的課程
    ?l1 <- (lesson (ID ?id1) (teacher ?t1) (class ?c1) (type ?type) (time $?old_time))
    (refuse-time (teacher ?t1) (time $?ref1))
    (test (> (count-violations ?old_time ?ref1) 0)) ; 確保有違規
    
    ; 找到任何可以移動到的時段（不一定是最優的）
    (alltime $? ?nt1 ?nt2 ?nt3&:(= (+ ?nt1 2) ?nt3) $?)
    (test (is-same-day ?nt1 ?nt3))
    (classroom (ID ?new_room_id) (type ?type))
    (test (= (count-violations (create$ ?nt1 ?nt2 ?nt3) ?ref1) 0)) ; 新時段無違規
    
    ; 確保沒有基本衝突
    (not (lesson (teacher ?t1) (time $? ?nt1|?nt2|?nt3 $?)))
    (not (lesson (class ?c1) (time $? ?nt1|?nt2|?nt3 $?)))
    (not (lesson (time $? ?nt1|?nt2|?nt3 $?) (room $? ?new_room_id $?)))
    
    (favorite-time (teacher ?t1) (time $?fav1))
    =>
    (bind ?new-time (create$ ?nt1 ?nt2 ?nt3))
    (bind ?old_violations (count-violations ?old_time ?ref1))
    
    ; 給消除違規一個巨大的獎勵分數，確保會被執行
    (bind ?violation-bonus (* 100 ?old_violations)) ; 每消除一個違規得100分
    (bind ?fav-change (* 2 (- (count-favorites ?new-time ?fav1) (count-favorites ?old_time ?fav1))))
    (bind ?aggressive-improvement (+ ?violation-bonus ?fav-change))
    
    ; 強制設為最佳方案
    (modify ?best (type move) (improvement ?aggressive-improvement) (id1 ?id1) (id2 0) 
            (new_time ?nt1 ?nt2 ?nt3) (new_room ?new_room_id ?new_room_id ?new_room_id))
    (printout t "🚨消除違規🚨 課程" ?id1 " 老師" ?t1 " 從違規時段移至 " ?nt1 " " ?nt2 " " ?nt3 " (消除" ?old_violations "個違規)" crlf))

; 優化的交換評估規則
(defrule evaluate-lesson-swap-for-improvement
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
    ; 計算交換的分數改善
    (bind ?old-time1 (create$ ?t11 ?t12 ?t13))
    (bind ?new-time1 (create$ ?t21 ?t22 ?t23))
    (bind ?old-time2 (create$ ?t21 ?t22 ?t23))
    (bind ?new-time2 (create$ ?t11 ?t12 ?t13))
    
    (bind ?fav-t1-old (count-favorites ?old-time1 ?fav1))
    (bind ?fav-t1-new (count-favorites ?new-time1 ?fav1))
    (bind ?fav-t2-old (count-favorites ?old-time2 ?fav2))
    (bind ?fav-t2-new (count-favorites ?new-time2 ?fav2))
    (bind ?fav-improvement (* 3 (+ (- ?fav-t1-new ?fav-t1-old) (- ?fav-t2-new ?fav-t2-old))))

    (bind ?ref-t1-old (count-violations ?old-time1 ?ref1))
    (bind ?ref-t1-new (count-violations ?new-time1 ?ref1))
    (bind ?ref-t2-old (count-violations ?old-time2 ?ref2))
    (bind ?ref-t2-new (count-violations ?new-time2 ?ref2))
    (bind ?ref-improvement (* -15 (+ (- ?ref-t1-new ?ref-t1-old) (- ?ref-t2-new ?ref-t2-old))))
    
    (bind ?total-improvement (+ ?fav-improvement ?ref-improvement))
    (if (> ?total-improvement ?current-improvement) then
        (modify ?best (type swap) (improvement ?total-improvement) (id1 ?id1) (id2 ?id2) (new_time) (new_room))
        (printout t "發現更好的交換方案: " ?id1 " <-> " ?id2 " (分數改善 +" ?total-improvement ")" crlf)))

; 優化的移動評估規則
(defrule evaluate-lesson-move-for-improvement
    (declare (salience 29))
    (phase optimize)
    ?best <- (best-move (improvement ?current-improvement))

    ?l1 <- (lesson (ID ?id1) (teacher ?t1) (class ?c1) (type ?type) (time $?old_time))
    (refuse-time (teacher ?t1) (time $?ref1))

    (alltime $? ?nt1 ?nt2 ?nt3&:(= (+ ?nt1 2) ?nt3) $?)
    (test (is-same-day ?nt1 ?nt3))
    (classroom (ID ?new_room_id) (type ?type))

    (not (lesson (teacher ?t1) (time $? ?nt1|?nt2|?nt3 $?)))
    (not (lesson (class ?c1) (time $? ?nt1|?nt2|?nt3 $?)))
    (not (lesson (time $? ?nt1|?nt2|?nt3 $?) (room $? ?new_room_id $?)))

    (favorite-time (teacher ?t1) (time $?fav1))
    =>
    (bind ?new-time (create$ ?nt1 ?nt2 ?nt3))
    (bind ?old_fav_count (count-favorites ?old_time ?fav1))
    (bind ?new_fav_count (count-favorites ?new-time ?fav1))
    (bind ?fav-improvement (* 3 (- ?new_fav_count ?old_fav_count)))

    (bind ?old_ref_count (count-violations ?old_time ?ref1))
    (bind ?new_ref_count (count-violations ?new-time ?ref1))
    (bind ?ref-improvement (* -15 (- ?new_ref_count ?old_ref_count)))

    (bind ?total-improvement (+ ?fav-improvement ?ref-improvement))
    (if (> ?total-improvement ?current-improvement) then
        (modify ?best (type move) (improvement ?total-improvement) (id1 ?id1) (id2 0) (new_time ?nt1 ?nt2 ?nt3) (new_room ?new_room_id ?new_room_id ?new_room_id))
        (printout t "發現更好的移動方案: " ?id1 " -> " ?nt1 "," ?nt2 "," ?nt3 " (分數改善 +" ?total-improvement ")" crlf)))

; 執行最佳移動
(defrule execute-best-optimization-move
    (declare (salience 28))
    (phase optimize)
    (exchange-round ?r)
    ?best <- (best-move (type ?type&:(neq ?type none))
                         (improvement ?imp)
                         (id1 ?id1)
                         (id2 ?id2)
                         (new_time $?new_time)
                         (new_room $?new_room))
    =>
    (if (> ?imp 0) then
        ; 只有真正的改善才執行
        (if (eq ?type swap) then
            (do-for-fact ((?l1 lesson)) (= ?l1:ID ?id1)
                (do-for-fact ((?l2 lesson)) (= ?l2:ID ?id2)
                    (bind ?temp-time ?l1:time)
                    (bind ?temp-room ?l1:room)
                    (modify ?l1 (time ?l2:time) (room ?l2:room))
                    (modify ?l2 (time ?temp-time) (room ?temp-room))))
            (printout t "第 " ?r " 輪: 執行交換 " ?id1 " <-> " ?id2 " (分數改善+" ?imp ")" crlf)
        else
            (if (eq ?type move) then
                (do-for-fact ((?l1 lesson)) (= ?l1:ID ?id1)
                    (modify ?l1 (time ?new_time) (room ?new_room)))
                (printout t "第 " ?r " 輪: 執行移動 " ?id1 " -> " (nth$ 1 ?new_time) " (分數改善+" ?imp ")" crlf)))
    else
        ; 沒有改善的情況下，直接結束這一輪
        (printout t "第 " ?r " 輪: 無有效改善方案 (最佳改善=" ?imp "), 結束本輪" crlf))
    (retract ?best))

; 檢查優化結束條件
(defrule check-optimization-completion
    (declare (salience 27))
    (phase optimize)
    ?round <- (exchange-round ?r&:(and (> ?r 0) (<= ?r 15)))
    ?best <- (best-move (improvement ?imp&:(<= ?imp 0)))
    =>
    ; 統計當前違規數量
    (bind ?current-violations 0)
    (do-for-all-facts ((?l lesson)) (= ?l:state 1)
        (bind ?teacher ?l:teacher)
        (do-for-fact ((?r refuse-time)) (= ?r:teacher ?teacher)
            (if ?l:time then
                (bind ?viol-count (count-violations ?l:time ?r:time))
                (bind ?current-violations (+ ?current-violations ?viol-count)))))
    
    (if (= ?current-violations 0) then
        ; 沒有違規，完美結束
        (retract ?best ?round)
        (assert (exchange-round 16))
        (printout t "🎉第 " ?r " 輪: 已完全消除所有違規時段！完美優化完成！🎉" crlf)
    else
        (if (< ?r 10) then
            ; 前10輪繼續嘗試
            (retract ?best ?round)
            (assert (exchange-round (+ ?r 1)))
            (printout t "第 " ?r " 輪: 仍有 " ?current-violations " 個違規，繼續優化(第" (+ ?r 1) "輪)" crlf)
        else
            ; 10輪後結束
            (retract ?best ?round)
            (assert (exchange-round 16))
            (printout t "第 " ?r " 輪: 仍有 " ?current-violations " 個違規，已達最大嘗試次數" crlf))))

; 達到最大輪次時結束
(defrule terminate-optimization-when-maximum-rounds-reached
    (declare (salience 26))
    (phase optimize)
    ?round <- (exchange-round ?r&:(> ?r 15))
    =>
    (retract ?round)
    (assert (phase save-results))
    (printout t "達到最大優化輪次(15輪)，結束優化" crlf))

; 優化結束後的結果輸出
(defrule generate-final-schedule-report
    (declare (salience 25))
    (phase optimize)
    (exchange-round ?r&:(>= ?r 16))
    =>
    (assert (phase save-results))
    (bind ?final-favorites 0)
    (bind ?final-violations 0)
    (bind ?total-lessons 0)
    
    (do-for-all-facts ((?l lesson)) (= ?l:state 1)
        (bind ?total-lessons (+ ?total-lessons 1))
        (bind ?teacher ?l:teacher)
        (do-for-fact ((?f favorite-time)) (= ?f:teacher ?teacher)
            (if ?l:time then
                (bind ?fav-count (count-favorites ?l:time ?f:time))
                (bind ?final-favorites (+ ?final-favorites ?fav-count))))
        (do-for-fact ((?r refuse-time)) (= ?r:teacher ?teacher)
            (if ?l:time then
                (bind ?viol-count (count-violations ?l:time ?r:time))
                (bind ?final-violations (+ ?final-violations ?viol-count)))))
    
    (bind ?final-score (- (* 3 ?final-favorites) (* 15 ?final-violations)))
    (bind ?satisfaction-rate (* 100 (/ ?final-favorites (* ?total-lessons 3))))
    
    (printout t "=== 最終優化結果 ===" crlf)
    (printout t "總課程數: " ?total-lessons crlf)
    (printout t "偏好時間總數: " ?final-favorites crlf)
    (printout t "違規時間總數: " ?final-violations crlf)
    (printout t "滿意度: " (round (* ?satisfaction-rate 100)) "%" crlf)
    (printout t "預測分數: " ?final-score crlf))