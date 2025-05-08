;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 已有程式碼到這裡（金鑰：不要變動下面這一段）
(deftemplate ballot   (slot id) (multislot order))
(deftemplate candidate (slot no) (slot votes))
(deffacts initial (phase load-data))
(defrule assert-data
  ?f <- (phase load-data)
  =>
  (retract ?f)
  (load-facts "hw8/vote-01.txt")
  (printout t "The 1 round:" crlf)
  (assert (phase count))
  (assert (round 1))
  (assert (remove))
  (assert (processed))
  (assert (least none))
)

(defrule count-first
  (phase count)
  (ballot (id ?id) (order ?first $?))
  ?f1 <- (candidate (no ?first) (votes ?votes))
  ?f2 <- (processed $?processed)
  (test (not (member$ ?id ?processed)))
  =>
  (retract ?f2)
  (modify ?f1 (votes (+ ?votes 1)))
  (assert (processed (create$ ?processed ?id)))
)
(defrule change-phase-1
  (declare (salience -10))
  ?f <- (phase count)
  =>
  (retract ?f)
  (assert (phase find-elected))
)
;; 既有程式碼到這裡結束
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 以下是補完的規則 —— 從「找當選／淘汰」到「下一輪」的完整流程
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. 找出已超過半數（> total/2）的候選人
(defrule find-elected
  (phase find-elected)
  (total ?T)
  (candidate (no ?no) (votes ?v&:(> ?v (/ ?T 2))))
  =>
  (printout t ">>> Candidate " ?no " is ELECTED with " ?v " votes." crlf)
  ;; 結束比賽
  (assert (phase halt))
)

;; 2. 找出得票最少的候選人（可能多位）
(defrule find-least
  (phase find-elected)
  (candidate (no ?n1) (votes ?v1))
  (not (candidate (no ?n2) (votes ?v2&:(< ?v2 ?v1))))
  =>
  ;; 標記所有得票等於最少票數的候選人
  (assert (least ?n1))
  (printout t ">>> Candidate " ?n1 " is REMOVED (lowest votes: " ?v1 ")." crlf)
  ;; 進入移除階段
  (assert (phase remove))
)

;; 3. 移除標記的候選人
(defrule remove-candidate
  (phase remove)
  (least ?no)
  ?c <- (candidate (no ?no) (votes ?))
  =>
  (retract ?c)
)

;; 4. 清除 least 標記，準備下一輪
(defrule cleanup-least
  (phase remove)
  (least ?l)
  (not (candidate (no ?l)))
  =>
  ;; 清除 old data
  (retract (least ?l))
  (retract (processed $?))
  ;; 票數歸零
  (bind ?all (find-all-facts ((?c candidate)) TRUE))
  (loop-for-count (?i 1 (length$ ?all))
    (bind ?ci (nth$ ?i ?all))
    (modify ?ci (votes 0))
  )
  ;; 下一輪
  (assert (round (+ (round) 1)))
  (printout t crlf "---- Round " (round) " ----" crlf)
  (assert (phase count))
)

;; 5. 如果所有候選人都被移除（平手），宣告失敗
(defrule fail-all-removed
  (phase find-elected)
  (not (candidate (no ?)))
  =>
  (printout t "*** Fail: all candidates have been removed. ***" crlf)
  (assert (phase halt))
)

;; 6. 主迴圈：只要沒進入 halt，就不斷 run
(defrule continue-running
  (phase halt)
  =>
  (printout t crlf "*** Election process completed. ***" crlf)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

