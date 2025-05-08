(deftemplate ballot (slot id) (multislot order))
(deftemplate candidate (slot no) (slot votes))
(deftemplate counted (slot id))
(deftemplate removed (slot no))
(deftemplate to-remove (slot no))
(deftemplate process-ballot (slot id) (multislot prefs))

;; 初始階段設置
(deffacts initial
  (phase load-data)
)

;; 載入資料並開始第一輪投票
(defrule assert-data
  ?f <- (phase load-data)
  =>
  (retract ?f)
  (load-facts "voting-02.txt")
  (printout t "The 1 round:" crlf)
  (assert (phase count-first-choices))
  (assert (round 1))
)

;; 計算第一輪選票的第一優先選擇
(defrule count-first-choices
  (phase count-first-choices)
  (ballot (id ?id) (order ?first $?))
  ?c <- (candidate (no ?first) (votes ?votes))
  (not (counted (id ?id)))
  =>
  (modify ?c (votes (+ ?votes 1)))
  (assert (counted (id ?id)))
)

;; 第一輪計票完成，轉換到尋找當選人階段
(defrule finish-first-count
  (declare (salience -10))
  ?f <- (phase count-first-choices)
  =>
  (retract ?f)
  (assert (phase find-elected))
)

;; 尋找當選人：檢查是否有候選人獲得超過半數選票
(defrule find-elected
  (phase find-elected)
  (candidate (no ?no) (votes ?votes))
  (total ?total)
  (test (> ?votes (/ ?total 2)))
  ?r <- (round ?round)
  =>
  (printout t "Candidate " ?no " is elected. (Number of Votes: " ?votes ")" crlf)
  (retract ?r)
  (halt)
)

;; 當只剩下一位未被移除的候選人時，該候選人當選
(defrule last-remaining-candidate
  (declare (salience -5))
  (phase find-elected)
  (candidate (no ?no) (votes ?votes))
  (not (removed (no ?no)))
  (not (and (candidate (no ?no2&:(neq ?no ?no2)))
            (not (removed (no ?no2)))))
  ?r <- (round ?round)
  =>
  (printout t "Candidate " ?no " is elected. (Number of Votes: " ?votes ")" crlf)
  (retract ?r)
  (halt)
)

;; 如果沒有當選人，轉換到尋找最少票數的候選人階段
(defrule no-winner-found
  (declare (salience -10))
  ?f <- (phase find-elected)
  =>
  (retract ?f)
  (assert (phase find-min))
  (assert (min-votes 9999))
)

;; 找出所有未被移除候選人的最小票數
(defrule find-min-votes
  (phase find-min)
  ?m <- (min-votes ?min)
  (candidate (no ?no) (votes ?votes))
  (not (removed (no ?no)))
  (test (< ?votes ?min))
  =>
  (retract ?m)
  (assert (min-votes ?votes))
)

;; 標記所有獲得最少票數的候選人準備移除
(defrule mark-candidates-for-removal
  (declare (salience -5))
  (phase find-min)
  (min-votes ?min)
  (candidate (no ?no) (votes ?votes&:(= ?votes ?min)))
  (not (removed (no ?no)))
  (not (to-remove (no ?no)))
  =>
  (assert (to-remove (no ?no)))
)

;; 完成標記最少票數的候選人，準備打印結果
(defrule prepare-removal
  (declare (salience -15))
  ?f <- (phase find-min)
  (min-votes ?min)
  (or (to-remove (no ?)) (test (> ?min 0)))
  =>
  (retract ?f)
  (assert (phase print-removed))
)

;; 打印被移除的候選人
(defrule print-removed
  (phase print-removed)
  (min-votes ?min)
  =>
  (printout t "Candidate ")
  (assert (phase print-names))
)

;; 循環打印所有被移除候選人的編號
(defrule print-removed-names
  (phase print-names)
  ?r <- (to-remove (no ?no))
  =>
  (retract ?r)
  (assert (removed (no ?no)))
  (printout t ?no " ")
)

;; 完成打印被移除候選人名單
(defrule finish-printing
  (declare (salience -10))
  (phase print-names)
  (min-votes ?min)
  (not (to-remove (no ?)))
  ?p <- (phase print-names)
  =>
  (retract ?p)
  (printout t "is removed. (Number of Votes: " ?min ")" crlf)
  (assert (phase prepare-next-round))
)

;; 準備下一輪計票
(defrule prepare-next-round
  (phase prepare-next-round)
  ?p <- (phase prepare-next-round)
  ?r <- (round ?round)
  ?m <- (min-votes ?min)
  =>
  (retract ?p ?r ?m)
  (assert (round (+ ?round 1)))
  (printout t "The " (+ ?round 1) " round:" crlf)
  (assert (phase reset-votes-start))
  (assert (reset-votes-flag))
)

;; 初始化重置候選人票數
(defrule start-reset-votes
  (phase reset-votes-start)
  ?p <- (phase reset-votes-start)
  =>
  (retract ?p)
  (assert (phase reset-votes))
)

;; 重置所有候選人票數 - 修改成只修改一個而不產生新的實例
(defrule reset-votes
  (phase reset-votes)
  ?c <- (candidate (no ?no) (votes ?votes&:(> ?votes 0)))
  =>
  (modify ?c (votes 0))
)

;; 完成重置候選人票數
(defrule finish-reset-votes
  (declare (salience -5))
  ?p <- (phase reset-votes)
  ?f <- (reset-votes-flag)
  (not (candidate (votes ?v&:(> ?v 0))))
  =>
  (retract ?p ?f)
  (assert (phase clear-counted))
)

;; 清除已計票標記，準備重新計票
(defrule clear-counted
  (phase clear-counted)
  ?c <- (counted (id ?id))
  =>
  (retract ?c)
)

;; 完成清除計票標記
(defrule finish-clear-counted
  (declare (salience -5))
  ?p <- (phase clear-counted)
  (not (counted (id ?)))
  =>
  (retract ?p)
  (assert (phase count-redistributed))
)

;; 重新分配選票: 處理每張選票
(defrule redistribute-votes
  (phase count-redistributed)
  (ballot (id ?id) (order $?prefs))
  (not (counted (id ?id)))
  =>
  (assert (process-ballot (id ?id) (prefs $?prefs)))
)

;; 處理選票: 尋找未被移除的第一個優先選擇
(defrule find-first-valid-preference
  (phase count-redistributed)
  ?p <- (process-ballot (id ?id) (prefs ?first $?rest))
  (not (removed (no ?first)))
  ?c <- (candidate (no ?first) (votes ?votes))
  =>
  (retract ?p)
  (modify ?c (votes (+ ?votes 1)))
  (assert (counted (id ?id)))
)

;; 處理選票: 第一優先已被移除，檢查下一個
(defrule skip-removed
  (phase count-redistributed)
  ?p <- (process-ballot (id ?id) (prefs ?first $?rest))
  (removed (no ?first))
  =>
  (retract ?p)
  (assert (process-ballot (id ?id) (prefs $?rest)))
)

;; 處理選票: 所有優先選擇都已被移除
(defrule all-preferences-removed
  (phase count-redistributed)
  ?p <- (process-ballot (id ?id) (prefs))
  =>
  (retract ?p)
  (assert (counted (id ?id)))
)

;; 計票完成，轉換到尋找當選人階段
(defrule finish-redistribution
  (declare (salience -10))
  ?p <- (phase count-redistributed)
  (not (process-ballot (id ?)))
  =>
  (retract ?p)
  (assert (phase find-elected))
)



