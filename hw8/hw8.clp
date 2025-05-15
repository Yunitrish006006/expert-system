(deftemplate ballot (slot id) (multislot order))
(deftemplate candidate (slot no) (slot votes))

(deffacts initial (phase load-data))


(deffunction not-member$ (?item $?list)
   (if (member$ ?item ?list) then FALSE else TRUE))

(deffunction count-facts (?template-name)
   (bind ?count 0)
   (do-for-all-facts ((?f ?template-name)) TRUE
      (bind ?count (+ ?count 1)))
   (return ?count))


(defrule assert-data
    ?f <- (phase load-data)
    =>
    (retract ?f)
    (load-facts "hw8/voting-04.txt")
    (printout t "The 1 round:" crlf)
    (assert (phase count))
    (assert (round 1))
    (assert (remove))
    (assert (processed))
    (assert (least none))
    (assert (total (count-facts ballot)))
    (assert (can_cnt (count-facts candidate)))
)

(defrule hard-trick
    ?f <- (phase before-cnt)
    (ballot (id B001) (order $?order))
    =>
    (retract ?f)
    (assert (can_cnt (length$ $?order)))
    (assert (phase count))
)


(defrule count-first
    (phase count)
    (ballot (id ?id) (order ?first $?))
    ?f1 <- (candidate (no ?first) (votes ?votes))
    ?f2 <- (processed $?processed)
    (test (not-member$ ?id $?processed))
    =>
    (retract ?f2)
    (modify ?f1 (votes (+ ?votes 1)))
    (assert (processed $?processed ?id))
)

(defrule change-phase-to-find
    (declare (salience -10))
    ?f <- (phase count)
    =>
    (retract ?f)
    (assert (phase find-elected))
)




(defrule find-elected-majority
    (phase find-elected)
    (round ?r)
    (total ?t)
    (remove $?remove)
    (candidate (no ?c) (votes ?v))
    (test (not (member$ ?c $?remove)))
    (test (> ?v (/ ?t 2)))
    =>
    (printout t "Candidate " ?c " is elected. (Number of Votes: " ?v ")" crlf)
    (halt)
)

(defrule find-current-least
    (phase find-elected)
    (candidate (no ?id) (votes ?votes))
    (remove $?remove)
    ?f_least <- (least ?least)
    (test (not (member$ ?id $?remove)))
    (test (or (eq ?least none) (< ?votes ?least)))
    =>
    (retract ?f_least)
    (assert (least ?votes))
)

(defrule mark-for-removal
    (declare (salience -1))
    (phase find-elected)
    ?f_c <- (candidate (no ?id) (votes ?votes))
    ?f_rm <- (remove $?remove)
    (least ?least)
    (test (not (eq ?least none)))
    (test (not (member$ ?id $?remove)))
    (test (= ?votes ?least))
    =>
    (retract ?f_rm)
    (retract ?f_c)
    (printout t "Candidate " ?id " ")
    (assert (remove $?remove ?id))
)

(defrule check-failure
    (declare (salience -5))
    ?f <- (phase find-elected)
    (can_cnt ?can_cnt)
    (remove $?remove)
    (least ?least) 
    (test (= ?can_cnt (length$ $?remove)))
    =>
    (retract ?f)
    (printout t " is removed. (Number of Votes: " ?least ")" crlf) ;; 💡 新增補印
    (assert (phase halt))
    (printout t crlf "Fail: All candidate are removed" crlf)
)


(defrule change-phase-to-next
    (declare (salience -10))
    ?f <- (phase find-elected)
    (least ?least)
    =>
    (retract ?f)
    (assert (phase next-round))
    (printout t " is removed. (Number of Votes: " ?least ")" crlf)
)


;重新


(defrule reset-votes
    (phase next-round)
    ?f_v <- (candidate (no ?no) (votes ?votes))
    =>
    (modify ?f_v (votes 0))
)

(defrule remove-eliminated-from-ballots
    (phase next-round)
    ?f <- (ballot (id ?) (order $?front ?no $?rear))
    (remove $?remove)
    (test (member$ ?no $?remove))
    =>
    (modify ?f (order $?front $?rear))
)

(defrule prepare-next-round
    (declare (salience -10))
    ?f <- (phase next-round)
    ?f_rnd <- (round ?round)
    ?f_least <- (least ?)
    ?f_p <- (processed $?p)
    =>
    (printout t "The " (+ ?round 1) " round:" crlf)
    (retract ?f)
    (retract ?f_rnd)
    (retract ?f_least)
    (retract ?f_p)
    (assert (round (+ ?round 1)))
    (assert (least none))
    (assert (phase count))
    (assert (processed))
)


