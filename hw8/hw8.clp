(deftemplate ballot (slot id) (multislot order))
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
  (test (not (member$ ?id $?processed)))
  =>
  (retract ?f2)
  (modify ?f1 (votes (+ ?votes 1)))
  (assert (processed (create$ $?processed ?id)))
)
(defrule change-phase-1
  (declare (salience -10))
  ?f <- (phase count)
  =>
  (retract ?f)
  (assert (phase find-elected))
)



