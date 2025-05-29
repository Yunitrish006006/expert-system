(deftemplate team (slot id) (multislot choice))
(deftemplate advisor (slot id) (multislot choice))
(deftemplate match (slot team) (slot advisor))

(deffacts initial
  (team (id t1)(choice a1 a2 a3 a4 a5 a6 a7 a8 a9 a10))
  (team (id t2)(choice a3 a5 a2 a1 a7 a6 a8 a4 a9 a10))
  (team (id t3)(choice a1 a3 a5 a2 a8 a4 a9 a7 a6 a10))
  (team (id t4)(choice a2 a3 a1 a5 a6 a4 a9 a7 a8 a10))
  (team (id t5)(choice a3 a5 a2 a1 a9 a8 a4 a6 a7 a10))
  (team (id t6)(choice a5 a1 a3 a2 a6 a4 a9 a8 a7 a10))
  (team (id t7)(choice a5 a2 a3 a4 a1 a9 a8 a6 a7 a10))
  (team (id t8)(choice a1 a5 a3 a2 a6 a4 a9 a8 a7 a10))
  (team (id t9)(choice a5 a3 a2 a4 a6 a8 a1 a9 a7 a10))
  (team (id t10)(choice a2 a1 a5 a3 a9 a8 a4 a7 a6 a10))
  (advisor (id a1)(choice t7 t5 t6 t2 t1 t10 t8 t3 t9 t4))
  (advisor (id a2)(choice t5 t6 t2 t7 t8 t3 t1 t10 t9 t4))
  (advisor (id a3)(choice t7 t6 t8 t1 t2 t5 t10 t9 t3 t4))
  (advisor (id a4)(choice t5 t7 t2 t6 t1 t10 t8 t9 t3 t4))
  (advisor (id a5)(choice t1 t8 t7 t5 t6 t2 t3 t10 t4 t9))
  (advisor (id a6)(choice t7 t5 t6 t8 t4 t1 t2 t10 t3 t9))
  (advisor (id a7)(choice t7 t6 t5 t2 t1 t9 t8 t10 t3 t4))
  (advisor (id a8)(choice t6 t5 t7 t8 t1 t10 t2 t3 t4 t9))
  (advisor (id a9)(choice t5 t7 t6 t1 t8 t4 t10 t3 t9 t4))
  (advisor (id a10)(choice t6 t5 t7 t1 t8 t4 t10 t3 t9 t4))

  (current-level 1)
  (last-level 10)
)


(defrule team-match-advisor
  (current-level ?c)
  ;;===========================
  (team (id ?t1) (choice $?teams ?a $?))
  (test (eq ?c (+ (length$ $?teams) 1)))
  (not (match (team ?) (advisor ?a)))
  (not (match (team ?t1) (advisor ?)))

  (not
    (and
      (team (id ?t2) (choice $?teams2 ?a $?))
      (advisor (id ?a) (choice $? ?t2 $? ?t1 $?))
      (test (eq ?c (+ (length$ $?teams2) 1) ))
      (not (match (team ?t2)))
    )
  )
  ;;===========================
  =>
  (assert (match (team ?t1) (advisor ?a)))
)

(defrule change-level
  (declare (salience -10))
  ?f1 <- (current-level ?c)
  =>
  (retract ?f1)
  (assert (current-level (+ ?c 1))))

(defrule level-finish
  (declare (salience 10))
  ?f1 <- (current-level ?c)
  ?f2 <- (last-level ?l)
  (test (> ?c ?l))
  =>
  (retract ?f1 ?f2))

(defrule print
  (declare (salience -20))
  (match (team ?t) (advisor ?a))
  =>
  (printout t "team " ?t " match advisor " ?a crlf))
