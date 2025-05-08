(deftemplate data (multislot numbers))

(defrule read-data
    (declare (salience 100))
    =>
    (printout t "Data sorting:")
    (bind ?input (explode$ (readline)))
    (assert (data (numbers ?input)))
)

(defrule sort
    ?pre <- (data (numbers $?begin ?m1 ?m2 $?end))
    (test(> ?m1 ?m2))
    =>
    (assert (data (numbers $?begin ?m2 ?m1 $?end)))
    (retract ?pre)
)

(defrule print-sorted
    (declare (salience -1))
    (data (numbers $?x))
   =>
   (printout t "The result is :" (implode$ ?x) crlf)
   (halt)
)