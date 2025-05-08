(deftemplate permutation (multislot values)(multislot rest))

(deffacts initial (total 0))

(defrule read-base-fact
    (declare (salience 100))
    =>
    (printout t "please input a base fact for the permutation: ")
    (bind ?input (explode$(readline)))
    (assert (permutation (values)(rest ?input)))
    ; (printout t "You have entered: "?input crlf)
)

(defrule generator
    (permutation (values $?v) (rest $?r_1 ?x $?r_2))
    =>
    (assert (permutation (values ?v ?x ) (rest ?r_1 ?r_2)))
)

(defrule print
    ?dat <- (permutation (values $?v) (rest))
    ?old-total <- (total ?total)
    =>
    (retract ?old-total ?dat)
    (printout t "Permutation is " ?v crlf)
    (assert (total (+ ?total 1)))
)

(defrule print_total
    (declare (salience -1))
    ?old_total <- (total ?total) 
    =>
    (printout t "total is " ?total crlf)
)