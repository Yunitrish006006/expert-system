(deftemplate set (multislot name)(multislot members))

(deffacts initial(phase input))

(defrule input-number
    (phase input)
    =>
    (printout t "Please input an integer set s1: ")
    (assert (set (name s1) (members (explode$ (readline)))))
    (printout t "Please input an integer set s2: ")
    (assert (set (name s2) (members (explode$ (readline)))))
)

(defrule check-input-error-1
    ?f1 <- (phase input)
    ?f2 <- (set(name ?t1)(members $? ?a $? ?a $?))
    ?f3 <- (set(name ?t2&~?t1)(members $?))
    =>
    (retract ?f1 ?f2 ?f3)
    (printout t "Input error!!Duplicate elements are not allowed in sets!!" crlf)
    (assert (phase input))
)

(defrule check-input-error-2
    ?f1 <- (phase input)
    ?f2 <- (set(name ?t1)(members $? ?a $?))
    ?f3 <- (set(name ?t2&~?t1)(members $?))
    (test (not (integerp ?a)))
    =>
    (retract ?f1 ?f2 ?f3)
    (printout t "Input error!! Some elements are not integers!!" crlf)
    (assert (phase input))
)

(defrule input-no-error
  (declare (salience -10))
  ?f <- (phase input)
  =>
  (retract ?f)
  (assert (phase compute))
)


(deffunction union$ (?l1 ?l2)
    (bind ?result (create$))
    (bind ?i 1)
    (bind ?n1 (length$ ?l1))
    (while (<= ?i ?n1) do
    (bind ?result (create$ ?result (nth$ ?i ?l1)))
    (bind ?i (+ ?i 1)))
    (bind ?i 1)
    (bind ?n2 (length$ ?l2))
    (while (<= ?i ?n2) do
    (bind ?elem (nth$ ?i ?l2))
    (if (not (member$ ?elem ?result)) then
    (bind ?result (create$ ?result ?elem)))
    (bind ?i (+ ?i 1)))
    ?result
)

(deffunction intersection$ (?l1 ?l2)
    (bind ?result (create$))
    (bind ?i 1)
    (bind ?n1 (length$ ?l1))
    (while (<= ?i ?n1) do
    (bind ?elem (nth$ ?i ?l1))
    (if (member$ ?elem ?l2) then
    (bind ?result (create$ ?result ?elem)))
    (bind ?i (+ ?i 1)))
    ?result
)

(deffunction sort$ (?lst)
    (bind ?n (length$ ?lst))
    (if (<= ?n 1) then
        (return ?lst)
    )
    (bind ?res ?lst)
    (bind ?i 1)
    (while (< ?i ?n) do
        (bind ?j 1)
        (while (<= ?j (- ?n ?i)) do
            (bind ?a (nth$ ?j ?res))
            (bind ?b (nth$ (+ ?j 1) ?res))
            (if (> ?a ?b) then
                (bind ?res (replace$ ?res ?j ?j ?b))
                (bind ?res (replace$ ?res (+ ?j 1) (+ ?j 1) ?a))
            )
            (bind ?j (+ ?j 1))
        )
        (bind ?i (+ ?i 1))
    )
    ?res
)

(defrule compute-and-output
  (phase compute)
  ?s1 <- (set (name s1) (members $?m1))
  ?s2 <- (set (name s2) (members $?m2))
  =>
  (bind ?union     (union$ ?m1 ?m2))
  (bind ?intersect (intersection$ ?m1 ?m2))
  (bind ?s1s (sort$ ?m1))
  (bind ?s2s (sort$ ?m2))
  (bind ?us  (sort$ ?union))
  (bind ?is  (sort$ ?intersect))

  (open "set.txt" outf "w")
  (printout outf "(set (name s1)              (members " (implode$ ?s1s)     "))" crlf)
  (printout outf "(set (name s2)              (members " (implode$ ?s2s)     "))" crlf)
  (printout outf "(set (name s1 union s2)     (members " (implode$ ?us)       "))" crlf)
  (printout outf "(set (name s1 intersect s2) (members " (implode$ ?is)       "))" crlf)
  (close outf)

  (printout t "Output written to set.txt" crlf)
  (retract ?s1 ?s2)
  (assert (phase done))
)


(defrule done
  (phase done)
  =>
  (printout t "All done." crlf)
)
