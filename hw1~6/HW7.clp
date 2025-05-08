(deftemplate sales (slot id)(multislot items))
(deftemplate same (multislot pair)(multislot items))
(deftemplate recommend(slot id)(multislot similar)(multislot items))

(deffacts initial (phase load-data))

(defrule assert-data
(phase load-data)
=>
(load-facts "record-01.txt")
(open "recommend.txt" out "w")
)

(defrule generate-recommend
(phase load-data)
(sales (id ?id1)(items $?))
=>
(assert (recommend(id ?id1)(similar)(items)))
)

(defrule generate-pair
(phase load-data)
(sales(id ?id1)(items $?))
(sales(id ?id2&~?id1)(items $?))
=>
(assert (same (pair ?id1 ?id2)(items)))
)

(defrule generate-same-items
(phase load-data)
?f <- (same (pair ?id1 ?id2)(items $?items))
(sales (id ?id1)(items $? ?x $?))
(sales (id ?id2)(items $? ?x $?))
(test (not (member$ ?x $?items)))
=>
(modify ?f (items $?items ?x))
)

(defrule change-phase-1
(declare (salience -10))
?f<-(phase load-data)
=>
(retract ?f)
(assert (phase find-similar))
)