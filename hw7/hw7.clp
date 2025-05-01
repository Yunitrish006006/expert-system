(deftemplate sales (slot id)(multislot items))
(deftemplate same (multislot pair)(multislot items))
(deftemplate recommend(slot id)(multislot similar)(multislot items))
(deftemplate max-common (slot id) (slot count))

(deffacts initial (phase load-data))

(defrule assert-data
(phase load-data)
=>
(load-facts "record-02.txt")
(open "recommend.txt" out "w")
)

(defrule generate-recommend
(phase load-data)
(sales (id ?id1)(items $?))
=>
(assert (recommend(id ?id1)(similar)(items)))
(assert (max-common (id ?id1) (count 0)))
)

(defrule generate-pair
(phase load-data)
(sales(id ?id1)(items $?items1))
(sales(id ?id2&:(> (str-compare ?id2 ?id1) 0))(items $?items2))
=>
(assert (same (pair ?id1 ?id2)(items)))
(assert (same (pair ?id2 ?id1)(items)))
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
(assert (phase find-max-common))
)

(defrule find-max-common-count
(phase find-max-common)
?f <- (max-common (id ?id1) (count ?count))
(same (pair ?id1 ?id2) (items $?common))
(test (> (length$ $?common) ?count))
=>
(modify ?f (count (length$ $?common)))
)

(defrule change-phase-2
(declare (salience -10))
?f <- (phase find-max-common)
=>
(retract ?f)
(assert (phase find-similar))
)

(defrule find-most-similar
(phase find-similar)
?f <- (recommend (id ?id1) (similar $?similar) (items $?))
(max-common (id ?id1) (count ?max-count))
(same (pair ?id1 ?id2) (items $?common&:(= (length$ $?common) ?max-count)))
(test (not (member$ ?id2 $?similar)))
=>
(modify ?f (similar $?similar ?id2))
)

(defrule change-phase-3
(declare (salience -10))
?f <- (phase find-similar)
=>
(retract ?f)
(assert (phase add-recommendations))
)

(defrule add-recommendations
(phase add-recommendations)
?f <- (recommend (id ?id1) (similar $?similar) (items $?items))
(sales (id ?id2&:(member$ ?id2 $?similar)) (items $? ?x $?))
(sales (id ?id1) (items $?items1))
(test (not (member$ ?x $?items1)))
(test (not (member$ ?x $?items)))
=>
(modify ?f (items $?items ?x))
)

(defrule change-phase-4
(declare (salience -10))
?f <- (phase add-recommendations)
=>
(retract ?f)
(assert (phase output))
)

(defrule output-recommendations
(phase output)
(recommend (id ?id1) (similar $?similar&:(> (length$ $?similar) 0)) (items $?items&:(> (length$ $?items) 0)))
=>
(printout out ?id1 "(" (implode$ $?similar) "):" $?items crlf)
)

(defrule close-file
(declare (salience -20))
(phase output)
=>
(close)
)