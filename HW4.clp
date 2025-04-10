(deftemplate conversion (slot arabic)(slot roman))
(deffacts conversions
   (conversion (arabic 1) (roman I))
   (conversion (arabic 5) (roman V))
   (conversion (arabic 10) (roman X))
   (conversion (arabic 50) (roman L))
   (conversion (arabic 100) (roman C))
   (conversion (arabic 500) (roman D))
   (conversion (arabic 1000) (roman M))
   (conversion (arabic 5000) (roman v))
   (conversion (arabic 10000) (roman x))
   (conversion (arabic 50000) (roman l))
   (conversion (arabic 100000) (roman c))
   (conversion (arabic 500000) (roman d))
   (conversion (arabic 1000000) (roman m))
)
;===================================================================
(defrule input-number
    (not (number $?))
    =>
    (printout t "Enter an Arabic number (-1 to end): ")
    (assert (number (read)))
    (assert (digit 1000000))
)

(defrule end
    (declare (salience 10))
    (number -1)
    =>
    (halt)
)

(defrule process-1
    ?f1 <- (digit ?digit&~0)
    ?f2 <- (number ?number)
    (test (= (integer (/ ?number ?digit)) 1))
    (conversion (arabic ?digit) (roman ?roman))
    =>
    (retract ?f1 ?f2)
    (printout t ?roman)
    (assert (number (mod ?number ?digit)))
    (assert (digit (integer (/ ?digit 10))))
)

(defrule process-2
    ?data <- (translation (string $?prv)(code $?a))
    ?ch <- (conversion (arabic ?c)(roman $?a))
    =>
    ; (printout t "prv: " ?prv crlf)
    ; (printout t "ch:" ?c crlf)
    (assert (translation (string $?prv ?c)(code)))
    (retract ?data)
)

(defrule print-result-1
    ?data <- (translation (string $?prv)(code))
    =>
    (printout t "The message is " (implode$ $?prv) crlf)
    (retract ?data)
)

(defrule print-result-2
    (declare (salience 1))
    ?data <- (translation (string $?prv)(code $? /))
    =>
    (printout t "The message is " (implode$ $?prv) crlf)
    (retract ?data)
)

(defrule print-result-3
    (declare (salience -1))
    ?data <- (translation (string $?)(code $?))
    =>
    (printout t "Can't decode this message" crlf)
    (retract ?data)
)