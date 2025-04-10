(deftemplate conversion
   (slot arabic)
   (slot roman))

(deffacts conversions
   (conversion (arabic 1)       (roman I))
   (conversion (arabic 5)       (roman V))
   (conversion (arabic 10)      (roman X))
   (conversion (arabic 50)      (roman L))
   (conversion (arabic 100)     (roman C))
   (conversion (arabic 500)     (roman D))
   (conversion (arabic 1000)    (roman M))
   (conversion (arabic 5000)    (roman v))
   (conversion (arabic 10000)   (roman x))
   (conversion (arabic 50000)   (roman l))
   (conversion (arabic 100000)  (roman c))
   (conversion (arabic 500000)  (roman d))
   (conversion (arabic 1000000) (roman m))
)

;;輸入
(defrule input-number
   (not (number $?))
   =>
   (printout t "Enter an Arabic number (-1 to end): ")
   (assert (number (read)))
   (assert (digit 1000000))
)

;;結束
(defrule end
   (declare (salience 10))
   (number -1)
   =>
   (halt)
)

;;重複數次
(defrule process-1to3
   ?f1 <- (digit ?digit&~0)
   ?f2 <- (number ?num)
   (test (< (integer (/ ?num ?digit)) 4))
   (test (> (integer (/ ?num ?digit)) 0))
   (conversion (arabic ?digit) (roman ?symbol))
   =>
   (retract ?f1 ?f2)
   (bind ?times (integer (/ ?num ?digit)))
   (while (> ?times 0)
      (printout t ?symbol)
      (bind ?times (- ?times 1))
   )
   (assert (number (mod ?num ?digit)))
   (assert (digit (integer (/ ?digit 10))))
)

;;處理9的部分
(defrule process-9
   ?f1 <- (digit ?digit&~0)
   ?f2 <- (number ?num)
   (test (= (integer (/ ?num ?digit)) 9))
   (conversion (arabic ?digit) (roman ?symbol1))
   (conversion (arabic (* 10 ?digit)) (roman ?symbol2))
   =>
   (retract ?f1 ?f2)
   (printout t ?symbol1 ?symbol2)
   (assert (number (mod ?num (* 9 ?digit))))
   (assert (digit (integer (/ ?digit 10))))
)
;;處理4的部分
(defrule process-4
   ?f1 <- (digit ?digit&~0)
   ?f2 <- (number ?num)
   (test (= (integer (/ ?num ?digit)) 4))
   (conversion (arabic ?digit) (roman ?symbol1))
   (conversion (arabic (* 5 ?digit)) (roman ?symbol2))
   =>
   (retract ?f1 ?f2)
   (printout t ?symbol1 ?symbol2)
   (assert (number (mod ?num (* 4 ?digit))))
   (assert (digit (integer (/ ?digit 10))))
)
;;處理5~8的部分
(defrule process-5plus
   ?f1 <- (digit ?digit&~0)
   ?f2 <- (number ?num)
   (test (>= (integer (/ ?num ?digit)) 5))
   (test (< (integer (/ ?num ?digit)) 9))
   (conversion (arabic (* 5 ?digit)) (roman ?symbol5))
   (conversion (arabic ?digit) (roman ?symbol1))
   =>
   (retract ?f1 ?f2)
   (bind ?times (integer (/ ?num ?digit)))
   (printout t ?symbol5)
   (bind ?cnt (- ?times 5))
   (while (> ?cnt 0)
      (printout t ?symbol1)
      (bind ?cnt (- ?cnt 1))
   )
   (assert (number (mod ?num ?digit)))
   (assert (digit (integer (/ ?digit 10))))
)

;;處理0的部分
(defrule skip-zero
   ?f1 <- (digit ?digit&~0)
   ?f2 <- (number ?num)
   (test (= (integer (/ ?num ?digit)) 0))
   (test (> ?num 0))
   =>
   (retract ?f1 ?f2)
   (assert (number ?num))
   (assert (digit (integer (/ ?digit 10))))
)
;;運算結束的判斷
(defrule done
   ?f1 <- (digit ?digit)
   ?f2 <- (number ?num)
   (test (or (<= ?digit 0) (= ?num 0)))
   =>
   (retract ?f1 ?f2)
   (printout t crlf)
   (assert (number (read)))
   (assert (digit 1000000))
)

