;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    定義基本轉換與預先計算乘法結果之事實            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 定義羅馬數字對應表
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

;; 定義乘法結果的模板，利用 multiplier 事實預先記錄固定運算結果
(deftemplate multiplier
   (slot factor)
   (slot base)
   (slot product))

(deffacts precomputed-products
   ;; factor 10 的運算結果
   (multiplier (factor 10) (base 1)     (product 10))
   (multiplier (factor 10) (base 10)    (product 100))
   (multiplier (factor 10) (base 100)   (product 1000))
   (multiplier (factor 10) (base 1000)  (product 10000))
   (multiplier (factor 10) (base 10000) (product 100000))
   (multiplier (factor 10) (base 100000)(product 1000000))
   
   ;; factor 5 的運算結果
   (multiplier (factor 5) (base 1)     (product 5))
   (multiplier (factor 5) (base 10)    (product 50))
   (multiplier (factor 5) (base 100)   (product 500))
   (multiplier (factor 5) (base 1000)  (product 5000))
   (multiplier (factor 5) (base 10000) (product 50000))
   (multiplier (factor 5) (base 100000)(product 500000))
   
   ;; factor 9 的運算結果
   (multiplier (factor 9) (base 1)     (product 9))
   (multiplier (factor 9) (base 10)    (product 90))
   (multiplier (factor 9) (base 100)   (product 900))
   (multiplier (factor 9) (base 1000)  (product 9000))
   (multiplier (factor 9) (base 10000) (product 90000))
   (multiplier (factor 9) (base 100000)(product 900000))
   
   ;; factor 4 的運算結果
   (multiplier (factor 4) (base 1)     (product 4))
   (multiplier (factor 4) (base 10)    (product 40))
   (multiplier (factor 4) (base 100)   (product 400))
   (multiplier (factor 4) (base 1000)  (product 4000))
   (multiplier (factor 4) (base 10000) (product 40000))
   (multiplier (factor 4) (base 100000)(product 400000))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   程式流程規則                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 輸入阿拉伯數字 (輸入 -1 結束)
(defrule input-number
   (not (number $?))
   =>
   (printout t "Enter an Arabic number (-1 to end): ")
   (assert (number (read)))
   (assert (digit 1000000))
)

;; 結束程式
(defrule end
   (declare (salience 10))
   (number -1)
   =>
   (halt)
)

;; 規則：數字位餘數除以當前位的數量落在 1 到 3 之間
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

;; 規則：當商數為 9 時，使用前置減法 (例如 IX, XC 等)
(defrule process-9
   ?f1 <- (digit ?digit&~0)
   ?f2 <- (number ?num)
   (test (= (integer (/ ?num ?digit)) 9))
   (conversion (arabic ?digit) (roman ?symbol1))
   ;; 利用 precomputed-products 取出 factor=10 的結果
   (multiplier (factor 10) (base ?digit) (product ?prod10))
   (conversion (arabic ?prod10) (roman ?symbol2))
   ;; 取得 factor=9 的結果用於餘數運算
   (multiplier (factor 9) (base ?digit) (product ?prod9))
   =>
   (retract ?f1 ?f2)
   (printout t ?symbol1 ?symbol2)
   (assert (number (mod ?num ?prod9)))
   (assert (digit (integer (/ ?digit 10))))
)

;; 規則：當商數為 4 時，使用前置減法 (例如 IV, XL 等)
(defrule process-4
   ?f1 <- (digit ?digit&~0)
   ?f2 <- (number ?num)
   (test (= (integer (/ ?num ?digit)) 4))
   (conversion (arabic ?digit) (roman ?symbol1))
   ;; 利用 factor=5 預先計算的結果
   (multiplier (factor 5) (base ?digit) (product ?prod5))
   (conversion (arabic ?prod5) (roman ?symbol2))
   ;; 並取出 factor=4 的結果以作模運算使用
   (multiplier (factor 4) (base ?digit) (product ?prod4))
   =>
   (retract ?f1 ?f2)
   (printout t ?symbol1 ?symbol2)
   (assert (number (mod ?num ?prod4)))
   (assert (digit (integer (/ ?digit 10))))
)

;; 規則：當商數介於 5 ~ 8 時，先輸出 5 倍符號，再加上餘下次數的基本符號
(defrule process-5plus
   ?f1 <- (digit ?digit&~0)
   ?f2 <- (number ?num)
   (test (>= (integer (/ ?num ?digit)) 5))
   (test (< (integer (/ ?num ?digit)) 9))
   (multiplier (factor 5) (base ?digit) (product ?prod5))
   (conversion (arabic ?prod5) (roman ?symbol5))
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

;; 規則：若該位沒有可輸出符號時，直接進入下一位
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

;; 規則：轉換完成，換行並提示下次輸入
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
