;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. 先定義句子模板，再宣告 MAIN 模組匯出它
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate sentence (multislot content))

(defmodule MAIN (export deftemplate sentence))
(defmodule PRODUCTION (import MAIN deftemplate sentence))
(defmodule RECOGNIZE (import MAIN deftemplate sentence))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. BNF 終端符號的 deffacts（圖片中 productions）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffacts PRODUCTION::productions
  (production <determiner> a)
  (production <determiner> an)
  (production <determiner> the)
  (production <adjective> small)
  (production <adjective> big)
  (production <adjective> white)
  (production <adjective> black)
  (production <noun> dog)
  (production <noun> cat)
  (production <noun> yard)
  (production <noun> house)
  (production <preposition> at)
  (production <preposition> in)
  (production <preposition> with)
  (production <verb> pursue)
  (production <verb> catch)
  (production <verb> bite)
  (production <verb> scratch)

  ;; term: determiner + up to三個 adj + noun
  (production <term> <noun>)
  (production <term> <determiner> <noun>)
  (production <term> <determiner> <adjective> <noun>)
  (production <term> <determiner> <adjective> <adjective> <noun>)
  (production <term> <determiner> <adjective> <adjective> <adjective> <noun>)

  ;; 間接受詞
  (production <indirect-object> <preposition> <term>)

  ;; 句子
  (production <sentence> <term> <verb> <term>)
  (production <sentence> <term> <verb> <term> <indirect-object>)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. 主控制規則：讀入、拆詞、assert sentence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule MAIN::control-rule
  (not (sentence (content $?)))
  =>
  (printout t "Enter a sentence (<Enter> to end): ")
  (bind ?input (readline))
  (if (eq ?input "") then
    (printout t crlf "Bye!" crlf)
    (halt))
  (bind ?words (explode$ ?input))
  (assert (sentence (content $?words)))
  (focus PRODUCTION RECOGNIZE)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRODUCTION 模組：bottom-up 簡化規則（加 salience）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule PRODUCTION::reduce6
  (declare (salience 100))
  ?f <- (sentence (content $?front ?w1 ?w2 ?w3 ?w4 ?w5 ?w6 $?back))
  (production ?lhs ?w1 ?w2 ?w3 ?w4 ?w5 ?w6)
  =>
  (retract ?f)
  (assert (sentence (content $?front ?lhs $?back)))
)

(defrule PRODUCTION::reduce5
  (declare (salience 90))
  ?f <- (sentence (content $?front ?w1 ?w2 ?w3 ?w4 ?w5 $?back))
  (production ?lhs ?w1 ?w2 ?w3 ?w4 ?w5)
  =>
  (retract ?f)
  (assert (sentence (content $?front ?lhs $?back)))
)

(defrule PRODUCTION::reduce4
  (declare (salience 80))
  ?f <- (sentence (content $?front ?w1 ?w2 ?w3 ?w4 $?back))
  (production ?lhs ?w1 ?w2 ?w3 ?w4)
  =>
  (retract ?f)
  (assert (sentence (content $?front ?lhs $?back)))
)

(defrule PRODUCTION::reduce3
  (declare (salience 70))
  ?f <- (sentence (content $?front ?w1 ?w2 ?w3 $?back))
  (production ?lhs ?w1 ?w2 ?w3)
  =>
  (retract ?f)
  (assert (sentence (content $?front ?lhs $?back)))
)

(defrule PRODUCTION::reduce2
  (declare (salience 60))
  ?f <- (sentence (content $?front ?w1 ?w2 $?back))
  (production ?lhs ?w1 ?w2)
  =>
  (retract ?f)
  (assert (sentence (content $?front ?lhs $?back)))
)

(defrule PRODUCTION::reduce1
  (declare (salience 50))
  ?f <- (sentence (content $?front ?w1 $?back))
  (production ?lhs ?w1)
  =>
  (retract ?f)
  (assert (sentence (content $?front ?lhs $?back)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. RECOGNIZE 模組：判斷最終結果
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule RECOGNIZE::success
  ?s <- (sentence (content <sentence>))
  =>
  (retract ?s)
  (printout t "Correct!" crlf)
)

(defrule RECOGNIZE::failure
  ?s <- (sentence (content $?anything))
  (test (neq (length$ ?anything) 1))
  =>
  (retract ?s)
  (printout t "Wrong!" crlf)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. 例子：載入之後執行
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (reset)  (run)
