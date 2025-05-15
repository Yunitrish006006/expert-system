;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 完整 CLIPS 程式（修正版）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. 定義句子模板與模組
(deftemplate sentence (multislot content))
(defmodule MAIN (export deftemplate sentence))
(defmodule PRODUCTION (import MAIN deftemplate sentence))
(defmodule RECOGNIZE (import MAIN deftemplate sentence))

;; 2. BNF 終端符號 deffacts
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

  (production <term> <adjective> <noun>)
  (production <term> <noun>)
  (production <term> <determiner> <noun>)
  (production <term> <determiner> <adjective> <noun>)
  (production <term> <determiner> <adjective> <adjective> <noun>)
  (production <term> <determiner> <adjective> <adjective> <adjective> <noun>)

  ;; 間接受詞
  (production <indirect-object> <preposition> <term>)

  ;; 句子
  (production <sentence> <term> <verb> <term> <end>)
  (production <sentence> <term> <verb> <term> <indirect-object> <end>)
)

;; 3. 主控制規則：讀入、拆詞、assert sentence
(defrule MAIN::control-rule
  (not (sentence (content $?)))
  =>
  (printout t "Enter a sentence (<Enter> to end): ")
  (bind ?input (readline))
  (if (eq ?input "") then
    (printout t crlf "Bye!" crlf)
    (halt))
  (bind ?words (explode$ ?input))
  (assert (sentence (content $?words <end>)))
  (focus PRODUCTION RECOGNIZE)
)

;; 4. PRODUCTION 模組：先嘗試完整句式再依長度分階段 reduce
(defrule PRODUCTION::reduce-sentence-io
  (declare (salience 300))
  ?f <- (sentence (content <term> ?v <term> <indirect-object>))
  =>
  (retract ?f)
  (assert (sentence (content <sentence>)))
  (focus RECOGNIZE)
)

(defrule PRODUCTION::reduce-sentence
  (declare (salience 290))
  ?f <- (sentence (content <term> ?v <term>))
  =>
  (retract ?f)
  (assert (sentence (content <sentence>)))
  (focus RECOGNIZE)
)

;; 原有分級 reduce 規則，salience 保持
(defrule PRODUCTION::reduce6
  (declare (salience 100))
  ?f <- (sentence (content $?front ?w1 ?w2 ?w3 ?w4 ?w5 ?w6 $?back))
  (production ?lhs ?w1 ?w2 ?w3 ?w4 ?w5 ?w6)
  =>
  (printout t "[reduce6] " $?front ?lhs $?back crlf)
  (retract ?f)
  (assert (sentence (content $?front ?lhs $?back)))
)

(defrule PRODUCTION::reduce5
  (declare (salience 90))
  ?f <- (sentence (content $?front ?w1 ?w2 ?w3 ?w4 ?w5 $?back))
  (production ?lhs ?w1 ?w2 ?w3 ?w4 ?w5)
  =>
  (retract ?f)
  (printout t "[reduce5] " $?front ?lhs $?back crlf)
  (assert (sentence (content $?front ?lhs $?back)))
)

(defrule PRODUCTION::reduce4
  (declare (salience 80))
  ?f <- (sentence (content $?front ?w1 ?w2 ?w3 ?w4 $?back))
  (production ?lhs ?w1 ?w2 ?w3 ?w4)
  =>
  (retract ?f)
  (printout t "[reduce4] " $?front ?lhs $?back crlf)
  (assert (sentence (content $?front ?lhs $?back)))
)

(defrule PRODUCTION::reduce3
  (declare (salience 70))
  ?f <- (sentence (content $?front ?w1 ?w2 ?w3 $?back))
  (production ?lhs ?w1 ?w2 ?w3)
  =>
  (retract ?f)
  (printout t "[reduce3] " $?front ?lhs $?back crlf)
  (assert (sentence (content $?front ?lhs $?back)))
)

(defrule PRODUCTION::reduce2
  (declare (salience 60))
  ?f <- (sentence (content $?front ?w1 ?w2 $?back))
  (production ?lhs ?w1 ?w2)
  =>
  (retract ?f)
  (printout t "[reduce2] " $?front ?lhs $?back crlf)
  (assert (sentence (content $?front ?lhs $?back)))
)

(defrule PRODUCTION::reduce1
  (declare (salience 50))
  ?f <- (sentence (content $?front ?w1 $?back))
  (production ?lhs ?w1)
  =>
  (retract ?f)
  (printout t "[reduce1] " $?front ?lhs $?back crlf)
  (assert (sentence (content $?front ?lhs $?back)))
)

(defrule PRODUCTION::reduce-io
  (declare (salience 300))
  ?f <- (sentence (content $?front ?w1 ?w2 $?back))
  (production <indirect-object> ?w1 ?w2)
  =>
  (printout t "[reduce-io] " $?front <indirect-object> $?back crlf)
  (retract ?f)
  (assert (sentence (content $?front <indirect-object> $?back)))
)

;; 5. RECOGNIZE 模組：判斷最終結果 RECOGNIZE 模組：判斷最終結果
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

;; 6. 執行示例：
;; (reset)  (run)
