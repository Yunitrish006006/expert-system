;; ========= 基本設定 =========

(deftemplate edge (slot v1) (slot v2) (slot distance))
(deftemplate shortest (slot v1) (slot v2) (slot distance) (multislot route))
(deftemplate path (slot v1) (slot v2) (slot distance) (slot left) (multislot route))

(deffacts initial
  (vertex A) (vertex B) (vertex C) (vertex D) (vertex E)
  (vertex F) (vertex G) (vertex H) (vertex I) (vertex J)

  ;; 單向邊
  (edge (v1 A) (v2 B) (distance 8))
  (edge (v1 A) (v2 C) (distance 3))
  (edge (v1 A) (v2 D) (distance 12))
  (edge (v1 B) (v2 D) (distance 23))
  (edge (v1 B) (v2 E) (distance 19))
  (edge (v1 C) (v2 D) (distance 5))
  (edge (v1 C) (v2 F) (distance 39))
  (edge (v1 D) (v2 E) (distance 2))
  (edge (v1 D) (v2 F) (distance 32))
  (edge (v1 D) (v2 G) (distance 16))
  (edge (v1 E) (v2 G) (distance 7))
  (edge (v1 F) (v2 G) (distance 19))
  (edge (v1 F) (v2 H) (distance 17))
  (edge (v1 F) (v2 I) (distance 6))
  (edge (v1 G) (v2 I) (distance 11))
  (edge (v1 G) (v2 J) (distance 2))
  (edge (v1 H) (v2 I) (distance 25))
  (edge (v1 I) (v2 J) (distance 10))
)

;; ========= 使用者輸入 =========

(defrule input-vertex
  (declare (salience 100))
  =>
  (printout t "Start Vertex: ") (assert (start (read)))
  (printout t "End Vertex: ") (assert (end (read))))

;; ========= 建立雙向邊 =========

(defrule generate-anti-direction-edge
  (declare (salience 90))
  (edge (v1 ?v1) (v2 ?v2) (distance ?d))
  (not (edge (v1 ?v2) (v2 ?v1) (distance ?)))
  =>
  (assert (edge (v1 ?v2) (v2 ?v1) (distance ?d))))

;; ========= 初始化 shortest =========

(defrule initialize-shortest-from-edge
  (edge (v1 ?v1) (v2 ?v2) (distance ?d))
  =>
  (assert (shortest (v1 ?v1) (v2 ?v2) (distance ?d) (route ?v1 ?v2))))

(defrule generate-disconnected-distance
  (declare (salience 80))
  (vertex ?v1)
  (vertex ?v2&~?v1)
  (not (edge (v1 ?v1) (v2 ?v2) (distance ?)))
  =>
  (assert (shortest (v1 ?v1) (v2 ?v2) (distance 1000) (route ?v1 ?v2))))

;; ========= Dijkstra 更新邏輯 =========

(defrule update-shortest-distance
  (declare (salience 70))
  (shortest (v1 ?x) (v2 ?y) (distance ?d1) (route $?r1))
  (shortest (v1 ?y) (v2 ?z) (distance ?d2) (route $?r2))
  (test (< (+ ?d1 ?d2) 1000))
  (not (shortest (v1 ?x) (v2 ?z) (distance ?d3&:(< ?d3 (+ ?d1 ?d2)))))
  =>
  (bind ?newdist (+ ?d1 ?d2))
  (assert (shortest (v1 ?x) (v2 ?z) (distance ?newdist) (route $?r1 ?z))))

;; ========= 產生最終 path =========

(defrule generate-path
    (declare (salience 10))
  (start ?s)
  (end ?e)
  (shortest (v1 ?s) (v2 ?e) (distance ?d) (route $?r))
  =>
  (assert (path (v1 ?s) (v2 ?e) (distance ?d) (left ?d) (route $?r))))

;; ============= 刪除用不到的 ==========


;; ========= 輸出結果 =========

(defrule print-result
    (declare (salience -1))
  (path (v1 ?s) (v2 ?e) (distance ?d) (route $?r))
  =>
  (printout t "Distance: " ?d crlf)
  (printout t "Route: (" 
            (implode$ ?r) 
            ")" crlf))
