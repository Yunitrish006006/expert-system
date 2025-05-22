;;================================================資料定義================================================

;; 課程資料
(deftemplate course
  (slot id)               ;; 課程代碼
  (slot teacher)          ;; 授課教師
  (slot class)            ;; 所屬班級
  (slot hours-per-week)   ;; 每週節數
  (slot need-lab)         ;; 是否需要電腦教室 yes/no
)

;; 教師資料
(deftemplate teacher
  (slot name)
  (multislot preferred-times) ;; 偏好時段 (e.g., 101 102 110)
)

;; 排課結果
(deftemplate schedule
  (slot course-id)
  (slot teacher)
  (slot class)
  (slot period)           ;; 節次 (e.g., 101)
  (slot room)             ;; 教室
)

;; 教室資料
(deftemplate room
  (slot id)
  (slot type) ;; normal / lab
)

;; 衝突記錄
(deftemplate conflict
  (slot type)   ;; e.g., teacher-conflict, class-conflict, etc.
  (slot detail) ;; 描述
)

;;================================================初始資料輸入================================================

(deffacts data
  ;; 教師資料
  (teacher (name T1) (preferred-times 101 102 201 301))
  (teacher (name T2) (preferred-times 103 110 210 310))
  
  ;; 課程資料
  (course (id C101) (teacher T1) (class A1) (hours-per-week 3) (need-lab no))
  (course (id C102) (teacher T2) (class A1) (hours-per-week 2) (need-lab yes))

  ;; 教室資料
  (room (id R1) (type normal))
  (room (id Lab1) (type lab))
)

;;================================================規則定義================================================

;;自動分配課程到可行時段與教室
(defrule assign-course
  ?c <- (course (id ?cid) (teacher ?t) (class ?cls) (hours-per-week ?h) (need-lab ?lab))
  (teacher (name ?t) (preferred-times $?times))
  (room (id ?rid) (type ?rtype&:(if (eq ?lab yes) (eq ?rtype lab) else (eq ?rtype normal))))
  =>
  ;; 假設一週內的某些節次為可能的候選，這裡可由 preferred-times 再分配
  (assert (schedule (course-id ?cid) (teacher ?t) (class ?cls)
                    (period 101) (room ?rid))) ;; 這裡可重複出現類似規則以涵蓋多節
)

;;檢查老師重複排課
(defrule teacher-conflict
  (schedule (teacher ?t) (period ?p))
  (schedule (teacher ?t) (period ?p) (course-id ?c1&~?c2))
  =>
  (assert (conflict (type teacher-conflict) (detail (str-cat "Teacher " ?t " conflict at " ?p))))
)

;;檢查班級衝突
(defrule class-conflict
  (schedule (class ?cls) (period ?p))
  (schedule (class ?cls) (period ?p) (course-id ?c1&~?c2))
  =>
  (assert (conflict (type class-conflict) (detail (str-cat "Class " ?cls " conflict at " ?p))))
)

;;評分
(defrule score-good-period
  (schedule (teacher ?t) (period ?p))
  (teacher (name ?t) (preferred-times $? ?p $?))
  =>
  ;; 偏好時段加分
  (assert (score +2))
)

