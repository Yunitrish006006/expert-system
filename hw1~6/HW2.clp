(deftemplate person (slot name)(slot sex))
(deftemplate parent-children (multislot parent)(multislot children))
(deftemplate pair (slot husband)(slot wife))
(deftemplate query (slot name)(slot subject))
(deffacts initial
    (person (name John) (sex male))
    (person (name Peter) (sex male))
    (person (name David) (sex male))
    (person (name Joe) (sex male))
    (person (name Kevin) (sex male))
    (person (name Mary) (sex female))
    (person (name Sue) (sex female))
    (person (name Linda) (sex female))
    (person (name Sherry) (sex female))

    (pair (husband John)(wife Sue))
    (pair (husband David)(wife Linda))
    (pair (husband Peter)(wife Sherry))

    (parent-children (parent John Sue)(children Mary Kevin))
    (parent-children (parent Peter Sherry)(children John Linda))
    (parent-children (parent David Linda)(children Joe))

    (query (name Mary)(subject Kevin))
    (query (name Sue)(subject Kevin))
    (query (name Peter)(subject Kevin))
    (query (name David)(subject Kevin))
    (query (name Linda)(subject Kevin))
    (query (name Joe)(subject Kevin))
)

(defrule define-brother
    (query (name ?p1)(subject ?p2))
        (or 
            (parent-children (parent $?) (children $? ?p1 $? ?p2 $?))
            (parent-children (parent $?) (children $? ?p2 $? ?p1 $?))
        )
    (person (name ?p1)(sex male))
    =>
    (printout t ?p1 " is "?p2"'s brother." crlf)
)

(defrule define-sister
    (query (name ?p1)(subject ?p2))
        (or 
            (parent-children (parent $?) (children $? ?p1 $? ?p2 $?))
            (parent-children (parent $?) (children $? ?p2 $? ?p1 $?))
        )
    (person (name ?p1)(sex female))
    =>
    (printout t ?p1 " is "?p2"'s sister." crlf)
)

(defrule define-farther
    (query (name ?p1)(subject ?p2))
        (or 
            (parent-children (parent $? ?p1) (children $? ?p2 $?))
            (parent-children (parent ?p1 $?) (children $? ?p2 $?))
        )
    (person (name ?p1)(sex male))
    =>
    (printout t ?p1 " is "?p2"'s farther." crlf)
)

(defrule define-mother
    (query (name ?p1)(subject ?p2))
        (or 
            (parent-children (parent $? ?p1) (children $? ?p2 $?))
            (parent-children (parent ?p1 $?) (children $? ?p2 $?))
        )
    (person (name ?p1)(sex female))
    =>
    (printout t ?p1 " is "?p2"'s mother." crlf)
)

(defrule define-grandfather
    (query (name ?p1)(subject ?p2))
    (parent-children (parent $? ?p1 $?) (children $? ?p3 $?))
    (parent-children (parent $? ?p3 $?) (children $? ?p2 $?))
    (person (name ?p1)(sex male))
    =>
    (printout t ?p1 " is "?p2"'s grandfather." crlf)
)

(defrule define-grandmother
    (query (name ?p1)(subject ?p2))
    (parent-children (parent $? ?p1 $?) (children $? ?p3 $?))
    (parent-children (parent $? ?p3 $?) (children $? ?p2 $?))
    (person (name ?p1)(sex female))
    =>
    (printout t ?p1 " is "?p2"'s grandmother." crlf)
)

(defrule define-uncle
    (query (name ?p1)(subject ?p2))
    (parent-children (parent $? ?p3 $?) (children $? ?p2 $?));找到同輩份的(p2父母)
    (
        or
        (parent-children (parent $?) (children $? ?p3 $? ?p1 $?)) 
        (parent-children (parent $?) (children $? ?p1 $? ?p3 $?)) ;父母是否為兄弟
	    (and 
            (pair (husband ?p1) (wife ?p0))
	        (or 
                (parent-children (parent $?) (children $? ?p0 $? ?p3 $?))
	            (parent-children (parent $?) (children $? ?p3 $? ?p0 $?))
            )
        )
    )
    (person (name ?p1)(sex male))
    
    =>
    (printout t ?p1 " is "?p2"'s uncle." crlf)
)

(defrule define-aunt
    (query (name ?p1)(subject ?p2))
    (parent-children (parent $? ?p3 $?) (children $? ?p2 $?));找到同輩份的(p2父母)
    (
        or
        (parent-children (parent $?) (children $? ?p3 $? ?p1 $?)) 
        (parent-children (parent $?) (children $? ?p1 $? ?p3 $?)) ;父母是否為兄弟
	    (and 
            (pair (husband ?p0) (wife ?p1))
	        (or 
                (parent-children (parent $?) (children $? ?p0 $? ?p3 $?))
	            (parent-children (parent $?) (children $? ?p3 $? ?p0 $?))
            )
        )
    )
    (person (name ?p1)(sex female))
    
    =>
    (printout t ?p1 " is "?p2"'s aunt." crlf)
)

(defrule define-cousin
    (query (name ?p1)(subject ?p2))
    (parent-children (parent $? ?p1p $?) (children $? ?p1 $?))
    (parent-children (parent $? ?p2p $?) (children $? ?p2 $?))
    (or
        (parent-children (parent $?) (children $? ?p1p $? ?p2p $?))
        (parent-children (parent $?) (children $? ?p2p $? ?p1p $?))
    )
    =>
    (printout t ?p1 " is "?p2"'s cousin." crlf)
)