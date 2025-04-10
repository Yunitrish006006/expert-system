(deftemplate car-problem (slot type))

(deftemplate car-problem-condition (slot shows))

(deffacts initial

    (car-problem-condition (shows noise-when-brake))

    (car-problem-condition (shows noise-from-tire))

    (car-problem-condition (shows water-leak))
)

 

(defrule brake-trouble-1

    (car-problem-condition (shows noise-when-brake))

    =>

    (assert (car-problem (type brake-trouble))))

(defrule brake-trouble-2

    (car-problem-condition (shows noise-from-tire))

    =>

    (assert (car-problem (type brake-trouble))))

 

(defrule water-tank-trouble-1

    (car-problem-condition (shows water-thermometer-H))

    =>

    (assert (car-problem (type water-tank-trouble))))

(defrule water-tank-trouble-2

    (car-problem-condition (shows water-leak))

    =>

    (assert (car-problem (type water-tank-trouble))))

 

(defrule engine-belt-loose-1

    (car-problem-condition (shows noise-from-engine-room))

    =>

    (assert (car-problem (type engine-belt-loose))))

 

(defrule car-battery-no-power-1

    (car-problem-condition (shows engine-cannot-catch))

    =>

    (assert (car-problem (type car-battery-no-power))))

 

(defrule deal-with-brake-trouble

    (car-problem (type brake-trouble))

    =>

    (printout t "check brake pedal and oil" crlf))

(defrule deal-with-water-tank-trouble

    (car-problem (type water-tank-trouble))

    =>

    (printout t "repair the water tank or add water" crlf))

(defrule deal-with-engine-belt-loose

    (car-problem (type engine-belt-loose))

    =>

    (printout t "change the engine belt" crlf))

(defrule deal-with-car-battery-no-power

    (car-problem (type car-battery-no-power))

    =>

    (printout t "replace or charge a car battery" crlf))
