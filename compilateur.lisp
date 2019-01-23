
(require "compilateur-functions.lisp")


(defun compilateur (exp)

	(if (atom exp) ()
	  (let ((operation (car exp))
	  	    (params(cdr exp)))

	      (cond
	      ((atom exp) (op-litteraux(exp)))
	       ((member (operation) '(+ - * /)) (op_arithmetique(exp)))
	       ((member (operation) '(< > = =< =>)) (op_comparaison(exp)))
	       ((member (operation) '(and or)) (op_boolean(exp)))
	       ((member (operation) '(if cond)) (op-condition(exp)))
	       ((member (operation) '(until while progn)) (op-boucles(exp)))
	       ))))