;;;;;________________________________________;;;;les opérations arithmètiques;;;;____________________________________________________;;;;;;;;


(defun op_arithmetiques_binaire (exp)
    (let ((op (car exp))
    	   (param(cdr exp)))

    (append (compilation (car param)      ;;créer une autre fonction pour les opérations arith non_binaire et appeler la fonction binaire +
                                           ;; fair le test sur le troisième argument

    	'((PUSH :R0))
    	(compilateur (cadr param)

    		'((PUSH :R0))
    		'((POP :R1))
    		'((POP :R0))

    		  ('+ '((ADD :R0 :R1)))
              ('- '((SUB :R0 :R1)))
              ('* '((MUL :R0 :R1)))
              ('/ '((DIV :R0 :R1)))
              ))))))


(defun op_arithmetique (exp)
     (let ((op (car exp))
     	    (param (cdr exp)))
      (if (null (cddr param))
      	 (op_arithmetique_binaire(exp))

      	 (append (compilation  `(,op ,(list op (car arg) (cadr arg)) ,@(cddr arg))))
     )))



;;;;;;;;__________________________________________;;;les opérations de comparaison;;;_____________________________________________;;;;;;;


(defun op_comparaison(exp)
	(let ((op (car exp))
		(param (cdr exp)))
		(End (make-symbol "EndTest"))
		(append (compilation (cadr exp))
			'((PUSH :R0))
			(compilation (caddr exp))
				'((PUSH :R0))
				'((POP :R0))
				'((POP :R1))
				'((CMP :R1 :R0))
			;;((MOVE (:DIESE T) :R0))
				(case op
					('= `((JEQ (@ ,fin))))
	                ('< `((JL (@ ,fin))))
	                ('> `((JG (@ ,fin))))
	                ('<= `((JLE (@ ,fin))))
	                ('>= `((JGE (@ ,fin)))))

				;;'(move(:DIESE NIL) :R0))

				`((@ ,END)))))

;;;;;;;;;_________________________________________;;;les opérations booléennes;;;___________________________________________________;;;;;

(defun op_boolean(exp)
	(let (op (car exp))
		(param (cdr exp))
	    (END (make-symbol "fin"))

			(case op
				('AND 
				(if (null param)

			      (append '((MOVE (:DIESE T) :R0)) `((@ ,END))) 
				  (op_AND (param)) `(@, END)))


				('OR   
                 (if (null param)

			    (append '((MOVE (:DIESE NIL) :R0)) `((@ ,END))) 
			    (op_OR  (param)) `(@, END))

				))))


(defun op_AND(exp)
      (let (firstParam (car exp))
		(therest (cdr exp))
	    (END (make-symbol "fin"))

	    (append (compilateur (firstParam)) ;;(car exp)
	    '((CMP :R0 (:DIESE T)))
	    `((JNE (@ ,END)))
	    (op_boolean (therest)));;(cdr exp)   ;;à revoir ;;utilisation de op_boolean parexemple le cad de (AND A (OR B C))

	))


(defun op_OR(exp)
	(let ((firstParam (car exp))
		(therest (cdr exp))
	    (END (make-symbol "fin")))

	    (append (compilation (op)) ;;(car exp)
	    '((CMP :R0 (:DIESE T)))
	    `((JEQ (@ ,END)))
	    (op_boolean (therest)))));;(cdr exp)	) ;;


;;;;;;;________________________________________________;;;les conditions;;;;_____________________________________________________;;;;


(defun op-condition (exp)
   (let((condi (car exp)))

   	(case condi
   		('if 

   		(op_if (cdr exp)) 
   			`(@, END))

   		('cond

   		(op_Cond (cdr exp))
   			`(@, END)))
))



(defun op-if (exp)
	(let ((condition (car exp))
		  (actionOne (cadr exp))
		  (actionTwo (caddr exp))
		(sinon (gensym "else"))
	     (END (gensym "END")))
	   (append (compilateur (condition))

	   	'((CMP :R0 (:DIESE NIL)))
	   	 `((JEQ (@ ,sinon))))
	   (compilation (actionOne) )
	    `((JMP (@ ,END)))
	    `((@ ,sinon))  
	    (compilation (actionTwo))
	    `((@ ,END))))

	     
(defun op-cond (exp)
	(let ((END gensym "END")
		   (eachCond gensym "COND")
		   (condition-cond(caar exp))
		   (action-cond (cadar exp)))

	    (if(null exp)  (append 
	    	          '((MOVE (:DIESEL NIL) :R0))  
	    	          `((@ ,END)))

	    	 (append (compilateur (condition-cond))
	    	 	'((CMP :R0 (:DIESE NIL)))                                
	             `((JEQ (@ ,eachCond)))

	              (compilater (action-cond)) `((JMP (@ ,END)))
               `((@ ,eachCond))
                 (op-cond (cdr exp))
	           	))))

;;_______________________________________________;;;;les boucles;;;;;_________________________________________________________________;;;;


(defun op-boucles(exp)
    (let ((whatBoucle (car exp))
         (action (cdr exp)))
      (case whatBoucle

      	('while (boucle_while(action)))

      	('until (boucle_do_until(action)))

        ('progn (do_progn(action)))))
	)


(defun do_progn(exp)
    (let ((first-inst (car exp))
           (therest (cdr exp)))
    (if (null exp)
      ()
     (append (compilateur (car exp)) (op-boucles (cdr exp))))))


(defun boucle_while(exp)
    (let ((condition (car exp))
    	   (action (caddr exp))
    	   (END gensym "END")
    	   (TQ gensym "while"))    
        
        (append `((@ ,TQ)) 
        (compilateur (condition))
        `((CMP :R0 (:DIESE nil)))
		`((JEQ (@ ,END)))

		(compilateur (action))
		`((JMP (@ ,TQ)))
		`((@, END)))
    )
	)

(defun boucle_do_until (exp)
     (let ((condition (car exp))
     	(action (caddr exp))
     	(END gensym "END")
     	(JSQ gensym "until"))
        (append `((@ ,JSQ))
        (compilateur (condition))
        `((CMP :R0 (:DIESE T))) 
         `((JEQ (@ ,END)))

         (compilateur (action)) 
		`((JMP (@ ,TQ)))
		`((@, END)))))


;;;;;;__________________________________________________;;;les litteraux;;;;__________________________________________________________;;;;


;; reste à faire après ;;;   ;;;un apdate;;;