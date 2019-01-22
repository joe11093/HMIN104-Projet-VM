;;make-mv
;;nom: vm par default
;;taille: 10000 par default
(defun make-mv (&optional (nom 'vm) (taille-vm 10000))
	(set-property nom 'taille-vm taille-vm) ;;sauvegarder la taille de la memoire
	(set-property nom 'memoire (make-array taille-vm))	;;creer le tableau de la memoire
	(set-property nom 'registres (make-hash-table))		;;creer le hash table des registres, to have key: value for registers
	;;les registres avec une valeure initiale
	(set-registre nom 'R0 NIL)
	(set-registre nom 'R1 NIL)
	(set-registre nom 'R2 NIL)
	(set-registre nom 'R3 NIL)
	;;registres de la pile
	(set-registre nom 'BP 100) ;;la pile commence à l'index 100
	(set-registre nom 'SP 100)	;;SP = BP pour une pile vide
	(set-registre nom 'FP 0)	;;frame pointer
	;;pointeur des variables globales
	(set-registre nom 'VGP 1)
	;;drapeaux des comparaisons
	(set-registre nom 'DEQ 0)
	(set-registre nom 'DPP 0)
	(set-registre nom 'DPG 0)
	;;program counter
	(set-registre nom 'PC (- taille-vm 1))
	;;link register
	(set-registre nom 'LR (- taille-vm 1))


);;end make vm


;;trouver registre parmis la liste
;;registres de la VM
(defun get-registre (vm registre)
	(gethash registre (get vm 'registres))
)

;;donner une valeure a un registre dans
;;la liste des regustres de la vm
;;creer ce registre s'il n'existe pas
(defun set-registre(vm registre valeur)
    (setf (gethash registre (get vm 'registres)) valeur)
)

(defun set-property (nom property valeur)
  (setf (get nom property) valeur)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;resolution d'addresses;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-source (mv src)
  (if (atom src)
      (cond
       ((numberp src) (get-adresse-memoire mv src))
       ((is-registre-mv src) (get-registre vm src))
       )
    (if (consp src)
	(cond
	 ((is-constante src) (cadr src)) ;;cadr to remove the CONST keyword
	 ((is-index src) (get-index mv src))	;;if it's an index, call function to resolve index
	 ((is-label src) (get-etiq mv (cadr src))) ;;if it's a label, call function to resolve label
	 ((is-variable-segment src) (get-indir-src mv src))
	 ((is-local src) (get-loc-src mv src))
	 )
      )
    )
  )

(defun get-destination (mv exp)
  (if (atom exp)
      (cond
       ((numberp exp) exp)
       ((is-registre-mv exp) exp)
       )
    (if (consp exp)
	(cond
	 ((is-index exp) exp)
	 ((is-label exp) (get-etiq mv (cadr exp)))
	 ((is-constante exp) (cadr exp))
	 ((is-variable-segment exp) (get-indir-dst mv exp))
	 ((is-local exp) (get-loc-dst mv exp))
	 )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;;fonctions 'helper';;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; si reg est parmis la liste
;; => reg est un registre
(defun is-registre-mv (reg) 
  (member reg (list R0 R1 R2 R3 BP SP FP VGP DEQ DPP DPG PC LR))
  )

;;une constante commence par le symbol :CONST
(defun is-constante (cst)
  (eql (car cst) ':CONST)
  )

;;un label commence pas @
(defun is-label (lbl) 
  (eql (car lbl) :@)
  )

;;acces à une valeure contenue dans une variable segment
;;elle est denotée par un * (LEC p.36)
(defun is-variable-segment (vsg)
	(eql (car vsg) :*)
)

(defun is-local (lcl)
  (eql (car lcl) 'LOCAL)
  )

(defun is-index (idx)
  (numberp (car idx))
  )
(defun get-index (mv idx)
  (get-adresse-memoire mv (+ (car idx) (get-source mv (cadr idx))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;fonctions de la memoire;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-adresse-memoire (mv adress)
  (aref (get mv memoire) adress)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;fonctions des etiquettes;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-label (mv cle)
  (get-hash (get-prop mv :etiq) cle)
  )

(defun set-label (mv cle val)
  (set-hash (get-prop mv :etiq) cle val)
  )