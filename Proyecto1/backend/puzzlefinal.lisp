
;; Definicion de los nodos
(defstruct nodo 
    matriz ;;Matriz que representa el estado con una lista de 10 elementos
    padre ;;Apuntador al padre del nodo
    valorg ;;profundidad del nodo
    valorh ;;distancia de manhattan
    valorf ;;valorg + valorh
    operador ;;movimiento hecho en el puzzle
)

(defun hamm (estado meta) ;;distancia de hamming 
    (setq dis 0)
    (setq i 0)
    (loop for x in estado
        do(progn (if (not(eq (nth i meta) x)) (incf dis)) (incf i))   
)dis)

;;Función que calcula la distancia de manhattan
(defun manhattan (estado)
(setq dist 0)
(loop for i from 0 to 8 ;;Hace un ciclo por los 9 valores del estado
    do (progn (setq fila_actual (floor i 3) col_actual (rem i 3));; saca las coordenadas del punto en el que estamos
        (setq fila_meta (floor (nth i estado) 3) col_meta (rem (nth i estado) 3));; saca las coordenadas del punto en el que deberiamos de estar
        (setq dist (+ dist (abs (- fila_meta fila_actual)) (abs (- col_meta col_actual))));; calcula la diferencia de lugares entre los dos
))dist)

(defun revisa-nodo (estado nodos)
(cond 
    ((null nodos) T)
    ((equal estado (nodo-matriz (car nodos))) NIL)
    (t (revisa-nodo estado (cdr nodos)))))

(defun crear-hijo(actual nuevo-estado dist operador)
(setq hijo (make-nodo :matriz nuevo-estado 
            :padre actual
            :valorg valg
            :valorh dist
            :valorf (+ valg dist)
            :operador operador )) hijo)

(defun inv_pos (estado esp_new esp_old);;
	(setf copia (copy-list estado))
	(rotatef (nth esp_new copia) (nth esp_old copia))
	(fill copia esp_new :start 9 :end 10)
	copia                                                
)
;;funcion que checa si se puede mover el 0 hacia arriba
(defun arriba (estado) 
(setq mov '(0 1 2)) ;; lista de posiciones prohibidas para mover hacia arriba
(setq pos (tenth estado)) ;;saca la posicion del 0
(null(member pos mov))) ;;checa si la posicion del 0 no esta en mov


(defun abajo (estado) 
(setq mov '(6 7 8))
(setq pos (tenth estado))
(null(member pos mov))
)

(defun izquierda (estado) 
(setq mov '(0 3 6))
(setq pos (tenth estado))
(null(member pos mov))
)

(defun derecha (estado) 
(setq mov '(2 5 8))
(setq pos (tenth estado))
(null(member pos mov))
)

(defun inserta-abierto (nodo lista)
(cond
    ((null lista) (list nodo))
	((< (nodo-valorf nodo) (nodo-valorf (car lista))) 
	 (cons nodo lista))
	(t (cons (car lista) (inserta-abierto nodo (cdr lista)))))
)

;;Funcion para expandir un nodo y generar los hijos con los operadores permitidos
(defun gen-hijos (actual)
(setq estado (nodo-matriz actual))
(setq valg (+ (nodo-valorg actual) 1)) ;;variable del nuevo nivel de profundidad
(if (arriba estado);;checa si se puede mover hacia arriba
    (progn  
        (setq nuevo-estado (inv_pos estado (-(tenth estado) 3) (tenth estado)));; crea un nuevo estado, moviendo el 0 hacia arriba
        (if (and (revisa-nodo nuevo-estado abierto) (revisa-nodo nuevo-estado cerrado));; revisa que el estado no haya sido expandido anteriormente
        (setq abierto (inserta-abierto (crear-hijo actual nuevo-estado (manhattan nuevo-estado) '^) abierto)))));;agrega el hijo a abierto
(if (abajo estado)
    (progn  
        (setq nuevo-estado (inv_pos estado (+(tenth estado) 3) (tenth estado)))
        (if (and (revisa-nodo nuevo-estado abierto) (revisa-nodo nuevo-estado cerrado))
        (setq abierto (inserta-abierto (crear-hijo actual nuevo-estado (manhattan nuevo-estado) 'v) abierto)))))
(if (derecha estado)
    (progn  
        (setq nuevo-estado (inv_pos estado (+(tenth estado) 1) (tenth estado)))
        (if (and (revisa-nodo nuevo-estado abierto) (revisa-nodo nuevo-estado cerrado))
        (setq abierto (inserta-abierto (crear-hijo actual nuevo-estado (manhattan nuevo-estado) '>) abierto)))))
(if (izquierda estado)
    (progn  
        (setq nuevo-estado (inv_pos estado (-(tenth estado) 1) (tenth estado)))
        (if (and (revisa-nodo nuevo-estado abierto) (revisa-nodo nuevo-estado cerrado))
        (setq abierto (inserta-abierto (crear-hijo actual nuevo-estado (manhattan nuevo-estado) '<) abierto)))))
)

(defun lista-solucion (node)
(cond ((null node) (print "Empieza la solucion") nil)
(t (push (nodo-operador node) solucion)
(lista-solucion (nodo-padre node))
)))

;;método a*
(defun a_star (solucion)
(if (null abierto) (print 'no_hay_solucion);;si abierto está vació, no hay solución
    (progn (setq actual (pop abierto)) (push actual cerrado);; saca el primer nodo de abierto y lo mete a cerrado
    (if (equal (nodo-matriz actual) meta) ;;si ya encontro la solucion, la regresa
        (progn  (lista-solucion actual))
        (progn (gen-hijos actual);;si no, expande el nodo
	    (if (> (length abierto) 500) 
            (setq abierto (reverse (nthcdr 50 (reverse abierto)))));; recorta la lista de abierto, quitando los últimos 50
        (a_star abierto))))))        

;;Método principal

;Lee el txt
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(setq estado-inicial '())

(get-file "input txt" estado-inicial)

(defun 8-puzzle (estado-inicial);;recibe la matriz inicial
(setq meta '(0 1 2 3 4 5 6 7 8 0));;se crea el estado meta
(setq nodo_inicial (make-nodo :matriz estado-inicial ;;crea el primer nodo con el estado inicial
:padre nil
:valorg 0
:valorh (manhattan estado-inicial)
:valorf 0
:operador 0 ))
(setq abierto (list nodo_inicial));;inicializa abierto con el nodo creado
(setq cerrado '());inicializa cerrado
(setq solucion '());inicializa la solución


(a_star abierto) solucion);;llama al método a*

;(8-puzzle '(1 4 2 3 7 5 6 8 0 8)) ;4 movivimentos
;(8-puzzle '(1 4 0 6 3 2 7 8 5 2)) ;8 movimientos
;(8-puzzle '(3 2 8 6 1 7 5 0 4 7)) ;15 movimientos
;(8-puzzle '(7 6 8 0 2 3 4 5 1 3)) ;21 movimientos
;(8-puzzle '(7 2 0 4 1 6 3 8 5 2)) ;16 movimientos
;(8-puzzle '(4 2 7 6 1 3 0 8 5 6)) ;14 movimientos
;(8-puzzle '(0 5 4 1 3 7 6 8 2 0)) ;12 movimientos
;(8-puzzle '(5 4 1 0 8 7 3 6 2 3))  ;17 movimientos
;(8-puzzle '(1 2 3 4 5 6 7 0 8 7)) ;21 movimientos
;(8-puzzle '(6 7 2 0 1 5 3 8 4 3)) ;19 movimientos
;(8-puzzle '(8 3 2 6 0 4 7 1 5 4)) ; 16 movimientos
;(8-puzzle '(2 3 5 6 1 7 0 8 4 6)); 14 movimientos
;(8-puzzle '(2 0 5 8 3 6 7 1 4 1)); 21 movimientos
;(8-puzzle '(0 5 4 1 6 2 7 3 8 0)) ;12 movimientos
;(8-puzzle '(5 7 6 1 0 2 4 3 8 4)) ;22 movimientos
(print solucion)

;Escribe la solucion en el txt
(with-open-file (str "output.txt"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (format str solucion))
