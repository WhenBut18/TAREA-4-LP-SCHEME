#lang scheme
(provide profundidades)

;; La funcion profundidades se encarga de definir 2 funciones auxiliares,
;; profundidades_aux y profundidades_aux_aux, y de inicializar la funcion
;; profundidades_aux pasandole el parametro arbol y el parametro de altura
;; con valor 0.
;;
;; arbol : Lista del valor del nodo y sub listas que representan los nodos hijos.
(define (profundidades arbol)

  ;; La funcion profundidades:_aux se encarga de explorar un nodo y su valor,
  ;; y atraves de una condicion verifica que no se un nodo nulo, y a traves
  ;; de otra condicion checkea que si el valor del nodo corresponde con T,
  ;; si lo es retorna la constitucion de una lista con la altura actual del nodo
  ;; y la funcion profundidades_aux_aux, pasandole a esta una lista que corresponde
  ;; a los hijos/subarboles contenidos en el nodo, y aumentando la altura en 1 unidad.
  ;; Si no corresponde a un nodo T retorna solamente la misma funcion con lo mismos
  ;; parametros que en otra opcion.
  ;;
  ;; arbol : Lista del valor del nodo y sub listas que representan los nodos hijos.        
  ;; altura : Altura actual en la que se encuentra la funcion.
  (define (profundidades_aux arbol altura)
    (cond
      ((null? arbol) '())
      (else
       (cond
         ((equal? (car arbol) 'T) (cons altura (profundidades_aux_aux (cdr arbol) (+ altura 1))))
         (else (profundidades_aux_aux (cdr arbol) (+ altura 1))))))) 

  ;; La funcion profundidades_aux_aux se encarga de administrar la lista de listas de nodos
  ;; primero verificando que esta no este vacia, y luego llamando a profundidades_aux para
  ;; procesar la primera sublista que se encuentra en subarboles, y a traves de recursion
  ;; simple se llama a si misma pasandose por parametro la cola de subarboles.
  ;;
  ;; subarboles : Lista de listas de subarboles.
  ;; altura : Altura actual en la que se encuentra la funcion.
  (define (profundidades_aux_aux subarboles altura)
    (cond
      ((null? subarboles) '())
      (else 
       (append (profundidades_aux (car subarboles) altura) (profundidades_aux_aux (cdr subarboles) altura)))))

  (sort (profundidades_aux arbol 0) <))