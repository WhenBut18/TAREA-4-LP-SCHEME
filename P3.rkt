#lang scheme
(provide evaluador)

;; La funcion evaluador dentro de si define tambien la funcion evaluador_aux,
;; la que esta encargada de evaluar la composicion de funciones.
;; Tambien tiene una condicion de cierre cuando no quedan numeros en la lista
;; de numeros.
;; Y de forma recursiva simple va llamando tanto a la funcion evaluador_aux,
;; como a si misma para crear la lista de numeros ya procesados.
;; 
;; funciones : Lista de funciones.
;; numeros : Lista de numeros.
(define (evaluador funciones numeros)

  ;; Esta funcion se encarga evaluar de composicion de funciones de una lista
  ;; de funciones data y el parametro a evaluar, utilizando la recursividad
  ;; de cola para tal proposito.
  ;; Primero se encarga de revisar la condicion de cierre que consiste en verificar
  ;; que la lista de funciones este vacia, de no ser asi significa que quedan
  ;; composiciones que realizar.
  ;; Finalmente utilizando la recursion de cola se llama asi misma otorgando como
  ;; parametro a evaluar la funcion en la cabeza de la lista funciones en el valor
  ;; de parametro actual, y entregando como lista de funciones la cola de la actual.
  ;;
  ;; funciones : Lista de funciones.
  ;; parametro : Parametro actual a evaluar.
  (define (evaluador_aux funciones parametro)
    (cond
      ((null? funciones) parametro)
      (else (evaluador_aux (cdr funciones) ((car funciones) parametro)))))
  (cond
    ((null? numeros) '())
    (else (cons (evaluador_aux funciones (car numeros)) (evaluador (append (cdr funciones) (list (car funciones))) (cdr numeros))))))