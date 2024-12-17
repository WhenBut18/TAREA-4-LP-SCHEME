#lang scheme
(provide buscador)

;; La funcion buscador define otra funcion llamada buscador_aux, que se encarga
;; de la recursividad, como tambien inicializa esta nueva funcion entregandole
;; la lista inicial, el elemento y la posicion 1.
;;
;; lista : Lista a la cual se le desea realizar la busqueda.
;; elemento : Elemento el cual se busca encontrar su posicion.
(define (buscador lista elemento)

  ;; La funcion buscador_aux se encarga de revisar a traves de 2 condiciones la situacion de la
  ;; cabeza de la lista actual, utilizando la recursividad.
  ;; Primero verifica que la lista este vacia, si lo esta devuelve -1, ya que significa que no
  ;; encontro el elemento a buscar.
  ;; La segunda condicion se encarga de verificar que la cabeza de la lista actual es igual al
  ;; elemento que se esta buscando, en caso de que lo sea devuelve la posicion con respecto a
  ;; la lista original.
  ;; Y el else se encarga de volver a llamar a la funcion buscador_aux en caso de fallar todas
  ;; la condiciones de cierre pasandole por parametro la cola de la lista actual, el elemento
  ;; sin modificar y la posicion sumada una unidad.
  ;;
  ;; lista : Lista a la cual se le desea realizar la busqueda.
  ;; elemento : Elemento el cual se busca encontrar su posicion.
  ;; posicion : Posicion actual de lectura con respecto a la lista original.
  (define (buscador_aux lista elemento posicion)
    (cond
      ((null? lista) -1)
      ((equal? (car lista) elemento) posicion)
      (else (buscador_aux (cdr lista) elemento (+ posicion 1)))))
  (buscador_aux lista elemento 1))

            