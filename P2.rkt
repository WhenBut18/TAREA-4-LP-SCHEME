#lang scheme
(provide taylorSenoSimple)
(provide taylorCosenoCola)
;; Se encarga de aplicar la operacion factorial de un numero n
;; utilizando recursion simple.
;;
;; n : Numero a realizar la operacion factorial.
(define (factorial n)
  (cond
    ((= n 0) 1)
    (else (* n (factorial (- n 1))))))

;; La funcion recibe como parametros n y x, siendo estos la cantidad
;; de terminos para la sumatoria de las series de Taylor y el termino
;; a evaluar respectivamente.
;; Primero verifica si n es igual a -1, esto significa que la sumatoria
;; a terminado y por ende debe retornar un 0 para no afectar el valor.
;; De lo contrario se realiza por medio de una recursion simple la suma
;; de la n serie de Taylor con la n-1 serie de Taylor.
;;
;; n : Cantidad de terminos de la serie de Taylor.
;; x : Valor a evaluar la serie de Taylor del seno.
(define (taylorSenoSimple n x)
  (cond
    ((= n -1) 0)
    (else (+ (/ (* (expt -1 n)(expt x (+ (* 2 n) 1))) (factorial (+ (* 2 n) 1))) (taylorSenoSimple (- n 1) x)))))

;; La funcion recibe como parametros n y x, siendo estos la cantidad
;; de terminos para la sumatoria de las series de Taylor y el termino
;; a evaluar respectivamente.
;; Dentro de la funcion se define la funcion auxiliar taylorCosenoCola_aux
;; para poder realizar la recursion de cola.
;; Luego inicializa la funcion taylorCosenoCola_aux con los valores iniciales
;; de n y x, y el parametro sum como 0.
;;
;; n : Cantidad de terminos de la serie de Taylor.
;; x : Valor a evaluar la serie de Taylor del seno.
(define (taylorCosenoCola n x)

  ;; La funcion recibe como parametros n, x y sum, siendo estos la cantidad
  ;; de terminos para la sumatoria de las series de Taylor, el termino
  ;; a evaluar y el valor actual de la sumatoria de Taylor respectivamente.
  ;; Primero verifica si n es igual a -1, esto significa que la sumatoria
  ;; a terminado y por ende debe retornar un 0 para no afectar el valor.
  ;; De lo contrario se realiza por medio de una recursion de cola la suma
  ;; de la n serie de Taylor con la n-1 serie de Taylor llamandose a si misma.
  ;;
  ;; n : Cantidad de terminos de la serie de Taylor.
  ;; x : Valor a evaluar la serie de Taylor del seno.
  ;; sum : Valor de la sumatoria actual de la serie.
  (define (taylorCosenoCola_aux n x sum)
    (cond
      ((= n -1) sum)
      (else (taylorCosenoCola_aux (- n 1) x (+ sum (/ (* (expt -1 n)(expt x (* 2 n))) (factorial (* 2 n))))))))
  (taylorCosenoCola_aux n x 0)
  )