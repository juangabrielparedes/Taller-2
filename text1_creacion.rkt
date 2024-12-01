#lang racket/base 

(require rackunit)
(require "interpretadorClase.rkt")
;;(require (prefix-in sllgen: "eopl/eopl.rkt"))

;; Recrear los datatypes explícitamente
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;; Crear la función de parseo
(define scan&parse 
  (sllgen:make-string-parser 
   especificacion-lexica 
   especificacion-gramatical))

;; Función para evaluar una expresión desde una cadena
(define (eval-string str)
  (a-program (scan&parse str)))

;; Pruebas
(test-begin "Pruebas del interpretador de listas"

  ;; Prueba 1: Lista vacía
  (test-equal? "empty" 
               (eval-string "empty") 
               '())

  ;; Prueba 2: Lista con un elemento
  (test-equal? "cons 1 empty" 
               (eval-string "cons(1 empty)") 
               '(1))

  ;; Prueba 3: Lista con varios elementos
  (test-equal? "cons 1 cons 2 cons 3 empty" 
               (eval-string "cons(1 cons(2 cons(3 empty)))") 
               '(1 2 3))

  ;; Prueba 4: Longitud de la lista
  (test-equal? "length cons 1 cons 2 cons 3 empty" 
               (eval-string "length(cons(1 cons(2 cons(3 empty))))") 
               3)

  ;; Prueba 5: Primer elemento de la lista
  (test-equal? "first cons 1 cons 2 cons 3 empty" 
               (eval-string "first(cons(1 cons(2 cons(3 empty))))") 
               1)

  ;; Prueba 6: Resto de la lista
  (test-equal? "rest cons 1 cons 2 cons 3 empty" 
               (eval-string "rest(cons(1 cons(2 cons(3 empty))))") 
               '(2 3))

  ;; Prueba 7: N-ésimo elemento de la lista
  (test-equal? "nth cons 1 cons 2 cons 3 empty 1" 
               (eval-string "nth(cons(1 cons(2 cons(3 empty))) 1)") 
               2)

  ;; Prueba 8: Sentencia cond
  (test-equal? "cond else" 
               (eval-string "cond else ==> 9 end") 
               9)

  ;; Prueba 9: Sentencia cond con condiciones
  (test-equal? "cond con condiciones" 
               (eval-string "let x = 2 in cond x ==> 1 else ==> 9 end") 
               1)
)

(test-end)