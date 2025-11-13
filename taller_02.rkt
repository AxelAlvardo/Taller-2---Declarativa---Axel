#lang racket

; ============================
; Ejercicio 1 – Contar elementos positivos en una lista
; ============================
(define (ej1-contar-positivos lista)
  (length (filter (lambda (x) (> x 0)) lista)))

; Prueba del Ejercicio 1
(define datos-ej1 '(3 -2 7 0 -5 9))
(display "Ejercicio 1 -> Cantidad de elementos positivos: ")
(display (ej1-contar-positivos datos-ej1))
(newline)
(newline)

; ============================
; Ejercicio 2 – Generar lista de cuadrados pares
; ============================
(define (ej2-cuadrados-pares lista)
  (map (lambda (x) (* x x))
       (filter even? lista)))

; Prueba del Ejercicio 2
(define datos-ej2 '(1 2 3 4 5 6 7 8))
(display "Ejercicio 2 -> Lista de cuadrados pares: ")
(display (ej2-cuadrados-pares datos-ej2))
(newline)
(newline)


; ============================
; Ejercicio 3 – Calcular el factorial de un número
; ============================
(define (ej3-factorial n)
  (if (<= n 1)
      1
      (* n (ej3-factorial (- n 1)))))

; Prueba del Ejercicio 3
(define n-ej3 5)
(display "Ejercicio 3 -> Factorial de ")
(display n-ej3)
(display ": ")
(display (ej3-factorial n-ej3))
(newline)
(newline)

; ============================
; Ejercicio 4 – Elevar cada número al cubo
; ============================
(define (ej4-cubos lista)
  (map (lambda (x) (* x x x)) lista))

; Prueba del Ejercicio 4
(define datos-ej4 '(2 3 4))
(display "Ejercicio 4 -> Lista elevada al cubo: ")
(display (ej4-cubos datos-ej4))
(newline)
(newline)

; ============================
; Ejercicio 5 – Sumar todos los elementos impares
; ============================
(define (ej5-suma-impares lista)
  (foldl + 0 (filter odd? lista)))

; Prueba del Ejercicio 5
(define datos-ej5 '(1 2 3 4 5 6 7))
(display "Ejercicio 5 -> Suma de elementos impares: ")
(display (ej5-suma-impares datos-ej5))
(newline)
(newline)

; ============================
; Ejercicio 6 – Determinar si una lista contiene números negativos
; ============================
(define (ej6-contiene-negativos lista)
  (ormap (lambda (x) (< x 0)) lista))

; Prueba del Ejercicio 6
(define datos-ej6 '(5 9 -3 2))
(display "Ejercicio 6 -> ¿Contiene números negativos?: ")
(display (ej6-contiene-negativos datos-ej6))
(newline)
(newline)

; ============================
; Ejercicio 7 – Calcular la suma acumulada de una lista
; ============================
(define (ej7-suma-acumulada lista)
  (reverse
   (foldl
    (lambda (x acc)
      (cons (+ x (if (null? acc) 0 (car acc))) acc))
    '()
    lista)))

; Prueba del Ejercicio 7
(define datos-ej7 '(1 2 3 4))
(display "Ejercicio 7 -> Suma acumulada: ")
(display (ej7-suma-acumulada datos-ej7))
(newline)
(newline)

; ============================
; Ejercicio 8 – Concatenar cadenas de texto en una lista
; ============================
(define (ej8-concatenar-cadenas lista)
  (foldl string-append "" lista))

; Prueba del Ejercicio 8
(define datos-ej8 '("Hola" " " "Mundo"))
(display "Ejercicio 8 -> Concatenación de cadenas: ")
(display (ej8-concatenar-cadenas datos-ej8))
(newline)
(newline)

; ============================
; Ejercicio 9 – Generar lista con el doble de los números mayores que 5
; ============================
(define (ej9-dobles-mayores5 lista)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lista)))

; Prueba del Ejercicio 9
(define datos-ej9 '(3 6 8 2 10))
(display "Ejercicio 9 -> Dobles de números mayores que 5: ")
(display (ej9-dobles-mayores5 datos-ej9))
(newline)
(newline)


; ============================
; Ejercicio 10 – Invertir el orden de una lista
; ============================
(define (ej10-invertir-lista lista)
  (foldl (lambda (x acc) (cons x acc)) '() lista))

; Prueba del Ejercicio 10
(define datos-ej10 '(1 2 3 4))
(display "Ejercicio 10 -> Lista invertida: ")
(display (ej10-invertir-lista datos-ej10))
(newline)
(newline)

; ============================
; Ejercicio 11 – Crear una función que reciba una función como parámetro
; ============================
(define (ej11-aplicar-funcion f lista)
  (map f lista))

; Definimos una función ejemplo: cuadrado
(define (cuadrado x) (* x x))

; Prueba del Ejercicio 11
(define datos-ej11 '(1 2 3 4))
(display "Ejercicio 11 -> Aplicar función cuadrado a la lista: ")
(display (ej11-aplicar-funcion cuadrado datos-ej11))
(newline)
(newline)

; ============================
; Ejercicio 12 – Calcular el promedio de los números mayores a 5 (versión decimal)
; ============================
(define (ej12-promedio-mayores5 lista)
  (let* ([mayores (filter (lambda (x) (> x 5)) lista)]
         [suma (foldl + 0 mayores)]
         [cantidad (length mayores)])
    (if (zero? cantidad)
        0
        (exact->inexact (/ suma cantidad)))))

; Prueba del Ejercicio 12
(define datos-ej12 '(3 8 10 4 9 2 7))
(display "Ejercicio 12 -> Promedio de números mayores a 5: ")
(display (ej12-promedio-mayores5 datos-ej12))
(newline)
(newline)