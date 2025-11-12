#lang racket 

;;TALLER2 

;;funciones preeeliminares 
(define squared (lambda (x) (* x x)) )


;;1. Cantidad de elemntos positivos de una lista: 

(define (countpos lst)
    (length (filter(lambda (x) (> x 0)) lst)))

(printf "1. cantidad de positivos: ~a\n" (countpos '(3 -2 7 0 -5 9)))

;;2. Generacion de lista de cuadrados, posteriormente filtrar los pares

(define (evensquares lst)
(filter even?  
    (map squared lst)))

(printf "2. cuadrados pares: ~a\n" (evensquares '(1 2 3 4 5 6 7 8)))



;;3. Calculo de factorial de un numero recursivamente

(define (factorial n)
  (if (<= n 1)     ; Caso base: si n es 0 o 1
      1            ; el factorial es 1
      (* n (factorial (- n 1))))) ; Caso recursivo

(printf "3. factorial de 5: ~a\n" (factorial 5))
 
;;4.  Elevar los numeros de una lista al cubo 
(define (cubo x)
  (*(* x x) x))

(define (lstcb lst)
  (map cubo lst))

  (printf "4. cubo de lista '(2,3,4): ~a \n" (lstcb '(2 3 4)))

;;5. sumar todos los elemtos impares
(define (odd-sum lst)
  (foldl + 0 (filter odd? lst)))
  (printf "5. suma de impares (1 2 3 4 5 6 7): ~a \n" (odd-sum '(1 2 3 4 5 6 7)))


;;6. determinar si una lista contiene numeros negativos
(define (contiene-negativos? lista)
  (not (null? (filter negative? lista))))

  (printf "6. contiene negativos? (5 9 -3 2): ~a \n" (contiene-negativos? '(5 9 -3 2)))

;;7. calcular el acumulador en una lista uso repetido de foldl
(define (suma-acumulada lst)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc) 0 (car acc))) acc))
          '()
          lst)))
(printf "7. lista acumulada ~a \n" (suma-acumulada '(1 2 3 4)))
        
;;8. concatenar cadenas te texto en una lista
(define (concatenar-cadenas lst)
  (foldl string-append ""  (reverse lst)))

(printf "8. concatenar cadenas:  ~a \n" (concatenar-cadenas '("Hola" " " "Mundo"))) 

;;9. generar una lista con los duplicados de los numeros mayores a 5 
(define (duplicar-mayores-a-5 lista)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lista)))
(printf "9. duplicar mayores a 5:  ~a \n" (duplicar-mayores-a-5 '(3 6 8 2 10)))

;;10 invertir el orden de una lista
(define (invertir-lista lst)
  (reverse lst))
(printf "10. invertir orden de lista:  ~a \n" (invertir-lista '(1 2 3 4)))


;;11 una lista con funcion como parametro, la funcion es 
;;el cuadrado de la lista

(define (procesar-lista lst f)
  (map f lst)) 

  (printf "11. procesar (cuadrado) lista: ~a\n" (procesar-lista '(1 2 3 4) squared))


;;12. reto integrador: promedio de numeros mayores a 5 
(define (promedio-mayores-a-5 lst)
  (if (null? (filter (lambda (x) (> x 5)) lst))
      0
      (/ (apply + (filter (lambda (x) (> x 5)) lst))
         (length (filter (lambda (x) (> x 5)) lst)))))

  (printf "12. promedio de los nmeros mayores a 5 : ~a\n" (promedio-mayores-a-5 '(3 8 10 4 9 2 7)))


