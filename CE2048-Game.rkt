#lang racket

; Crea una fila de N celdas inicializadas en 0
(define (crear-fila n)
  (if (= n 0)
      '()
      (cons 0 (crear-fila (- n 1)))))


; Crea un tablero vacío de M filas y N columnas
(define (crear-tablero m n)
  (if (= m 0)
      '()
      (cons (crear-fila n)
            (crear-tablero (- m 1) n))))



; Verifica si todos los elementos de una fila son 0
(define (fila-vacia? fila)
  (if (null? fila)
      #t
      (if (= (car fila) 0)
          (fila-vacia? (cdr fila))
          #f)))


; Verifica si el tablero está completamente vacío
(define (tablero-vacio? tablero)
  (if (null? tablero)
      #t
      (if (fila-vacia? (car tablero))
          (tablero-vacio? (cdr tablero))
          #f)))


; Obtiene los índices de columna vacíos en una fila
(define (posiciones-vacias-en-fila fila num-fila col)
  (if (null? fila)
      '()
      (if (= (car fila) 0)
          (cons (list num-fila col)
                (posiciones-vacias-en-fila (cdr fila) num-fila (+ col 1)))
          (posiciones-vacias-en-fila (cdr fila) num-fila (+ col 1)))))


; Función auxiliar recursiva para recorrer el tablero
(define (posiciones-vacias-helper tablero num-fila)
  (if (null? tablero)
      '()
      (append
       (posiciones-vacias-en-fila (car tablero) num-fila 0)
       (posiciones-vacias-helper (cdr tablero) (+ num-fila 1)))))


; Obtiene todas las posiciones vacías del tablero
(define (posiciones-vacias tablero)
  (posiciones-vacias-helper tablero 0))

; ============================================================

; Obtiene el valor de una celda en (fila, col)
(define (obtener-celda tablero fila col)
  (list-ref (list-ref tablero fila) col))


; Reemplaza el valor en la posición col de una fila
(define (establecer-en-fila fila col valor)
  (if (= col 0)
      (cons valor (cdr fila))
      (cons (car fila)
            (establecer-en-fila (cdr fila) (- col 1) valor))))


; Devuelve un nuevo tablero con una celda actualizada
(define (establecer-celda tablero fila col valor)
  (if (= fila 0)
      (cons (establecer-en-fila (car tablero) col valor)
            (cdr tablero))
      (cons (car tablero)
            (establecer-celda (cdr tablero) (- fila 1) col valor))))

; ============================================================

; Genera aleatoriamente un valor 2 o 4
(define (generar-valor-nuevo)
  (if (< (random 10) 9)
      2
      4))
 
 
; Selecciona una posición al azar de una lista
(define (posicion-aleatoria posiciones)
  (list-ref posiciones (random (length posiciones))))
 
 
; Función auxiliar que inserta un valor en una
(define (insertar-en-posicion tablero posicion valor)
  (establecer-celda tablero
                    (car posicion)
                    (cadr posicion)
                    valor))
 
 
; Inserta un valor en una posición vacía aleatoria
; del tablero. Usado tanto al inicio del juego como
; después de cada movimiento del jugador.
(define (insertar-valor-aleatorio tablero valor)
  (insertar-en-posicion tablero
                        (posicion-aleatoria (posiciones-vacias tablero))
                        valor))


; Crea el tablero de inicio del juego completo:
; tablero MxN vacío con dos baldosas de valor 2
; en posiciones aleatorias distintas
(define (generar-tablero-inicial m n)
  (insertar-valor-aleatorio
   (insertar-valor-aleatorio (crear-tablero m n) 2)
   2))

; ============================================================
; Elimina todos los ceros de una fila,
; compactando los valores hacia la izquierda
(define (eliminar-ceros fila)
  (if (null? fila)
      '()
      (if (= (car fila) 0)
          (eliminar-ceros (cdr fila))
          (cons (car fila)
                (eliminar-ceros (cdr fila))))))
 

; Combina pares adyacentes iguales en una fila
; ya sin ceros. Cuando dos valores iguales se
; encuentran, se fusionan en su suma y el resultado
; NO puede combinarse de nuevo (se salta con cddr).
(define (combinar fila)
  (if (null? fila)
      '()
      (if (null? (cdr fila))
          fila
          (if (= (car fila) (cadr fila))
              (cons (* 2 (car fila))
                    (combinar (cddr fila)))
              (cons (car fila)
                    (combinar (cdr fila)))))))
 
 
; Agrega ceros al final de una fila hasta que
; alcance el largo original n
(define (rellenar-ceros fila n)
  (if (= (length fila) n)
      fila
      (rellenar-ceros (append fila '(0)) n)))
 
 
; Aplica los 3 pasos del movimiento izquierda
; a una sola fila del tablero
(define (mover-fila-izquierda fila)
  (rellenar-ceros
   (combinar
    (eliminar-ceros fila))
   (length fila)))
 
 
; Aplica el movimiento izquierda a todo el tablero
; procesando cada fila de forma recursiva
(define (mover-izquierda tablero)
  (if (null? tablero)
      '()
      (cons (mover-fila-izquierda (car tablero))
            (mover-izquierda (cdr tablero)))))
 
 
; ============================================================
; Calcula los puntos ganados al combinar una fila.
; Detecta los mismos pares que detectaría combinar,
; por eso también trabaja sobre la fila sin ceros.
(define (calcular-puntaje-fila fila)
  (if (null? fila)
      0
      (if (null? (cdr fila))
          0
          (if (= (car fila) (cadr fila))
              (+ (* 2 (car fila))
                 (calcular-puntaje-fila (cddr fila)))
              (calcular-puntaje-fila (cdr fila))))))
 
 
; Calcula el puntaje total ganado en un movimiento
; sumando los puntos de todas las filas del tablero
(define (calcular-puntaje tablero)
  (if (null? tablero)
      0
      (+ (calcular-puntaje-fila (eliminar-ceros (car tablero)))
         (calcular-puntaje (cdr tablero)))))
 
 

