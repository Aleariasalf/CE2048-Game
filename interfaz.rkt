#lang racket
(require racket/gui)

(require "CE2048-Game.rkt")

; VARIABLES

; creo el tablero inicial de 4x4
(define tablero (generar-tablero-inicial 4 4))

; puntaje empieza en 0
(define puntaje 0)

; COLORES

; esta función devuelve el color dependiendo del valor de la celda
(define (color-por-valor v)
  (cond
    [(= v 0) (make-object color% 205 193 180)]
    [(= v 2) (make-object color% 238 228 218)]
    [(= v 4) (make-object color% 237 224 200)]
    [(= v 8) (make-object color% 242 177 121)]
    [(= v 16) (make-object color% 245 149 99)]
    [(= v 32) (make-object color% 246 124 95)]
    [(= v 64) (make-object color% 246 94 59)]
    [(= v 128) (make-object color% 237 207 114)]
    [(= v 256) (make-object color% 237 204 97)]
    [(= v 512) (make-object color% 237 200 80)]
    [(= v 1024) (make-object color% 237 197 63)]
    [(= v 2048) (make-object color% 237 194 46)]
    [else (make-object color% 60 58 50)]))

; DIBUJAR

; dibuja todo el tablero en pantalla
(define (dibujar-tablero dc tablero)
  (define size 100) ; tamaño de cada celda

  ; fondo general
  (send dc set-brush (make-object color% 187 173 160) 'solid)
  (send dc draw-rectangle 0 0 450 550)

  ; título del juego
  (send dc set-font (make-object font% 28 'modern 'normal 'bold))
  (send dc set-text-foreground "black")
  (send dc draw-text "2048" 170 10)

  ; mostrar puntaje actual
  (send dc set-font (make-object font% 14 'modern 'normal 'bold))
  (send dc draw-text (string-append "Puntaje: " (number->string puntaje)) 20 50)

  ; recorrer filas y columnas del tablero
  (for ([i (in-range 4)])
    (for ([j (in-range 4)])

      ; obtengo el valor de cada celda
      (define val (list-ref (list-ref tablero i) j))

      ; calculo la posición donde se dibuja
      (define x (+ 20 (* j size)))
      (define y (+ 90 (* i size)))

      ; pequeño brillo si el número es alto
      (when (>= val 128)
        (send dc set-brush (make-object color% 255 255 200) 'solid)
        (send dc draw-rounded-rectangle (- x 3) (- y 3) (+ size 6) (+ size 6) 12))

      ; dibujo la celda con su color
      (send dc set-brush (color-por-valor val) 'solid)
      (send dc draw-rounded-rectangle x y size size 10)

      ; si no es 0, dibujo el número
      (when (not (= val 0))
        (send dc set-font (make-object font% 20 'modern 'normal 'bold))
        (send dc set-text-foreground "black")
        (send dc draw-text
              (number->string val)
              (+ x 30)
              (+ y 35))))))

; VENTANA

; creo la ventana principal
(define frame
  (new frame%
       [label "2048"]
       [width 450]
       [height 550]))

; MANEJO TECLAS

; esta función maneja cuando el usuario presiona una tecla
(define (manejar-tecla key)
  ; dependiendo de la tecla, hago el movimiento
  (define nuevo-tablero
    (cond
      [(equal? key 'left)  (ejecutar-movimiento tablero 'izquierda)]
      [(equal? key 'right) (ejecutar-movimiento tablero 'derecha)]
      [(equal? key 'up)    (ejecutar-movimiento tablero 'arriba)]
      [(equal? key 'down)  (ejecutar-movimiento tablero 'abajo)]
      [else tablero]))

  ; si el tablero cambió, actualizo todo
  (when (not (equal? nuevo-tablero tablero))
    (set! tablero nuevo-tablero)
    ; sumo el puntaje según el tablero nuevo
    (set! puntaje (+ puntaje (calcular-puntaje tablero))))

  ; refresco la pantalla
  (send canvas refresh)

  ; si ganó
  (when (gano? tablero)
    (message-box "GANASTE" "Llegaste a 2048"))

  ; si perdió
  (when (perdio? tablero)
    (message-box "PERDISTE" "No hay más movimientos"))
)

; CANVAS CON TECLAS

; clase personalizada del canvas para capturar teclas
(define mi-canvas%
  (class canvas%
    (inherit refresh)

    ; cuando se presiona una tecla
    (define/override (on-char event)
      (manejar-tecla (send event get-key-code)))

    (super-new)))

; creo el canvas donde se dibuja todo
(define canvas
  (new mi-canvas%
       [parent frame]
       [paint-callback
        (lambda (canvas dc)
          (dibujar-tablero dc tablero))]))

; para que el canvas reciba las teclas
(send canvas focus)

; MOSTRAR

; muestro la ventana
(send frame show #t)