#lang racket
(require racket/gui)

(define FIELD-SIZE 10)
(define SHIP-LIST '(1 1 1 1 2 2 2 3 3 4 4))
(define SUBMITTED (cons #f #f))
(define MOVE-FIRST #t)
(define FINISHED-GAME #f)

(define pair-my-ships (cons '() '()))
(define pair-beaten-cells (cons '() '()))

(define (distance x y) (+ (abs (- (car x) (car y))) (abs (- (cdr x) (cdr y)))))
(define (distance-max x y) (max (abs (- (car x) (car y))) (abs (- (cdr x) (cdr y)))))

(define error-dialog (new dialog% [label "Error"]))
(define msg (new message% [parent error-dialog] [label ""] [auto-resize #t] [vert-margin 10] [horiz-margin 10]))

;===============================================
(define (fill-my-ships)
  
  (define (neib? x y)
    (or (< (distance x y) 2)
        (and (= (distance x y) 2) (not (= (car x) (car y))) (not (= (cdr x) (cdr y))))))
  
  (define (check-one-neib new-ship have-ship)
    (if (null? new-ship)
        #t
        (if (neib? (car new-ship) have-ship) #f (check-one-neib (cdr new-ship) have-ship))))
  
  (define (check-neib new-ship have-ships)
    (if (null? have-ships)
        #t
        (if (check-one-neib new-ship (car have-ships)) (check-neib new-ship (cdr have-ships)) #f)))
  
  (define (random-ship len)
    (define (go x y len direction)
      (if (= len 0)
          '()
          (cons (cons x y) (go (+ x direction) (+ y (- 1 direction)) (- len 1) direction))))
    (define direction (random 2)) ; 0 - horizontal, 1 - vertical
    (define startx (random (- FIELD-SIZE (* len direction))))
    (define starty (random (- FIELD-SIZE (* len (- 1 direction)))))
    (go startx starty len direction))
  
  (define (go L res)
    (define ship (if (null? L) '() (random-ship (car L))))
    (if (null? L)
        res
        (if (check-neib ship res)
            (go (cdr L) (append ship res))
            (go L res))))
  
  (go (reverse (sort SHIP-LIST <)) '()))
;===============================================

(define (pair-sort L)
  (sort L (lambda (x y)
            (or (< (car x) (car y))
                (and (= (car x) (car y))
                     (< (cdr x) (cdr y)))))))
  
(define (neib-side? x y) (<= (distance x y) 1))

(define (next-ship ship rem-ships)
  (if (null? rem-ships)
      (list ship)
      (if (neib-side? ship (car rem-ships))
          (cons ship (next-ship (car rem-ships) (cdr rem-ships)))
          (next-ship ship (cdr rem-ships)))))
    
(define (next-list ship rem-ships)
  (if (null? rem-ships)
      '()
      (if (neib-side? ship (car rem-ships))
          (next-list (car rem-ships) (cdr rem-ships))
          (cons (car rem-ships) (next-list ship (cdr rem-ships))))))

;===============================================
(define (check-my-ships my-ships)
  
  (define (check-numbers my-ships)
    (if (null? my-ships)
        '()
        (cons (- (length my-ships) (length (next-list (car my-ships) (cdr my-ships))))
              (check-numbers (next-list (car my-ships) (cdr my-ships))))))
  
  (define (check-corners my-ships)
    
    (define (neib-corner? x y)
      (and (= (distance x y) 2) (not (= (car x) (car y))) (not (= (cdr x) (cdr y)))))
    
    (define (have-bad position rem-ships)
      (if (null? rem-ships)
          #f
          (if (neib-corner? position (car rem-ships))
              #t
              (have-bad position (cdr rem-ships)))))
    
    (if (null? my-ships)
        #t
        (if (have-bad (car my-ships) (cdr my-ships))
            #f
            (check-corners (cdr my-ships)))))
  
  (and (check-corners my-ships)
       (equal? (sort (check-numbers (pair-sort my-ships)) <) SHIP-LIST)))
;===============================================

;===============================================
(define (refresh-canvas canvas color)
  
  (define C 0.25)
  (define dx (/ (send canvas get-height) FIELD-SIZE))
  (define dy (/ (send canvas get-width) FIELD-SIZE))
  (define player-number (string-ref (send (send canvas get-parent) get-label) 0))
  
  (define (go-beaten beaten-cells my-ships)
    (cond ((not (null? beaten-cells))
           (if (member (car beaten-cells) my-ships)
               (begin
                 (send (send canvas get-dc) set-pen "black" 1 'solid)
                 (send (send canvas get-dc) set-brush "red" 'solid)
                 (send (send canvas get-dc) draw-rounded-rectangle (* dy (+ (cdar beaten-cells) 0.25)) (* dx (+ (caar beaten-cells) 0.25)) (/ dy 2) (/ dx 2))
                 (go-beaten (cdr beaten-cells) my-ships))
               (begin
                 (send (send canvas get-dc) set-pen "black" 2 'solid)
                 (send (send canvas get-dc) draw-line (* dy (+ (cdar beaten-cells) C)) (* dx (+ (caar beaten-cells) C))
                       (* dy (+ (cdar beaten-cells) (- 1 C))) (* dx (+ (caar beaten-cells) (- 1 C))))
                 (send (send canvas get-dc) draw-line (* dy (+ (cdar beaten-cells) (- 1 C))) (* dx (+ (caar beaten-cells) C))
                       (* dy (+ (cdar beaten-cells) C)) (* dx (+ (caar beaten-cells) (- 1 C))))
                 (go-beaten (cdr beaten-cells) my-ships))))))
  
  (define (go-ships L)
    (cond ((not (null? L))
           (begin
             (send (send canvas get-dc) set-pen "black" 1 'solid)
             (send (send canvas get-dc) set-brush "red" 'solid)
             (send (send canvas get-dc) draw-rounded-rectangle (* dy (+ (cdar L) 0.25)) (* dx (+ (caar L) 0.25)) (/ dy 2) (/ dx 2))
             (go-ships (cdr L))))))
  
  (define (go-field x y)
    (cond ((not (= x FIELD-SIZE))
           (if (= y FIELD-SIZE)
               (go-field (+ x 1) 0)
               (begin
                 (send (send canvas get-dc) set-pen "black" 1 'solid)
                 (send (send canvas get-dc) set-brush color 'solid)
                 (send (send canvas get-dc) draw-rectangle (* dy y) (* dx x) dy dx)
                 (go-field x (+ y 1)))))))
  
  (go-field 0 0)
  
  (cond ((or (and (eq? player-number #\1) (not (car SUBMITTED)))
             (and (eq? player-number #\2) (car SUBMITTED) (not (cdr SUBMITTED)))
             (and (equal? SUBMITTED (cons #t #t)) (or (and (eq? player-number #\1) (not MOVE-FIRST)) (and (eq? player-number #\2) MOVE-FIRST))))
         (begin
           (send (send canvas get-dc) set-brush "yellow" 'transparent)
           (send (send canvas get-dc) set-pen "yellow" 10 'solid)
           (send (send canvas get-dc) draw-rectangle 0 0 (send canvas get-width) (send canvas get-height)))))
  
  (cond ((and (eq? player-number #\1) (not (car SUBMITTED))) (go-ships (car pair-my-ships))))
  (cond ((and (eq? player-number #\2) (car SUBMITTED) (not (cdr SUBMITTED))) (go-ships (cdr pair-my-ships))))
  
  (cond ((equal? SUBMITTED (cons #t #t))
         (if (eq? player-number #\1) (go-beaten (car pair-beaten-cells) (car pair-my-ships))
             (go-beaten (cdr pair-beaten-cells) (cdr pair-my-ships))))))
;===============================================

;===============================================
(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      
      (define dx (/ (send this get-height) FIELD-SIZE))
      (define dy (/ (send this get-width) FIELD-SIZE))
      
      (define (between lf rg mid) (and (<= lf mid) (< mid rg)))
      
      (define (get-position i j x y)
        (cond ((not (= i FIELD-SIZE))
               (if (= j FIELD-SIZE)
                   (get-position (+ i 1) 0 x y)
                   (if (and (between (* dx i) (* dx (+ i 1)) x) (between (* dy j) (* dy (+ j 1)) y))
                       (cons i j)
                       (get-position i (+ j 1) x y))))))
      
      (define (xor-my-ships position my-ships)
        (if (member position my-ships)
            (remove position my-ships)
            (cons position my-ships)))
      
      (define (change-my-ships number my-ships)
        (if (= number 1)
            (set! pair-my-ships (cons my-ships (cdr pair-my-ships)))
            (set! pair-my-ships (cons (car pair-my-ships) my-ships))))
            
      (define (add-if-dead my-ships beaten-cells)
        (define (split-ships my-ships)
          (if (null? my-ships)
              '()
              (cons (next-ship (car my-ships) (cdr my-ships))
                    (split-ships (next-list (car my-ships) (cdr my-ships))))))
                    
        (define (go all-ships)
          (define (get-around x y ship beaten-cells)

            (define (neib-to-ship? x y ship)
              (if (null? ship)
                  #f
                  (if (<= (distance-max (cons x y) (car ship)) 1)
                      #t
                      (neib-to-ship? x y (cdr ship)))))
            
            (if (= x FIELD-SIZE)
                '()
                (if (= y FIELD-SIZE)
                    (get-around (+ x 1) 0 ship beaten-cells)
                    (if (and (not (member (cons x y) beaten-cells)) (neib-to-ship? x y ship))
                        (cons (cons x y) (get-around x (+ y 1) ship beaten-cells))
                        (get-around x (+ y 1) ship beaten-cells)))))
        
          (define (sub-list A B)
            (if (null? A)
                #t
                (if (member (car A) B)
                    (sub-list (cdr A) B)
                    #f)))

          (if (null? all-ships)
              '()
              (if (sub-list (car all-ships) beaten-cells)
                  (append (get-around 0 0 (car all-ships) beaten-cells) (go (cdr all-ships)))
                  (go (cdr all-ships)))))
                  
        (go (split-ships (pair-sort my-ships))))
      
      (define (add-beaten position player-number beaten-cells my-ships)
        (if (member position beaten-cells)
            (begin
                (send msg set-label "You have already shot this cell")
                (send error-dialog show #t))
            (begin
              (if (eq? player-number #\1)
                  (set! pair-beaten-cells (cons (cons position (car pair-beaten-cells)) (cdr pair-beaten-cells)))
                  (set! pair-beaten-cells (cons (car pair-beaten-cells) (cons position (cdr pair-beaten-cells)))))
                  
              (if (not (member position my-ships))
                  (set! MOVE-FIRST (not MOVE-FIRST))
                  (if (eq? player-number #\1)
                      (set! pair-beaten-cells (cons (append (add-if-dead my-ships (car pair-beaten-cells)) (car pair-beaten-cells)) (cdr pair-beaten-cells)))
                      (set! pair-beaten-cells (cons (car pair-beaten-cells) (append (add-if-dead my-ships (cdr pair-beaten-cells)) (cdr pair-beaten-cells)))))))))
      
      (define (finish-game my-ships beaten-cells)
        (if (null? my-ships)
            #t
            (if (not (member (car my-ships) beaten-cells))
                #f
                (finish-game (cdr my-ships) beaten-cells))))
      
      (cond ((and (not FINISHED-GAME) (eq? (send event get-event-type) 'left-down))
             (begin
               (define position (get-position 0 0 (send event get-y) (send event get-x)))
               (define player-number (string-ref (send (send this get-parent) get-label) 0))
               
               (cond ((and (eq? player-number #\1) (not (car SUBMITTED))) (change-my-ships 1 (xor-my-ships position (car pair-my-ships)))))
               (cond ((and (eq? player-number #\2) (car SUBMITTED) (not (cdr SUBMITTED))) (change-my-ships 2 (xor-my-ships position (cdr pair-my-ships)))))
               
               (cond ((and (equal? SUBMITTED (cons #t #t)) (eq? player-number #\1) (not MOVE-FIRST)) (add-beaten position player-number (car pair-beaten-cells) (car pair-my-ships))))
               (cond ((and (equal? SUBMITTED (cons #t #t)) (eq? player-number #\2) MOVE-FIRST) (add-beaten position player-number (cdr pair-beaten-cells) (cdr pair-my-ships))))
               
               (send (car field-canvas) refresh-now)
               (send (cdr field-canvas) refresh-now)
               
               (cond ((equal? SUBMITTED (cons #t #t))
                        (begin
                        (define finish-game-dialog (new dialog% [label "Congratulations!!!"]))
                        (define finish-game-msg (new message% [parent finish-game-dialog] [label "1 player wins!!!"] [vert-margin 10] [horiz-margin 20]))
                        
                        (cond ((finish-game (cdr pair-my-ships) (cdr pair-beaten-cells))
                               (begin
                                 (set! FINISHED-GAME #t)
                                 (send finish-game-dialog show #t))))
                        
                        (cond ((finish-game (car pair-my-ships) (car pair-beaten-cells))
                               (begin
                                 (set! FINISHED-GAME #t)
                                 (send finish-game-msg set-label "2 player wins!!!")
                                 (send finish-game-dialog show #t)))))))))))
    
    (super-new)))
;===============================================

(define (new-field-canvas Parent color)
  (new my-canvas% [parent Parent]
       [paint-callback (lambda (canvas dc) (refresh-canvas canvas color))]
       [min-width 300] [min-height 300]))

(define (new-clear-button Parent)
  (new button% [label "Clear"]
       [parent Parent]
       [callback (lambda (button event)
                   (begin
                     (define player-number (string-ref (send (send Parent get-parent) get-label) 0))
                     (cond ((and (eq? player-number #\1) (not (car SUBMITTED))) (set! pair-my-ships (cons '() (cdr pair-my-ships)))))
                     (cond ((and (eq? player-number #\2) (car SUBMITTED) (not (cdr SUBMITTED))) (set! pair-my-ships (cons (car pair-my-ships) '()))))
                     (send (car field-canvas) refresh-now)
                     (send (cdr field-canvas) refresh-now)))]))

(define (new-fill-button Parent)
  (new button% [label "Fill Random"]
       [parent Parent]
       [callback (lambda (button event)
                   (begin
                     (define player-number (string-ref (send (send Parent get-parent) get-label) 0))
                     (cond ((and (eq? player-number #\1) (not (car SUBMITTED))) (set! pair-my-ships (cons (fill-my-ships) (cdr pair-my-ships)))))
                     (cond ((and (eq? player-number #\2) (car SUBMITTED) (not (cdr SUBMITTED))) (set! pair-my-ships (cons (car pair-my-ships) (fill-my-ships)))))
                     (send (car field-canvas) refresh-now)
                     (send (cdr field-canvas) refresh-now)))]))

(define (new-submit-button Parent)
  (new button% [label "Submit"]
       [parent Parent]
       [callback (lambda (button event)
                   (begin
                     (define player-number (string-ref (send (send Parent get-parent) get-label) 0))
                     (cond ((and (eq? player-number #\1) (not (car SUBMITTED)))
                            (if (check-my-ships (car pair-my-ships))
                                (set! SUBMITTED (cons #t #f))
                                (begin
                                  (send msg set-label "Check the number of ships and their locations")
                                  (send error-dialog show #t)))))
                                
                     (cond ((and (eq? player-number #\2) (car SUBMITTED) (not (cdr SUBMITTED)))
                            (if (check-my-ships (cdr pair-my-ships))
                                (set! SUBMITTED (cons #t #t))
                                (begin
                                  (send msg set-label "Check the number of ships and their locations")
                                  (send error-dialog show #t)))))
                                  
                     (send (car field-canvas) refresh-now)
                     (send (cdr field-canvas) refresh-now)))]))

(define frame (new frame% [label "Battleship"] [width 1000] [height 500]))

(define new-game-button
  (new button% [label "New game"]
       [parent frame]
       [callback (lambda (button event)
                   (begin
                     (set! SUBMITTED (cons #f #f))
                     (set! MOVE-FIRST #t)
                     (set! FINISHED-GAME #f)
                     (set! pair-my-ships (cons '() '()))
                     (set! pair-beaten-cells (cons '() '()))
                     (send (car field-canvas) refresh-now)
                     (send (cdr field-canvas) refresh-now)))]))

(define playground-panel (new horizontal-panel% [parent frame]))
(define (new-playground Label Parent) (new group-box-panel% [label Label] [parent Parent]))
(define (new-button-panel Parent) (new horizontal-panel% [parent Parent] [alignment '(center center)] [stretchable-height #f]))

(define playgrounds (cons (new-playground "1 player" playground-panel) (new-playground "2 player" playground-panel)))
(define field-canvas (cons (new-field-canvas (car playgrounds) "LightSkyBlue")
                           (new-field-canvas (cdr playgrounds) "DodgerBlue")))
(define button-panels (cons (new-button-panel (car playgrounds)) (new-button-panel (cdr playgrounds))))
(define clear-buttons (cons (new-clear-button (car button-panels)) (new-clear-button (cdr button-panels))))
(define fill-buttons (cons (new-fill-button (car button-panels)) (new-fill-button (cdr button-panels))))
(define submit-buttons (cons (new-submit-button (car button-panels)) (new-submit-button (cdr button-panels))))

(send frame show #t)
