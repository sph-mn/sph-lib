(import (sph test-old) (sph list) (sph) (sph random-data))

(define (test-random-list inp exp)
  (list-bind inp (len max min)
    (let (r (apply random-list inp))
      (if
        (and (= (length r) len) (every (l (ele) (and (integer? ele) (<= ele max) (>= ele min))) r))
        #t r))))

(define (test-random inp exp)
  (list-bind inp (max min)
    (let (r (n-times-map 300 (l (n) (random max min))))
      (every (l (e) (and (integer? e) (>= e min) (<= e max))) r))))

(define (test-random-3 inp exp)
  (list-bind inp (max min)
    (let*
      ( (r (n-times-map 300 (l (n) (random max min))))
        (r-2 (every (l (ele) (and (integer? ele) (>= ele min) (<= ele max))) r)))
      (if r-2 #t (list r-2 r)))))

(define (test-random-string inp exp) (let (r (apply random-string inp)) (if r #t r)))

(execute-tests-quasiquote (random-list (200 255 0) #t)
  (random (2 1) #t (6 3) #t (3000 300) #t) (random-string () #t))