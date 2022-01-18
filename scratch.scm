#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) '())
     (else
      (cons (+ (car tup1) (car tup2))
         (tup+ (cdr tup1) (cdr tup2)))))))

(car 1 1 )

(tup+ '(1 2 3) '(1 2 3))

(define sub1
  (lambda (n)
    (- n 1)))

(define add1
  (lambda (n)
    (+ n 1)))

(define >
  (lambda (n m)
    (cond
     ;;((and (zero? n) (zero? m)) #f)
     ((zero? n) #f)
     ((zero? m) #t)
     (else
      (gt (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else
      (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
     ((zero? m) (zero? n))
     ((zero? n) #f)
     (else (= (sub1 n) (sub1 m))))))

(define =2
  (lambda (n m)
    (cond
     ((> n m) #f)
     ((< n m) #f)
     (else #t))))

(define plus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (plus (add1 n)  (sub1 m))))))

(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (plus n (x n (sub1 m)))))))

(define pow
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else
      (x n (pow n (sub1 m)))))))

(define length
  (lambda (n)
    (cond
     ((null? n) 0)
     (else
      (add1 (length (cdr n)))))))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
      (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define nonums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (nonums (cdr lat)))
     (else (cons (car lat) (nonums (cdr lat)))))))


(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else
       (all-nums (cdr lat))))))

(define egan?
  (lambda (a b)
    (cond
     ((and (number? a) (number? b)) (= a b))
     ((or (number? a) (number? b)) #f)
     (else (eq? a b)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((egan? a (car lat))
      (add1
       (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(define one? (lambda (a) (and (number? a) (= a 1))))

(define rempick2
  (lambda (n lat)
    (cond
     ((one? n) (car lat))
     (else
      (rempick2 (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat)
                 (rempick (sub1 n)
                    (cdr lat)))))))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (rember* a (cdr l)))
       (else (cons (car l)
               (rember* a (cdr l))))))
       (else (cons (rember* a (car l))
              (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons (car l)
              (cons new
                    (insertR* new old (cdr l)))))
       (else (cons (car l)
                   (insertR* new old (cdr l))))))
     (else (cons (insertR* new old
                           (car l))
                 (insertR* new old (cdr l)))))))

(insertR* 'cats 'dogs
          '(I like dogs
              '(dogs rabbits dogs
                     '(another list with dogs))
              dogs are fun. dogs are big))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else
      (occur* a (car l))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
       (cons new (subst* new old (cdr l))))
       (else
        (cons (car l) (subst* new old (cdr l))))))
      (else
       (cons (subst* new old (car l))
             (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new
              (cons (car l)
                        (insertL* new old (cdr l)))))
       (else
        (cons (car l) (insertL* new old (cdr l))))))
     (else
      (cons (insertL* new old (car l))
            (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (cond
       ((eq? a (car l)) #t)
       (else
        (member* a (cdr l)))))
     (else
      (or
       (member* a (car l))
       (member* a (cdr l)))))))


(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((and (null? l1) (atom? (car l2)))
       #f)
      ((null? l1) #f)
      ((and (atom? (car l1))
            (atom? (car l2)))
       (and (egan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((atom? (car l1)) #f)
      ((null? l2) #f)
      ((atom? (car l2)) #f)
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))


(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1))
            (atom? (car l2)))
      (and (egan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1))
          (atom? (car l2)))
      #f)
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))


(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (egan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else
      (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal?  (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))
            )))))

(define rember
  (lambda (s l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((equal? (car l) s) (cdr l))
       (else (cons (car l)
                   (rember s (cdr l)))))))))


(define rember
  (lambda (s l)
    (cond
     ((null? l) (quote ()))
     ((equal? (car l) s) (cdr l))
     (else
      (cons (car l) (rember s (cdr l)))))))

(define numbered?
  (lambda (s)
    (cond
     ((atom? s) (number? s))
     ((eq? (car (cdr s)) (quote +))
      (and (numbered? (car s))
           (numbered? (car (cdr (cdr s))))))
     ((eq? (car (cdr s)) (quote *))
      (and (numbered? (car s))
           (numbered? (car (cdr (cdr s))))))
     ((eq? (car (cdr s)) (quote expt))
      (and (numbered? (car s))
           (numbered? (car (cdr (cdr s))))))
     )))

(define numbered?
  (lambda (s)
    (cond
     ((atom? s) (number? s))
     ((or (eq? (car (cdr s)) (quote +))
          (eq? (car (cdr s)) (quote *))
          (eq? (car (cdr s)) (quote expt)))
      (and (numbered? (car s))
           (numbered? (car (cdr (cdr s)))))))))

(define 1st-sub-exp
  (lambda (aexp)
     (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) (quote +))
      (+ (value (1st-sub-exp nexp))
         (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) (quote *))
      (* (value (1st-sub-exp nexp))
         (value (2nd-sub-exp nexp))))
     (else
      (expt (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))))))

(define zero-test
  (lambda (num)
    (cond
     ((null? num) #t)
     (else #f))))

(define add-one
  (lambda (num)
    (cons (quote ()) num)))

(define sub-one
  (lambda (num)
    (cdr num)))

(define plus
  (lambda (n m)
    (cond
     ((zero-test m) n)
     (else (add-one (plus n (sub-one m)))))))

(lat? '((()) (() ()) (() () ())))


(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (equal? (car lat) a)
               (member? a (cdr lat)))))))

(define set?
  (lambda (lat)
   (cond
    ((null? lat) #t)
    ((member? (car lat) (cdr lat)) #f)
    (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
     (else (cons (car lat) (makeset (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((equal? (car lat) a) (multirember a (cdr lat)))
       (else
        (cons (car lat) (multirember a (cdr lat)))))))))

(define makeset2
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cons (car lat) (makeset2 (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2) (subset? (cdr set1) set2))
     (else #f))))

(define subset-and?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (and (member? (car set1) set2)
           (subset-and? (cdr set1) set2))
      ))))

(subset-and? '(1 2 3) '(1 2 3))

(define eqset-orig?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2)
      (eqset? (cdr set1) (rember (car set1) set2)))
     (else #f))))

(define eqset?
  (lambda (set1 set2)
    (cond
     ((subset? set1 set2)
      (subset? set2 set1))
     (else #f))))

(define eqset?
  (lambda (set1 set2)
     (and (subset? set1 set2)
      (subset? set2 set1))))

(eqset? '(1 2 3 5) '(3 2 1))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     ((member? (car set1) set2) #t)
     (else
       (intersect? (cdr set1) set2)))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
    (else
      (or
        (member? (car set1) set2)
        (intersect? (cdr set1) set2))))))

(intersect? '(stewed tomatoes and macaroni) '(macaroni or cheese))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) (quote ()))
     ((member? (car set1) set2)
      (cons (car set1) (intersect (cdr set1) set2)))
     (else
      (intersect (cdr set1) set2)))))


(intersect '(stewed tomatoes or macaroni) '(macaroni or cheese))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2) (union (cdr set1) set2))
     (else
       (cons (car set1) (union (cdr set1) set2))))))

(union '(stewed tomatoes or macaroni) '(macaroni beer mango))
