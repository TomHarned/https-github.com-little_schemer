(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(+ 1 2 3)

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

(car '(1 2 3 4))
(car '(1 1))

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
      (> (sub1 n) (sub1 m))))))

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

(intersect '(stewed tomatoes or macaroni) '(macaroni beer mango))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) (quote ()))
     ((member? (car set1) set2)
      (cons (car set1) (intersect (cdr set1) set2)))
     (else
      (intersect (cdr set1) set2)))))


(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else
      (intersect (car l-set)
              (intersectall (cdr l-set)))))))

(intersectall '((1 2 4) (5 4 3) (4 5 6)))

(intersect '(1 2 4)
           (intersectall '((5 4 3) (4 5 6))))

(intersect '(1 2 4)
           (intersect '(5 4 3)
           (intersectall '((4 5 6)))))

(intersect '(1 2 4)
       (intersect '(5 4 3)
                '(4 5 6)))

(intersectall '((1 5 4) '(5 4 3) '(4 5 6)))

(define a-pair?
  (lambda (x)
    (cond
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cdr (cdr x))) #t)
    (else #f))))

(define first
  (lambda (p)
    (cond
     (else (car p)))))

(define second
  (lambda (p)
    (cond
     (else (car (cdr p))))))

(define build
  (lambda (s1 s2)
    (cond
     (else
      (cons s1 (cons s2 (quote ())))))))

(build 1 '(2 3))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(third '(2 3 4 5))


(define firsts
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else
      (cons (first (first rel)) (firsts (cdr rel)))))))

(firsts '((2 3) (4 5)))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))


(fun? '((2 3) (4 5) (2 7)))


(define revrel
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else
      (cons (build (second (first rel))
                   (first (first rel)))
            (revrel (cdr rel)))))))

(revrel '((2 3) (4 5) (2 7)))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel-better
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else
      (cons (revpair (first rel))
            (revrel-better (cdr rel)))))))

(revrel-better '((2 3) (4 5) (2 7)))

(define fullfun?
  (lambda (rel)
    (and (fun? rel) (fun? (revrel rel)))))

(fullfun? '((2 3) (4 5) (6 7)))

(fullfun? '((2 3) (4 5) (6 5)))

(equal? 1 1)

(equal? "1" 1)

(define rember-f
  (lambda (test? a lat)
    (cond
     ((null? lat) (quote ()))
     ((test? a (car lat)) (cdr lat))
     (else (cons
            (car lat)
            (rember-f test? a (cdr lat)))))))

(rember-f = 2 '(2 1 2 3 4 2 2))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

((eq?-c 5) 4)
((eq?-c 5) 5)


;; comment
(define rember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) (quote ()))
       ((test? a (car lat)) (cdr lat))
       (else (cons (car lat)
                   ((rember-f test?) a (cdr lat))))))))

(define rember= (rember-f =))

(rember= 2 '(1 2 2 3))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((equal? (car lat) old)
        (cons new (cons old (cdr lat))))
       (else
        (cons (car lat) (insertL new old (cdr lat)))))))))


(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
      ((null? lat) (quote ()))
      ((test? (car lat) old)
       (cons new lat))
      (else (cons (car lat)
                  ((insertL-f test?) new old
                   (cdr lat))))))))


(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
      ((null? lat) (quote ()))
      ((test? (car lat) old)
       (cons old (cons new (cdr lat))))
      (else (cons (car lat)
                  ((insertR-f test?) new old
                   (cdr lat))))))))

(define insertR-eq (insertR-f =))

(define insertL-lt (insertL-f <))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
      ((null? lat) (quote ()))
      ((test? (car lat) old)
       (cons old (cons new (cdr lat))))
      (else (cons (car lat)
                  ((insertR-f test?) new old
                   (cdr lat))))))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((equal? (car l) old)
        (seq new old (cdr l)))
       (else
        (cons (car l)
              ((insert-g seq)
               new old (cdr l))))))))

(define insertL-2
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define subst
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((equal? (car l) old)
      (cons new (cdr l)))
     (else (cons (car l)
                 (subst new old (cdr l)))))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(seqS 13 3 '(1 2 3 4))

(define subst (insert-g seqS))


(subst 13 3 '(1 2 3 4))

;; Inserts new and old onto a list from left to right
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(seqL 1 2 3)

;; Inserts new and old onto a list from right to left
(define seqR
  (lambda (new old l)
    (cons old (cons old l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((equal? old (car l))
        (seq new old (cdr l)))
       (else
        (cons (car l)
              ((insert-g seq) new old (cdr l))))))))


(seqL 1 4 '(3 4))

(define subst (insert-g seqS))


(cons #f '(1 2 3))


(subst 13 3 '(1 2 3 4))



(define seqrem
  (lambda (new old l)
    l))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(define yyy2
  (lambda (a l)
    ((insert-g seqprem) 1 a l)))

(yyy "sausage" '("pizza" "with" "sausage" "and" "bacon"))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((equal? (operator nexp) (quote +))
      (+ (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp))))
     ((equal? (operator nexp) (quote *))
      (* (value (1st-sub-exp nexp))
         (value (2nd-sub-exp nexp))))
     (else
      (expt (value (operator 1st-sub-exp))
            (value (operator 2nd-sub-exp)))))))

(define atom-to-function
  (lambda (x)
    (cond
     ((equal? x (quote +)) +)
     ((equal? x (quote *)) *)
     (else expt))))

((atom-to-function (operator '(+ 2 5 6))) 3 4 5)

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else ((atom-to-function (operator nexp))
                   (value (1st-sub-exp nexp))
                   (value (2nd-sub-exp nexp)))))))


(define multirember-f
 (lambda (test?)
   (lambda (a lat)
     (cond
      ((null? lat) (quote ()))
      ((test? (car lat) a)
       ((multirember-f test?) a (cdr lat)))
      (else (cons (car lat)
                  ((multirember-f test?) a (cdr lat))))))))


(define multirember-eq? (multirember-f =))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (equal? x a))))

(define eq?-tuna (eq?-c (quote tuna)))

(eq?-tuna 'tuna')

(define multiremberT
   (lambda (worker-fun lat)
     (cond
      ((null? lat) (quote ()))
      ((worker-fun (car lat))
       (multiremberT worker-fun (cdr lat)))
      (else
       (cons (car lat)
             (multiremberT worker-fun (cdr lat)))))))

(multiremberT eq?-tuna '(pizza fries tuna roasbeef))
((multirember-f =) 1 '(1 2 3 2 1))



(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((equal? old (car lat))
      (cons old
            (cons new
                  (multiinsertR new old (cdr lat)))))
     (else
      (cons (car lat)
            (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((equal? old (car lat))
      (cons new
            (cons old
                  (multiinsertL new old (cdr lat)))))
     (else
      (cons (car lat)
            (multiinsertL new old (cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) (quote ()))
     ((equal? oldL (car lat))
      (cons new
            (cons oldL
                  (multiinsertLR new oldL oldR (cdr lat)))))
     ((equal? oldR (car lat))
      (cons oldR
            (cons new
                  (multiinsertLR new oldL oldR (cdr lat)))))
     (else
      (cons (car lat)
            (multiinsertLR new oldL oldR (cdr lat)))))))

;; (multiinsertL 99 2 '(1 2 3 2 1))
;; (multiinsertR 99 2 '(1 2 3 2 1))
;; (multiinsertLR 99 0 2 '(0 1 2 3 2 1 0))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col (quote ())  0 0))
     ((equal? oldL (car lat))
      (multiinsertLR&co new oldL oldR
                        (cdr lat)
                        (lambda (newlat L R)
                        (col (cons new (cons old (cdr newlat))) (add1 L) R))))
     ((equal? oldR (car lat))
      (multiinsertR new oldL oldR
                    (cdr lat)
                    (lambda (newlat L R)
                        (col (cons old (cons new (cdr newlat))) L (add1 R)))))
     (else
      (multiinsertLR&co new oldL oldR
                        (cdr lat)
                        (lambda (newlat L R)
                          (col (cons (car lat) newlat) L R)
                        ))))))

;; 0 (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) col)


;; 1 (multiinsertLR&co 'salty 'fish 'chips
;;                  '(and fish or fish and chips)
;;                  (lambda (newlat L R)
;;                    (col (cons 'chips (cons 'salty '(cdr newlat)) L (add1 R)))))


;; 2 (multiinsertLR&co 'salty 'fish 'chips
;;                  '(fish or fish and chips)
;;                  (lambda (newlat L R)
;;                    (col (cons 'chips (cons 'salty '(cdr newlat)) L (add1 R)))))


(define dumb-even?
  (lambda (n)
    (integer? (/ n 2))))

(define dumb-evens-only*
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     ((atom? l)
      (cond
       ((dumb-even? l) l)
       (else (quote()))))
      (else
       (cons
        (dumb-evens-only* (car l))
        (dumb-evens-only* (cdr l)))))))

(define l-evens
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     ((dumb-even? (car l))
      (cons (car l) (l-evens (cdr l))))
     (else
      (l-evens (cdr l))))))

(define l-evens*
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     ((atom? l)
      (cond
       ((dumb-even? l) l)
       (else (quote ()))))
     ((lat? l)
      (cond
       ((dumb-even? (car l))
        (cons (car l) (l-evens* (cdr l))))
       (else
        (l-evens* (cdr l)))))

(define evens-only*
    (lambda (l)
        (cond
            ((null? l) (quote())))
            ((atom? (car l))
                (cond
                    ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
                    (else
                        (evens-only* (cdr l)))))
            (else
                (cons (evens-only* (car l)) (evens-only* (cdr l))))))

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
(l-evens* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(dumb-evens-only* '(1 2 3 (4 5 6) 7 8 9 100 200))

(dumb-evens-only* (car  '(0 1 2 3 (4 5 6) 7 8 9 100 200)))

(atom? 2)
(dumb-evens-only?* '(3))


(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l)
                (evens-only* (cdr l))))
         (else
           (evens-only* (cdr l)))))
      (else
        (cons (evens-only* (car l))
              (evens-only* (cdr l)))))))
