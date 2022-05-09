;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bignum) (read-case-sensitive #t) (teachpacks ((lib "cs17.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "cs17.ss" "installed-teachpacks")) #f)))
(require "bignum-operators.rkt")

;; Data Definition:
;; a num list is either
;;   empty
;;   (cons item b) where item is an integer, and b is a num list
;; nothing else is an num list.
;;
;; Example data:
;; num list: list 5 list 5 7 6
;;
;; num will be used as an intermediary data type and is any integer
;;
;; Example data:
;; num: 5, 3 , 4, 9, 0

;; num list:
(define l0 empty)
(define l1 (list 5 4 2 1 4))
(define l2 (list 4 4 3 1 7 8))
(define l3 (list 1 2))
(define l4 (list 5 1 7))

;;-----------------------------------------------------------------------------

;; Let all bignums be written as a list of single digit numbers
;; written in the reverse order
;; The empty list represents 0
;;
;; Examples
;; 67328 -> list 8 2 3 7 6
;; 0 -> empty
;; 7328659 -> list 9 5 6 8 2 3 7

;;-----------------------------------------------------------------------------

;; carry-1: (num list) -> (num list)
;;
;; input:
;; bignum, a list of numbers
;; non-empty inputs only
;;
;; output:
;; a num list where the next element of the list is increased by 1

(define (carry-add bignum)
  (cond
    [(empty? (rest bignum)) (cons 1 (rest bignum))]
    [(cons? (rest bignum))
     (cons (digit-add 1 (first (rest bignum)))
           (rest (rest bignum)))]))

;; Test Cases for carry-1:
(check-expect (carry-add (list 9)) (list 1))
(check-expect (carry-add (list 8 7)) (list 8))
(check-expect (carry-add (list 8 7 6)) (list 8 6))

;;-----------------------------------------------------------------------------

;; bignum+: (num list) * (num list)  -> (num list)

;; input:
;; bignum1 and bignum2, two bignums

;; output:
;; a bignum of the corresponding sum of the two input bignums


;; bignum+ RD:
;;
;; OI: (list 4 2 3) (list 1 2 5)
;;
;; -- RI: (list 2 3) (list 2 5)
;; -- RO: (list 4 8)
;;
;; use digit-add on first two numbers, if this sum adds up to 10 or more,
;; subtract 10 from your output and add one to the next number in the list
;; else, cons this added value onto the list created.
;;
;; OO: (list 5 4 8)

;; OI: (list 4 2 3) (list 6 7 1)
;;
;; -- RI: (list 2 3) (list 7 1)
;; -- RO: (list 0 5)
;;
;; use digit-add on first two numbers, if this sum adds up to 10 or more,
;; subtract 10 from your output and add one to the next number in the list
;; else, cons this added value onto the list created.
;;
;; OO: (list 0 0 5)

(define (bignum+ bignum1 bignum2)
  (cond
    [(and (empty? bignum1) (empty? bignum2)) empty]
    [(and (cons? bignum1) (empty? bignum2)) bignum1]
    [(and (empty? bignum1) (cons? bignum2)) bignum2]
    [(and (cons? bignum1) (cons? bignum2)) 
     (if (<= 10 (digit-add (first bignum1) (first bignum2)))
         (cond
           [(>= (length bignum1) (length bignum2))
            (cons
             (digit-sub (digit-add (first bignum1) (first bignum2)) 10)
             (bignum+
              (rest bignum1)
              (carry-add bignum2)))]
            [(< (length bignum1) (length bignum2))
              (cons
               (digit-sub (digit-add (first bignum1) (first bignum2)) 10)
               (bignum+
                (carry-add bignum1)
                (rest bignum2)))])
         (cons
          (digit-add (first bignum1) (first bignum2))
          (bignum+ (rest bignum1) (rest bignum2))))]))

;; Test Cases for bignum+:
(check-expect (bignum+ l0 l0) empty)
(check-expect (bignum+ l1 l0) l1)
(check-expect (bignum+ l0 l1) l1)
(check-expect (bignum+ l1 l2) (list 9 8 5 2 1 9))
(check-expect (bignum+ l3 l4) (list 6 3 7))
(check-expect (bignum+ (list 1) (list 9 9 9)) (list 0 0 0 1))
(check-expect (bignum+ (list 9 9 9) (list 1)) (list 0 0 0 1))

;;-----------------------------------------------------------------------------

;; mult-help: (num list) * (num list) -> (num list)

;; input:
;; bignum1 and bingum2, two bignums

;; output:
;; a bigum which is the product of the
;; first number in bignum1 and each number in bignum2

;; mult-help RD:
;;
;; OI: (list 1 1)(list 2 2)
;;
;; -- RI: (list 1 1)(list 2)
;; -- RO: (list 2)
;;
;; multiply the next item of list two with
;; the first item of list one and cons the result on
;;
;; OO: (list 2 2)

;; OI: (list 2 2)(list 3 3)
;;
;; -- RI: (list 2 2)(list 3)
;; -- RO: (list 6)
;;
;; multiply the next item of list two with
;; the first item of list one and cons the result on

;; OO: (list 6 6)

(define (mult-help bignum1 bignum2)
  (cond
    [(or (empty? bignum1) (empty? bignum2)) empty]
    [(cons? bignum1)
     (cons
      (digit-mult (first bignum1) (first bignum2))
      (mult-help bignum1 (rest bignum2)))]))

;; Test Cases for mult-help:
(check-expect (mult-help (list 1 1) (list 2 2)) (list 2 2))
(check-expect (mult-help (list 2 2) (list 3 3)) (list 6 6))

;;-----------------------------------------------------------------------------

;; carry-mult: (num list) -> (num list)

;; input:
;; bignum, a list of numbers

;; output:
;; a bignum which is the input bignum, but changed so that all the elements
;; are only one digit long (10^0)
;; and carries the tens (10^1) to the next number in the list

;; carry-mult RD:
;;
;; OI: (list 26 7 2)
;;
;; -- RI: (list 9 2)
;; -- RO: (list 9 2)
;;
;; cons the 10^0 number onto the rest of the list
;; add the 10^1 number onto the next item of the list
;;
;; OO: (list 6 9 2)

;; OI: (list 7 2 26)
;;
;; -- RI: (list 2 26)
;; -- RO: (list 2 6 2)
;;
;; cons the 10^0 number onto the rest of the list
;; add the 10^1 number onto the next item of the list
;;
;; OO: (list 7 2 6 2)

(define (carry-mult bignum)
  (cond
    [(empty? bignum) empty]
    [(cons? bignum)
     (cond
       [(<= 10 (first bignum))
        (cond
          [(cons? (rest bignum))
           (cons
            (digit-rem (first bignum) 10)
            (carry-mult
             (cons
              (digit-add (digit-quo (first bignum) 10)
                         (first(rest bignum)))
              (rest(rest bignum)))))]
          [(empty? (rest bignum))
           (cons
            (digit-rem (first bignum) 10)
            (cons
             (digit-quo (first bignum) 10) empty))])]
       [(> 10 (first bignum))
        (cons (first bignum) (carry-mult (rest bignum)))])]))

;; Test Cases for carry-mult:
(check-expect (carry-mult (list 26 7 2)) (list 6 9 2))
(check-expect (carry-mult (list 7 2 26)) (list 7 2 6 2))


;;-----------------------------------------------------------------------------

;; bignum*: (num list) * (num list) -> (num list)
;;
;; input:
;; bignum1 and bignum2, both lists of numbers
;;
;; output:
;; a singular list of numbers which is the result of multiplying
;; the two bignums together

;; bignum* RD:
;;
;; OI: (list 6 1)(list 2 2)
;;
;; -- RI: (list 1)(list 0 2 2)
;; -- RO: (list 0 2 2)
;;
;; multiply first number of the first list with the first number in the second
;; bignum+ this to: the first number multiplied with the second number in the
;; second with a zero cons-ed into the first position
;; do this for the entirety of list two before moving number two in list one
;; and then iterate through list one
;;
;; OO: (list 2 5 3)

;; OI: (list 1 9)(list 9 1)
;;
;; -- RI: (list 9)(list 0 9 1)
;; -- ROt: (list 0 1 7 1)
;;
;; multiply first number of the first list with the first number in the second
;; bignum+ this to: the first number multiplied with the second number in the
;; second with a zero cons-ed into the first position
;; do this for the entirety of list two before moving number two in list one
;; and then iterate through list one
;;
;; OO: (list 9 2 7 1)

(define (bignum* bignum1 bignum2)
  (cond
    [(or (empty? bignum1) (empty? bignum2)) empty]
    [(and (cons? bignum1) (cons? bignum2))
     (bignum+
      (carry-mult (mult-help bignum1 bignum2))
      (bignum* (rest bignum1) (cons 0 bignum2)))]))


;; Test Cases for bignum*:
(check-expect (bignum* l0 l0) empty)
(check-expect (bignum* l1 l0) empty)
(check-expect (bignum* l0 l1) empty)
(check-expect (bignum* l1 l2) (list 0 8 2 3 8 5 8 3 9 5 3))
(check-expect (bignum* l3 l4) (list 5 1 0 5 1))
