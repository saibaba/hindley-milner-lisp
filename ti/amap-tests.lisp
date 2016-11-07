(defpackage :amap-tests
  (:shadow "SUBST")
  (:shadow "TYPE")
  (:use :cl
        :lisp-unit
        :amap
        :expr
        :types))

(in-package :amap-tests)

(define-test teq (assert-equal (type-of (TVar "x")) (type-of (TVar "y"))))

(define-test typeenv-store-retrievex (assert-equal "t1"
  (let* ((ev (EVar "x"))
         (tv (TVar "t1"))
         (empty (EmptyTypeEnv))
         (scheme (Scheme (list) tv))
         (tenv (NewTypeEnv empty ev scheme))
         (retr-scheme (a-get (envmap tenv) ev nil :test 'expr-equal))
         (ret-tv (types:type retr-scheme)))
    (name ret-tv))))

(defun tst1x()
  (let* (
         (t1 (TVar "t1"))
         (t2 (TVar "t2"))
         (tx (TVar "tx"))
         (s1 (EmptySubst))
         (s2 (NewSubst s1 t1 t2))
         (r1 (apply-subst s2 t1))
         (r2 (apply-subst s2 tx)))
  (list (name r1) (name r2))))

(define-test test-basic-subst (assert-equal (tst1x) (list "t2" "tx")))

(defun sample-keys()
  (map 'list #'symbol-name '(D O S C A B E Z A S)))

(defun sample-values()
  '(9 15 20 2 10 19 1 3 10 20))

(defun m()
  (pairlis (sample-keys) (sample-values)))

(defun s-equal (k1 k2)
  (equalp (string-downcase k1) (string-downcase k2)))

(defun t1()
  (a-get (m) "X" 5 :test #'s-equal ))

(define-test test-absent (assert-equal 5 (a-get (m) "X" 5 :test #'s-equal )))
(define-test test-exists (assert-equal 1 (a-get (m) "E" 5 :test #'s-equal )))
(define-test test-other-case (assert-equal 20 (a-get (m) "s" 5 :test #'s-equal )))


(define-test amap (assert-equal (list (cons 2 11)) (a-map #'(lambda (x) (+ 1 x)) (pairlis (list 1) (list 10)))))

(defun sample-map1 ()
    (pairlis (list 5 3) (list "A" "B")))
(defun sample-map2 ()
    (pairlis (list 5 7) (list "X" "C")))

; http://stackoverflow.com/questions/33306244/eval-undefined-function-nil-in-lisp
(define-test aunion (assert-equal '((7 . "C") (3 . "B") (5 . "A"))  (a-union (sample-map1) (sample-map2) :test #'(lambda (x y) (equal x y) )  )))
