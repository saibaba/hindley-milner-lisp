;;;; ti.lisp
(defpackage #:ti
    (:shadow "SUBST")
      (:shadow "TYPE")
        (:use #:cl)
          (:use #:amap)
            (:use #:lisp-unit)
              (:use #:expr)
                (:use #:types)
                  (:export #:tst1))

(in-package #:ti)

(asdf:load-system :lisp-unit)

(defun my-max (a b)
  (if (> a b) a b))

(define-test teqx (assert-equal (type-of (TVar "x")) (type-of (TVar "y"))))

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

(define-test test-my-maxx
                (assert-equal 5 (my-max 2 5))
                   (assert-equal 5 (my-max 5 2))
                      (assert-equal 10 (my-max 10 10))
                         (assert-equal 0 (my-max -5 0)))
