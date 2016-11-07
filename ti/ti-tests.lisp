(defpackage :ti-tests
  (:shadow "SUBST")
  (:shadow "TYPE")
  (:use :cl :lisp-unit :ti :amap :expr :types))

(in-package :ti-tests)

(defmacro mTVar (tv) (list `TVar (string-downcase (symbol-name tv))))

(define-test teq (assert-equal (type-of (TVar "x")) (type-of (TVar "y"))))

(define-test typeenv-store-retrieve (assert-equal "t1"
  (let* ((ev (EVar "x"))
         (tv (TVar "t1"))
         (empty (EmptyTypeEnv))
         (scheme (Scheme (list) tv))
         (tenv (NewTypeEnv empty ev scheme))
         (retr-scheme (a-get (envmap tenv) ev nil :test 'expr-equal))
         (ret-tv (types:type retr-scheme)))
    (name ret-tv))))

(define-test test-tvar-type-equal (assert-true (type-equal (mTVar :t1) (mTVar :t1))))

(define-test test-types-list-equal (assert-true (type-equal (list (mTVar :t1) (mTVar :x1)) (list (mTVar :t1) (mTVar :x1)) )))
(define-test test-types-list-one-item-not-equal (assert-nil (type-equal (list (mTVar :t1)) (list (mTVar :t2)) )))
(define-test test-types-list-not-equal (assert-nil (type-equal (list (mTVar :t1) (mTVar :y1)) (list (mTVar :t1) (mTVar :x1)) )))

(defun tst1()
  (let* (
         (t1 (TVar "t1"))
         (t2 (TVar "t2"))
         (tx (TVar "tx"))
         (s1 (EmptySubst))
         (s2 (NewSubst s1 t1 t2))
         (r1 (apply-subst s2 t1))
         (r2 (apply-subst s2 tx)))
  (list (name r1) (name r2))))

(define-test test-basic-subst (assert-equal (tst1) (list "t2" "tx")))

(defun tsub-arr()
  (let* (
        (t1 (mTVar :t1))
        (t2 (mTVar :t2))
        (t3 (mTVar :t3))
        (tf (TArr t1 (TArr t2 t3)))
        (s (NewSubst (EmptySubst) t3 t1)))
    (apply-subst s tf)))

(define-test test-tarr-subst (assert-true (type-equal (tsub-arr) (TArr (mTVar :t1) (TArr (mTVar :t2) (mTVar :t1))))))

(defun empty-sub-apply-test()
  (let* (
         (t1 (mTVar :t1))
         (t2 (mTVar :t2))
         (t3 (mTVar :t3))
         (tf (TArr t1 (TArr t2 t3)))
         (s (EmptySubst)))
    (apply-subst s tf)))

(define-test test-empty-subst-apply (assert-true (type-equal (empty-sub-apply-test)  (TArr (mTVar :t1) (TArr (mTVar :t2) (mTVar :t3)))  )))

(defun scheme-tst()
  (let* (
         (t1 (TVar "t1"))   ; quantifier bound type v
         (t2 (TVar "t2"))   ; free type v
         (ti (TInt))        ; t2 = TInt
         (tf (TArr t1 (TArr t2 t1)))
         (scheme (Scheme (list t1) tf))
         (s (NewSubst (EmptySubst) t2 ti)))
    (apply-subst s scheme)))

(defun s()
  (Scheme (list (mTVar :t1)) (Tarr (mTVar :t1) (Tarr (TInt) (mTVar :t1)))))
(define-test test-scheme-tst (assert-true (type-equal (s) (scheme-tst) )))

(defun type-env-subst-tst()
  (let* (
         (e1 (EVar "x"))
         (t1 (TVar "t1"))
         (ti (TInt))
         (scheme (Scheme (list) t1))
         (te (NewTypeEnv (EmptyTypeEnv) e1 scheme))
         (s (NewSubst (EmptySubst) t1 ti)))
    (a-get (envmap (apply-subst s te)) e1 nil :test 'expr-equal )))

(define-test test-compose-subst
  (assert-equal t
    (let* (
           (t1 (TVar "t1"))
           (t2 (TVar "t2"))
           (t3 (TVar "t3"))
           (t4 (TVar "t4"))
           (t5 (TVar "t5"))
           (t7 (TVar "t7"))
           (t8 (TVar "t8"))
           (s1 (NewSubst (NewSubst (EmptySubst) t1 t2) t3 t4))
           (s2 (NewSubst (NewSubst (EmptySubst) t5 t1) t7 t8))
           (expected-map (pairlis (list t5 t7 t3 t1) (list t2 t8 t4 t2)))
           (actual-map (envmap (compose-subst s1 s2))))
      (a-equal expected-map actual-map :test 'types:type-equal))))

(defun sx()
  (Scheme (list) (TInt)))
(define-test test-subst-type-env (assert-true (type-equal (sx) (type-env-subst-tst) )))

(defun scheme-free-tvars-tst()
  (let* (
         (t1 (TVar "t1"))
         (t2 (TVar "t2"))
         (tf (TArr t1 (TArr t2 t1)))
         (scheme (Scheme (list t1) tf)))
    (free-type-vars scheme)))

(define-test test-scheme-free-tvars (assert-true (type-equal (list (mTVar :t2)) (scheme-free-tvars-tst))))

(defun tenv-free-tvars-tst()
  (let* (
         (t1   (TVar "t1"))
         (tenv (NewTypeEnv (EmptyTypeEnv) (EVar "x") t1)))
    (free-type-vars tenv)))

(define-test test-tenv-free-tvars
  (assert-true (type-equal (list (mTVar :t1)) (tenv-free-tvars-tst))))

(defun va-tst()
  (let* (
         (f121 (TArr (TVar "t1") (TArr (TVar "t2") (TVar "t1"))) )
         (fbib (TArr (TBool) (TArr (TInt) (TBool))) )
         (s (var-bind f121 fbib)))
    (cdr (car (envmap (var-bind f121 fbib))))))

(define-test test-va-basic (assert-true (type-equal (TArr (TBool) (TArr (TInt) (TBool))) (va-tst))))

(defun get-basic-gen()
  (let* (
         (t1   (TVar "t1"))
         (t2   (TVar "t2"))
         (f121 (TArr t1 (TArr t2 t1)))
         (tenv (NewTypeEnv (EmptyTypeEnv) (EVar "x") t1)))
    (generalize tenv f121)))

(defun gen-basic-tst()
  (let* (
         (t1   (TVar "t1"))
         (t2   (TVar "t2"))
         (f121 (TArr t1 (TArr t2 t1)))
         (tenv (NewTypeEnv (EmptyTypeEnv) (EVar "x") t1)))
    (tvars (generalize tenv f121))))

(define-test test-gen-basic (assert-true (type-equal (list (mTVar :t2)) (gen-basic-tst))))

(defun inst-basic-tst()
  (let* ((gf121 (get-basic-gen)))
         (instantiate gf121)))

(define-test test-inst-basic-arg (assert-true (type-equal (mTVar :t1) (argType (inst-basic-tst)))))
(define-test test-inst-basic-ret (assert-true (type-equal (mTVar :t1) (retType (retType (inst-basic-tst))))))

(defun unbound-var-typeof-test()
  (let* ( (tenv (EmptyTypeEnv)))
    (ti tenv (EVar "x"))))

(define-test test-unbound-var-typeof (assert-error 'variable-unbound-error (unbound-var-typeof-test)))

(defun lit-typeof-test()
  (let* (
         (t1   (TVar "t1"))
         (ei   (ELit (LInt 22)))
         (tenv (NewTypeEnv (EmptyTypeEnv) (EVar "x") t1)))
    (ti tenv ei)))

(define-test test-lit-typeof (assert-true (type-equal (TInt) (second (lit-typeof-test)))))

(defun evar-typeof-test()
  (let* (
         (empty (EmptyTypeEnv))
         (s1   (generalize empty (TVar "t1")))
         (ex   (EVar "x"))
         (tenv (NewTypeEnv empty ex s1)))
    (ti tenv ex)))

(define-test test-evar-typeof (assert-true (type-equal (cur-tvar) (second (evar-typeof-test)))))

(defun scheme-typeof-test()
  (let* (
         (empty (EmptyTypeEnv))
         (t1 (TVar "t1"))
         (t2 (TVar "t2"))
         (tf (TArr t1 (TArr t2 t1)))
         (scheme (Scheme (list t1) tf))
         (ex   (EVar "x"))
         (tenv (NewTypeEnv (NewTypeEnv empty t2 (TInt)) ex scheme)))
    (ti tenv ex)))

(define-test test-scheme-typeof (assert-nil  (envmap (first (scheme-typeof-test)))))

(defun eabs-typeof-test()
  (let* (
         (empty (EmptyTypeEnv))
         (s1   (generalize empty (TVar "t1")))
         (ex   (EVar "x"))
         (f    (EAbs ex ex))
         (x    (ti empty f))
         (test-result (type-equal (argType (second x)) (retType (second x)))))
    test-result))

(define-test test-eabs-typeof (assert-true (eabs-typeof-test)))

(defun eapp-typeof-test()
  (let* (
         (empty-env (EmptyTypeEnv))
         (s1   (generalize empty-env (TVar "t1")))
         (ex   (EVar "x"))
         (f    (EAbs ex ex))
         (i    (ELit (LInt 22)))
         (a    (EApp f i))
         (r    (ti empty-env a)))
    r))

(define-test test-eapp-typeof (assert-true (type-equal (TInt) (second (eapp-typeof-test)))))

; let x = 10 in x
(defun elet-typeof-test()
  (let* (
         (ten (ELit (LInt 10)))
         (x (EVar "x"))
         (empty-env (EmptyTypeEnv))
         (elet (ELet x ten x))
         (r (ti empty-env elet)))
    r))

(define-test test-elet-basic (assert-true (type-equal (TInt) (second (elet-typeof-test)))))

; let id=\x-> x in id
(defun sample1-test()
  (let* (
         (id (EVar "id"))
         (e  (EAbs (EVar "x") (EVar "x")))
         (body (EVar "id"))
         (elet (ELet id e body))
         (empty-env (EmptyTypeEnv))
         (r (ti empty-env elet))
         (ft (second r)))
    (list (argType ft) (retType ft))))

(define-test test-sample1 (assert-equal t
  (let* (
         (r (sample1-test))
         (argT (first r))
         (retT (second r)))
    (type-equal argT retT))))

; let id = \x-> x in (id id)
(defun sample2-test()
  (let* (
         (id (EVar "id"))
         (e  (EAbs (EVar "x") (EVar "x")))
         (body (EApp (EVar "id") (EVar "id")))
         (le (ELet id e body))
         (empty-env (EmptyTypeEnv))
         (r (ti empty-env le))
         (em (envmap (first r)))
         (ft (second r)))
    (list (argType ft) (retType ft))))

(define-test test-sample2 (assert-equal t
  (let* (
         (r (sample2-test))
         (argT (first r))
         (retT (second r)))
    (type-equal argT retT))))


; let id = \x -> (let \y =  x in y )  (id id)

(defun sample3-test()
  (let* (
         (e  (ELet (EVar "id") (EAbs (EVar "x") (ELet (EVar "y") (Evar "x") (EVar "y"))) (EApp (EVar "id") (EVar "id"))))
         (empty-env (EmptyTypeEnv))
         (r (ti empty-env e))
         (ft (second r)))
    (list (argType ft) (retType ft))))
    
(define-test test-sample3 (assert-equal t
  (let* (
         (r (sample3-test))
         (argT (first r))
         (retT (second r)))
    (type-equal argT retT))))

;let id = \x -> x x in id
(defun sample5-test()
  (let* (
         (e  (ELet (EVar "id") (EAbs (EVar "x") (EApp (EVar "x") (EVar "x"))) (EVar "id")))
         (empty-env (EmptyTypeEnv))
         (r (ti empty-env e))
         (ft (second r)))
    (list (argType ft) (retType ft))))


(define-test test-sample5 (assert-error 'recursive-type-error (sample5-test)))

;(define-test test-sample5 (assert-equal t
;  (let* (
;         (r (sample5-test))
;         (argT (first r))
;         (retT (second r)))
;    (type-equal argT retT))))
;
(defun sample6-test()
  (let* (
         (e (EAbs (EVar "m") (ELet (EVar "y") (EVar "m") (ELet (Evar "x") (EApp (EVar "y") (ELit (LBool t))) (EVar "x")))))
         (empty-env (EmptyTypeEnv))
         (r (ti empty-env e))
         (ft (second r)))
    (list (argType (argType ft)) (retType (argType ft)) (retType ft))))

(define-test test-sample6 (assert-equal t
  (let* (
         (r (sample6-test))
         (t1 (first r))
         (t2 (second r))
         (t3 (third r)))
    (and (type-equal (Tbool) t1)
         (type-equal t2 t3)))))

(defun sample7-test()
  (let* (
         (e (EApp (ELit (LInt 2)) (ELit (Lint 2))))
         (empty-env (EmptyTypeEnv))
         (r (ti empty-env e)))
    r))

(define-test test-sample7 (assert-error 'non-unifiable-types-error (sample7-test)))
