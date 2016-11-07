(defpackage #:types
  (:use #:cl #:expr #:amap)
  (:shadow "SUBST")
  (:shadow "TYPE")
  (:export
    #:item
    #:prTypeEnv
    #:TVar
    #:TBool
    #:TInt
    #:TArr
    #:envmap
    #:NewTypeEnv
    #:Scheme
    #:TVar
    #:type-equal
    #:EmptyTypeEnv
    #:argType
    #:retType
    #:tvars
    #:type
    #:var-bind
    #:generalize
    #:instantiate
    #:ti
    #:free-type-vars
    #:subs-as-fun
    #:apply-subst
    #:EmptySubst
    #:compose-subst
    #:cur-tvar
    #:NewSubst
    #:variable-unbound-error
    #:non-unifiable-types-error
    #:recursive-type-error))

(in-package #:types)

(define-condition recursive-type-error (error)
  ((message :initarg :message :reader message)))

(define-condition variable-unbound-error (error)
  ((message :initarg :message :reader message)))

(define-condition non-unifiable-types-error (error)
  ((message :initarg :message :reader message)))

; place holder to defgeneric
(defclass Substitutable() ())
(defgeneric free-type-vars(Substitutable))

(defclass TypeC() (Substitutable))
(defclass TVarC (TypeC)
  ((name :initarg :name :accessor name)))
(defclass TIntC (TypeC)
  ())
(defclass TBoolC (TypeC)
  ())
(defclass TArrC (TypeC)
  ( (argType :initarg :argType :accessor argType) (retType :initarg :retType :accessor retType)))

(defmethod free-type-vars ( (tv TVarC)) (list tv))
(defmethod free-type-vars ( (x TIntC)) (list))
(defmethod free-type-vars ( (x TBoolC)) (list))
(defmethod free-type-vars ( (tarr TArrC)) (union (free-type-vars (argType tarr)) (free-type-vars (retType tarr))))

(defun TVar (n) (make-instance 'TVarC :name n))
(defun TInt () (make-instance 'TIntC))
(defun TBool () (make-instance 'TBoolC))
(defun TArr (argType retType) (make-instance 'TArrC :argType argType :retType retType))

(defgeneric type-equal (TypeC TypeC) )
(defmethod type-equal ( (i TIntC) (j TIntC) ) (eq (type-of i) (type-of j)))
(defmethod type-equal ( (i TBoolC) (j TBoolC) ) (eq (type-of i) (type-of j)))
(defmethod type-equal ( (a TVarC) (b TVarC) ) 
  (equal (name a) (name b)))
(defmethod type-equal ( (a TArrC) (b TArrC) ) 
  (and 
    (type-equal (argType a) (argType b))
    (type-equal (retType a) (retType b))))
(defmethod type-equal ( (t1 TypeC) (t2 TypeC)) (not t) )

(defmethod type-equal ( (l1 list) (l2 list))
  (if (not (equal (length l1) (length l2)))
    nil
    (or (and (null l1) (null l2))
      (and
        (type-equal (first l1) (first l2))
        (type-equal (rest l1) (rest l2))))))

(defclass EnvC () ((envmap :initarg :envmap :accessor envmap)))
(defun NewEnvMap(env k v) (a-put (envmap env) k v))

(defclass SubstC(EnvC) ())
(defgeneric apply-subst(SubstC Substitutable))

(defun Subst(em) (make-instance 'SubstC :envmap em))
(defun EmptySubst() (Subst (a-empty)))
(defun NewSubst (subs typ1 typ2) (Subst (NewEnvMap subs typ1 typ2)))


(defmethod apply-subst ( (s SubstC) ( tv TVarC))
  (a-get (envmap s) tv tv :test 'type-equal ))
(defmethod apply-subst ( (s SubstC) ( tbool TBoolC))
  tbool)
(defmethod apply-subst ( (s SubstC) ( tint TIntC))
  tint)
(defmethod apply-subst ( (s SubstC) ( tarr TArrC))
  (let (
        (newArgType (apply-subst s (argType tarr)))
        (newRetType (apply-subst s (RetType tarr))))
    (TArr newArgType newRetType)))

(defclass TypeEnvC (EnvC) ())
(defun TypeEnv(em) (make-instance 'TypeEnvC :envmap em))
(defun EmptyTypeEnv() (TypeEnv (a-empty)))
(defun NewTypeEnv (env evar scheme) (TypeEnv (NewEnvMap env evar scheme)))

(defmethod free-type-vars ( (te TypeEnvC))
  (reduce #'(lambda (m et) (union m (free-type-vars (cdr et)) :test 'type-equal)) (envmap te) :initial-value (list)))

(defmethod apply-subst ( (s SubstC) (tenv TypeEnvC))   ; TODO testing, particularli if the usage of reduce correct or not?
  (let* (
         (items (envmap tenv))
         (new-items (reduce #'(lambda (m kv) (a-put m (car kv) (apply-subst s (cdr kv))) ) items :initial-value (list))))
    (TypeEnv new-items)))

(defclass SchemeC(Substitutable)
  ((tvars :initarg :tvars :accessor tvars) (type :initarg :type :accessor type)))
(defun Scheme(tvars type) (make-instance 'SchemeC :tvars tvars :type type))

(defmethod type-equal ( (s1 SchemeC)  (s2 SchemeC) )
  (and (type-equal (tvars s1) (tvars s2))
    (type-equal (type s1) (type s2))))


(defmethod free-type-vars ( (scheme SchemeC))
  (set-difference (free-type-vars (type scheme)) (tvars scheme) ))

(defmethod apply-subst ( (s SubstC) (scheme SchemeC) )
  (let* (
         (bound-vars (tvars scheme))
         (sd (Subst (a-delete (envmap s) bound-vars :test 'type-equal)))
         (ntype (apply-subst sd (type scheme))))
    (Scheme bound-vars ntype)))

(defun compose-subst (s1 s2)
  (let* (
         (m1 (envmap s1))
         (m2 (envmap s2))
         (sub-map (a-map #'(lambda (typ) (apply-subst s1 typ)) m2))
         (u-map (a-union sub-map m1 :test 'type-equal)))
    (Subst u-map)))

(defun var-bind(a typ)
  (if (type-equal typ a)
    (EmptySubst)
    (if (member a (free-type-vars typ) :test 'type-equal)
      (error 'recursive-type-error :message "recursive types not allowed")
      (NewSubst (EmptySubst) a typ))))

(defgeneric mgu (TypeC TypeC))
(defmethod mgu ( (i1 TIntC) (i2 TIntC)) (EmptySubst))
(defmethod mgu ( (b1 TBoolC) (b2 TBoolC)) (EmptySubst))
(defmethod mgu ( (tv TVarC) (typ TypeC)) (var-bind tv typ))
(defmethod mgu ( (typ TypeC) (tv TVarC)) (var-bind tv typ))
(defmethod mgu ( (tf1 TArrC) (tf2 TArrC))
  (let* (
         (s1 (mgu (argType tf1) (argType tf2)))
         (s2 (mgu (apply-subst s1 (retType tf1)) (apply-subst s1 (retType tf2))))
         (cs (compose-subst s1 s2)))
    cs))

(defmethod mgu ( (t1 TypeC) (t2 TypeC))
  (error 'non-unifiable-types-error :message "Types do not unify!"))

(defun generalize(typenv typ)
  (let* ( (tvars (set-difference (free-type-vars typ) (free-type-vars typenv) :test 'type-equal)))
    (Scheme tvars typ)))

(setq *gensym-counter* 1)

(defun fresh(pfx)
  (symbol-name (gensym pfx) ))

(defun fresh-tvar()
  (TVar (fresh "a")))

(defun cur-tvar()
  (TVar (concatenate 'string "a" (write-to-string *gensym-counter* ))))

(defun instantiate (scheme)
  (let* (
         (tvs (tvars scheme))
         (m (map 'list #'(lambda (tv) (fresh-tvar)) tvs))
         (p (pairlis tvs m))
         (s (Subst p)))
    (apply-subst s (type scheme))))

(defgeneric ti (TypeEnvC ExpC))

(defmethod ti ((te TypeEnvC) (el ELitC))
  (let* ( (l (lit el)))
    (typecase l
     (LIntC (list (EmptySubst) (TInt)))
     (LBoolC (list (EmptySubst) (TBool))))))

(defmethod ti ((te TypeEnvC) (ev EVarC))
  (let* (
         (scheme (a-get-expr (envmap te) ev nil :test 'expr-equal)))
    (if (null scheme)
      (error 'variable-unbound-error :message (concatenate 'string "unbound variable - " (name ev))) 
      (let* (
             (inst (instantiate scheme)))
        (list (EmptySubst) inst)))))

(defmethod ti ((te TypeEnvC) (ea EAbsC))
  (let* (
         (x (arg ea))
         (e (body ea))
         (tv (fresh-tvar))
         (scheme (Scheme (list) tv))
         (env1 (TypeEnv (a-remove (envmap te) x :test 'expr-equal)))
         (env11 (NewTypeEnv env1 x scheme))
         (l (ti env11 e))
         (s1 (first l))
         (t1 (second l))
         (s (apply-subst s1 tv))
         (typ (TArr s t1)))
    (list s1 typ)))

(defmethod ti ((env TypeEnvC) (ea EAppC))
  (let* (
         (e1 (apply-fn ea))
         (e2 (apply-arg ea))
         (l1 (ti env e1))
         (s1 (first l1))
         (t1 (second l1))
         (l2 (ti (apply-subst s1 env) e2))
         (s2 (first l2))
         (t2 (second l2))
         (tv (fresh-tvar))
         (s3 (mgu (apply-subst s2 t1) (TArr t2 tv)))
         (s  (compose-subst (compose-subst s1 s2) s3))
         (r (apply-subst s3 tv)))
    (list s r)))

(defmethod ti ((env TypeEnvC) (elet ELetC))
  (let* (
         (x (let-var elet))
         (e1 (let-expr elet))
         (e2 (let-body elet))
         (l1 (ti env e1))
         (s1 (first l1))
         (t1 (second l1))
         (env1 (TypeEnv (a-remove (envmap env) x :test 'expr-equal)))
         (envq (apply-subst s1 env))
         (tq (generalize envq t1))
         (env11 (NewTypeEnv env1 x tq))
         (l2 (ti (apply-subst s1 env11) e2))
         (s2 (first l2))
         (t2 (second l2))
         (s (compose-subst s1 s2))
         (r t2))
    (list s r)))

; Print utilities
(defmethod print-object ( (tv TVarC) out) 
  (print-unreadable-object (tv out :type t :identity t)
    (format out "~s" (name tv))))

(defun prTypeEnv(env)
  (format t "~{~a~^, ~}" (envmap env)))
