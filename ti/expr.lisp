(defpackage #:expr
  (:use #:cl)
  (:export
    #:name
    #:ELitC
    #:ELit
    #:EVarC
    #:EVar
    #:EMul
    #:EApp
    #:EAppC
    #:apply-fn
    #:apply-arg
    #:EAbs
    #:EAbsC
    #:body #:arg
    #:ELet
    #:ELetC
    #:LBool
    #:LBoolC
    #:LInt
    #:LIntC
    #:lit
    #:let-var
    #:let-expr
    #:let-body
    #:expr-equal
    #:rect-area-expr
    #:circle-peri-expr))

(in-package #:expr)

(defclass LitC() () )
(defclass LIntC (LitC) ((value :accessor value :initarg :value)))
(defclass LBoolC (LitC) ((value :accessor value :initarg :value)))

(defun LInt (n) (make-instance 'LIntC :value n))
(defun LBool (b) (make-instance 'LBoolC :value b))

(defclass ExpC() () )
(defclass EVarC (ExpC) 
  ((name :initarg :name :accessor name)))
(defclass EAbsC (ExpC)
  ((arg :accessor arg :initarg :arg) (body :accessor body :initarg :body)))
(defclass EAppC (ExpC)
  ((fn :accessor apply-fn :initarg :fn) (arg :accessor apply-arg :initarg :arg)))
(defclass EMulC (ExpC)
  ((expr1 :initarg :expr1) (expr2 :initarg :expr2)))
(defclass ELetC (ExpC)
  ((evar :accessor let-var :initarg :evar) (expr :accessor let-expr :initarg :expr) (body :accessor let-body :initarg :body)))
(defclass ELitC (ExpC) 
  ((lit :initarg :lit :accessor lit )))

(defun EVar (n) (make-instance 'EVarC :name n))
(defun EAbs (x e) (make-instance 'EAbsC :arg x :body e))
(defun EApp (fn arg) (make-instance 'EAppC :fn fn :arg arg))
(defun EMul (expr1 expr2) (make-instance 'EMulC :expr1 expr1 :expr2 expr2))
(defun ELet (evar expr-r expr-b) (make-instance 'ELetC :evar evar :expr expr-r :body expr-b))
(defun ELit (lit) (make-instance 'ELitC :lit lit))

(defgeneric expr-equal (ExpC ExpC) )
(defmethod expr-equal ( (e1 EVarC) (e2 EVarC)) (equal (name e1) (name e2)) )
(defmethod expr-equal ( (e1 ExpC) (e2 ExpC)) (not t) )

(defun rect-area-expr ()  (EAbs (EVar "w") (EAbs (EVar "h") (EMul  (EVar "w") (Evar "h") ))) )
(defun circle-peri-expr ()  (EAbs (EVar "r") (EMul (EMul (ELit (LInt 2)) (ELit (LInt 3.14))) (EVar "r"))))


; Print utilities
(defmethod print-object ( (elit ELitC) out) 
  (print-unreadable-object (elit out :type t :identity t)
    (format out "~s" (value (lit elit)))))

