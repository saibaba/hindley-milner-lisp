(defpackage #:amap
  (:use #:cl)
  (:export
    #:a-put
    #:a-empty
    #:a-get
    #:a-get-expr
    #:a-remove
    #:a-delete
    #:a-map
    #:a-union
    #:a-equal
    #:item))

(in-package #:amap)

(defun a-put (m k v)
  (acons k v m))

(defun a-empty ()
  (pairlis (list) (list)))

(defun a-get (m k &optional dflt &key test)
  (let* (
         (r (cdr (assoc k m :test test))))
    (if (null r) dflt r)))

(defun a-get-expr (m k &optional dflt &key test)
  (let ( (r (cdr (assoc k m :test test))) )
    (if (null r) dflt r)))

(defun a-remove (m key &key test)
  (remove-if #'(lambda (cell) (apply test (list key (car cell)))) m))

(defun a-delete (m l &key test)
  (if (null l) m (a-delete (a-remove m (first l) :test test) (rest l) )))

(defun item (env k &optional dflt &key test) (a-get (envmap env) k dflt :test test))

(defun a-map (f m)
  (map 'list #'(lambda (kv) (cons (apply f (list (car kv))) (apply f (list (cdr kv))) ) )  m))

(member 2 '((1 . 2) (3 . 4)) :test-not #'= :key #'cdr) 

(defun a-add-if-not-exist (m p &key test)
  (if (member (car p) m :test test :key #'car)
    m
    (a-put m (car p) (cdr p))))
   
(defun a-union (m1 m2 &key test)
  (reduce #'(lambda (m kv) (a-add-if-not-exist m kv :test test)) m2 :initial-value m1))

(defun a-equal-given-length (m1 m2 &key test)
  (or (and (null m1) (null m2))
      (let* (
         (p1 (first m1))
         (p2 (first m2))
         (r1 (apply test (list (car p1) (car p2))))
         (r2 (apply test (list (cdr p1) (cdr p2))))
         (r (and r1 r2)))
    (and r (a-equal-given-length (rest m1) (rest m2) :test test)))))

(defun a-equal (m1 m2 &key test)
  (if (not (equal (length m1) (length m2)))
    nil
    (a-equal-given-length m1 m2 :test test)))
