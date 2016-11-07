#!/usr/local/bin/sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload "ti")
(in-package :amap-tests)
(setq *print-failures* t)
(print-errors (lisp-unit:run-tests))
(in-package :ti-tests)
(setq *print-failures* t)
(print-errors (lisp-unit:run-tests))
