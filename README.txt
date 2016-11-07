I References


http://www.grabmueller.de/martin/www/pub/AlgorithmW.pdf
http://blog.thezerobit.com/2012/07/21/immutable-persistent-data-structures-in-common-lisp.html
https://common-lisp.net/~mmommer/asdf-howto.shtml
https://gergo.erdi.hu/blog/2013-02-17-write_yourself_a_haskell..._in_lisp/
http://www.grabmueller.de/martin/www/pub/AlgorithmW.pdf
https://cseweb.ucsd.edu/classes/wi13/cse230-a/lectures/lec-inference.html


II Setup

Followed http://xach.livejournal.com/278047.html to create "ti".

1) installed quicklisp.lisp 
2) created ~/.config/common-lisp/source-registry.conf.d/projects.conf 
3) ran (quickproject:make-project "ti")

III Usage

Inside "ti" folder:

% pwd
/users/sai/lisp/ti
% rlwrap sbcl
* (ql:quickload "ti")
* (ti:hello-world "sai")
Hello "sai"!
* (exit) 


III Unit tests

Inside "ti" folder:

% rlwrap sbcl
* (ql:quickload "ti")
* (in-package :ti)
* (lisp-unit:run-tests)
* (print-errors *)
* (exit)

For complete tests, run tests.sh
