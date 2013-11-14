;;; test code for mumeml.el
;;; Run with emacs -batch -l mumemo-test.el -f ert-run-tests-batch-and-exit

(require 'ert)

(when noninteractive
    (push "." load-path))

(require 'mumemo)


(defconst mumemo-test-directory "test"
  "Directory where test cases are stay
TODO: Be independent from CWD.")

;;; test universe
(defconst mumemo-test-content-directory (expand-file-name "test/univ0")
  ;; Consider about recover content of directory for each test.
  )

(defconst mumemo-test-universe
  (mumemo-universe-create "univ0" :directory mumemo-test-content-directory))

(defconst mumemo-test-universes
  (list mumemo-test-universe))

;;; the files provided in test universe
(defconst mumemo-test-filename "2013/10/01-0123.md")
(defconst mumemo-test-filename-1 "2013/10/01-0124.md")


(defmacro mumemo-deftest (name arg &rest body)
 ;; TODO: suport DOCSTRING and KEYs.
  `(ert-deftest ,name ,arg
     (let ((mumemo-universes mumemo-test-universes))
       ,@body)))
  
;; TODO: do it on ERT start up.
(mapc #'(lambda (test-file) 
          (load test-file nil nil 'nosuffix))
      (directory-files mumemo-test-directory 'full "test-.*\\.elc?" 'nosort))
