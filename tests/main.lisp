(defpackage run/tests/main
  (:use :cl
        :run
        :rove))
(in-package :run/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :run)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
