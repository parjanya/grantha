(defpackage grantham/tests/main
  (:use :cl
        :grantham
        :rove))
(in-package :grantham/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :grantha)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
