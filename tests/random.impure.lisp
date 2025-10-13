;;;; tests of RANDOM with possible side-effects

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;
;;;; This software is in the public domain and is provided with
;;;; absoluely no warranty. See the COPYING and CREDITS files for
;;;; more information.


(defvar *seed-string*
  (format nil "~A ~A ~A ~A" (machine-version) (software-version) (lisp-implementation-version) (sb-int:format-universal-time nil (get-universal-time))))

(test-util::log-msg/non-pretty *trace-output* "*SEED-STRING* = ~S" *seed-string*)

(let ((*random-state* (sb-ext:seed-random-state (sb-ext:string-to-octets *seed-string* :external-format :utf-8))))
  (multiple-value-bind (zeros epsilons odds)
      (loop repeat (expt 2 28)
            for float = (random 1.0)
            for bits = (sb-kernel:single-float-bits float)
            if (= float 0.0) count t into zeros
            if (< float single-float-epsilon) count t into epsilons
            count (logbitp 0 bits) into odds
            finally (return (values zeros epsilons odds)))
    ;; p(0.0) should be 2^-125 (since least-positive-single-float is
    ;; 2^-124).  Therefore E(zeros) is 2^-95 with variance 2^-95, so
    ;; we are 2^47 standard deviations away from finding a single
    ;; zero.
    (with-test (:name (random single-float :quality zerop))
      (assert (= zeros 0)))
    ;; we can't test for the quality of epsilon count with false
    ;; positive probability better than two parts in a million while
    ;; catching errors by a factors of two.  epsilons should be
    ;; distributed as a Poisson distribution with parameter 16, whose
    ;; millionth extremiles are 1 and 38.  We will suffer many false
    ;; negatives for a Poisson distribution with parameter 32, getting
    ;; true positives only ~13% of the time.
    (with-test (:name (random single-float :quality :epsilon))
      (assert (<= 1 epsilons 38)))
    ;; the low bit of the mantissa should be set with probability
    ;; one-half, so E(odds) is 2^27 and Var(odds) is 2^26;
    ;; StdDev(odds) is 2^13, so sixteen standard deviations is 2^17
    (with-test (:name (random single-float :quality oddp))
      (assert (<= (- (expt 2 27) (expt 2 17)) odds (+ (expt 2 27) (expt 2 17)))))))
