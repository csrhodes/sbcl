;;;; tests of SB-INT:SET-FLOATING-POINT-MODES

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

(defvar *initial-modes* (sb-int:get-floating-point-modes))

(defun denormalized-result (x)
  (declare (type (single-float 0.0 0.5) x))
  (* least-positive-normalized-single-float x))
(defun denormalized-inputs (x y)
  (declare (type (single-float 0.0 (#.least-positive-normalized-single-float)) x y))
  (+ x y))

(with-test (:name (:fast-mode nil :denormalized-result))
  (sb-int:set-floating-point-modes :fast-mode nil)
  (assert (> (denormalized-result 0.5) 0.0)))
(with-test (:name (:fast-mode t :denormalized-result))
  (sb-int:set-floating-point-modes :fast-mode t)
  (assert (= (sb-kernel:single-float-bits (denormalized-result 0.5)) 0)))

(sb-int:set-floating-point-modes :fast-mode nil)

(let ((denorm (* least-positive-normalized-single-float 0.5)))
  (with-test (:name (:fast-mode nil :denormalized-inputs))
    (sb-int:set-floating-point-modes :fast-mode nil)
    (assert (= (denormalized-inputs denorm denorm) least-positive-normalized-single-float)))
  (with-test (:name (:fast-mode t :denormalized-inputs))
    (sb-int:set-floating-point-modes :fast-mode t)
    (assert (= (sb-kernel:single-float-bits (denormalized-inputs denorm denorm)) 0))))
