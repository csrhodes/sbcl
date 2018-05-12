;;;; cross-compile-time-only replacements for unportable hashing stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defun %sxhash-substring (string &optional (count (length string)))
  ;; KLUDGE: this is the same as %SXHASH-SUBSTRING in
  ;; src/code/target-sxhash.lisp, minus the type declarations and
  ;; scary optimize qualities.
  (macrolet ((set-result (form)
               `(setf result (ldb (byte sb!vm:n-word-bits 0) ,form))))
    (let ((result 0))
      (dotimes (i count)
        (set-result (+ result (char-code (aref string i))))
        (set-result (+ result (ash result 10)))
        (set-result (logxor result (ash result -6))))
      (set-result (+ result (ash result 3)))
      (set-result (logxor result (ash result -11)))
      (set-result (logxor result (ash result 15)))
      (logand result sb!xc:most-positive-fixnum))))

(defun ensure-symbol-hash (symbol)
  ;; this cross-implementation doesn't in fact ensure the hash of
  ;; symbols, but it does compute the same answer as the target will
  ;; eventually ensure.
  (if (null symbol)
      (ash sb!vm::nil-value (- sb!vm:n-fixnum-tag-bits))
      (let ((hash (logand (lognot (%sxhash-substring (symbol-name symbol)))
                          sb!xc:most-positive-fixnum)))
        (if (zerop hash) #x55AA hash))))
