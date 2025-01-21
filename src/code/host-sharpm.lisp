;;;; Common Lisp reader definitions that need to be on the host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;; This comes early so that DEFSTRUCT can expand into code with type
;; tests for SHARP-EQUAL-WRAPPER.

(defconstant +sharp-equal-marker+ '+sharp-equal-marker+)

(sb-xc:defstruct (sharp-equal-wrapper
                   (:constructor make-sharp-equal-wrapper (label))
                   (:copier nil))
  (label nil :read-only t)
  (value +sharp-equal-marker+))

(define-function-name-syntax sharp-s-constructor (name)
  (let ((tail (cdr name)))
    (when (and (consp tail) (null (cdr tail)))
      (let* ((name (car tail))
             (symbolp (symbolp name)))
        (values symbolp (and symbolp name))))))
