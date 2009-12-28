;;;; a test file for subclassable structures

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(load "assertoid.lisp")

(defpackage "MOP-31"
  (:use "CL" "SB-MOP"))

(in-package "MOP-31")

(setf sb-ext:*evaluator-mode* :interpret)

(defstruct (struct1 (:subclassable t)) (a 3))
(defclass class1a (struct1 standard-object)
  ((b :initarg :b :accessor b)))
(defvar *class1a* (make-instance 'class1a))
(defclass class1b (standard-object struct1)
  ((b :initarg :b :accessor b)))
(defvar *class1b* (make-instance 'class1b))

(assert (struct1-p *class1a*))
(assert (typep *class1a* 'struct1))
(assert (subtypep 'class1a 'struct1))
(setf (b *class1a*) 4)
(assert (= (b *class1a*) 4))
(assert (= (slot-value *class1a* 'b) 4))
(assert (= (struct1-a *class1a*) 3))
(setf (struct1-a *class1a*) 17)
(assert (= (struct1-a *class1a*) 17))
(assert (= (slot-value *class1a* 'a) 17))

(assert (struct1-p *class1b*))
(assert (typep *class1b* 'struct1))
(assert (subtypep 'class1b 'struct1))
(setf (b *class1b*) 4)
(assert (= (b *class1b*) 4))
(assert (= (slot-value *class1b* 'b) 4))
(assert (= (struct1-a *class1b*) 3))
(setf (struct1-a *class1b*) 17)
(assert (= (struct1-a *class1b*) 17))
(assert (= (slot-value *class1b* 'a) 17))

(defgeneric foo1a (o)
  (:method ((o class1a))
    (with-slots (a b) o
      (list a b))))
(defgeneric foo1b (o)
  (:method ((o class1b))
    (with-slots (a b) o
      (list a b))))
(assert (equalp (foo1a *class1a*) (list 17 4)))
(assert (equalp (foo1b *class1b*) (list 17 4)))

(setf sb-ext:*evaluator-mode* :compile)

(defstruct (struct2 (:subclassable t)) (a 3))
(defclass class2a (struct2 standard-object)
  ((b :initarg :b :accessor b)))
(defvar *class2a* (make-instance 'class2a))
(defclass class2b (struct2 standard-object)
  ((b :initarg :b :accessor b)))
(defvar *class2b* (make-instance 'class2b))

(assert (struct2-p *class2a*))
(assert (typep *class2a* 'struct2))
(assert (subtypep 'class2a 'struct2))
(setf (b *class2a*) 4)
(assert (= (b *class2a*) 4))
(assert (= (slot-value *class2a* 'b) 4))
(assert (= (struct2-a *class2a*) 3))
(setf (struct2-a *class2a*) 17)
(assert (= (struct2-a *class2a*) 17))
(assert (= (slot-value *class2a* 'a) 17))

(assert (struct2-p *class2b*))
(assert (typep *class2b* 'struct2))
(assert (subtypep 'class2b 'struct2))
(setf (b *class2b*) 4)
(assert (= (b *class2b*) 4))
(assert (= (slot-value *class2b* 'b) 4))
(assert (= (struct2-a *class2b*) 3))
(setf (struct2-a *class2b*) 17)
(assert (= (struct2-a *class2b*) 17))
(assert (= (slot-value *class2b* 'a) 17))

(defgeneric foo2a (o)
  (:method ((o class2a))
    (with-slots (a b) o
      (list a b))))
(defgeneric foo2b (o)
  (:method ((o class2b))
    (with-slots (a b) o
      (list a b))))
(assert (equalp (foo2a *class2a*) (list 17 4)))
(assert (equalp (foo2b *class2b*) (list 17 4)))

