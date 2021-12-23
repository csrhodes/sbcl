(in-package "SB-NIBBLES")

;;; approximately sb-c::vop-existsp, but we don't have that now.
(macrolet ((vop-translation-existsp (name)
             (let ((info (sb-int:info :function :info name)))
               (not (null (sb-c::fun-info-templates info))))))
  ;; using vop-translation-existsp will lead to code deletion, as it evaluates to
  ;; T or NIL at macroexpansion time.
  (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))

  (sb-c:deftransform %check-bound
      ((vector bound offset n-bytes)
       ((simple-array (unsigned-byte 8) (*)) sb-int:index (and fixnum sb-vm:word) (sb-int:constant-arg (member 2 4 8 16)))
       *
       :node node)
    "optimize away bounds check"
    (cond
      ((sb-c:policy node (= sb-c::insert-array-bounds-checks 0)) 'offset)
      ((vop-translation-existsp %check-bound) (sb-c::give-up-ir1-transform))
      ((not (sb-c::constant-lvar-p bound))
       (let ((n-bytes (sb-c::lvar-value n-bytes)))
         `(and (<= 0 offset (- bound ,n-bytes)) offset)))
      (t
       (let* ((bound (sb-c::lvar-value bound))
              (n-bytes (sb-c::lvar-value n-bytes))
              (upper (- bound n-bytes -1)))
         `(the (integer 0 (,upper)) offset)))))

  (macrolet ((def (name signedp byte-size setterp le-p)
               (flet ((computed-name (signedp byte-size setterp le-p)
                        (let ((symbol-name (format nil "~:[UB~;SB~]~D~:[REF~;SET~]/~:[BE~;LE~]" signedp byte-size setterp le-p)))
                          (find-symbol symbol-name "SB-NIBBLES")))
                      (generic-access-form (signedp noctets setterp le-p)
                        (flet ((set-forms (signedp noctets le-p)
                                 `(,@(loop for i from 1 to noctets
                                           for voffset = (if le-p (1- i) (- noctets i))
                                           for byte-pos = (* 8 (1- i))
                                           collect `(setf (aref vector (+ offset ,voffset)) (ldb (byte 8 ,byte-pos) value)))
                                   value))
                               (get-form (signedp noctets le-p)
                                 `(logior
                                   ,@(loop for i from 1 to noctets
                                           for voffset = (if le-p (1- i) (- noctets i))
                                           for shift = (* 8 (1- i))
                                           if (and (= i noctets) signedp)
                                             collect `(let ((octet (aref vector (+ offset ,voffset))))
                                                        (ash (dpb octet (byte 8 0) (- (ash octet -7))) ,shift))
                                           else
                                             collect `(ash (aref vector (+ offset ,voffset)) ,shift)))))
                          (let ((upper (- array-dimension-limit (truncate byte-size 8))))
                            `(locally (declare (type (integer 0 ,upper) offset))
                               ,@(if setterp
                                     (set-forms signedp noctets le-p)
                                     (list (get-form signedp noctets le-p))))))))
                 (assert (eq name (computed-name signedp byte-size setterp le-p)))
                 (let* ((value-type `(,(if signedp 'signed-byte 'unsigned-byte) ,byte-size))
                        (arg-types `((simple-array (unsigned-byte 8) (*)) sb-int:index ,@(when setterp (list value-type)))))
                   `(sb-c:deftransform ,name
                        ((vector offset ,@(when setterp (list 'value)))
                         ((simple-array (unsigned-byte 8) (*)) sb-int:index ,@(when setterp (list value-type))))
                      (cond
                        ((vop-translation-existsp ,name) (sb-c::give-up-ir1-transform))
                        (t ',(generic-access-form signedp (truncate byte-size 8) setterp le-p))))))))
    (def ub16ref/be nil 16 nil nil)
    (def ub16ref/le nil 16 nil t)
    (def ub16set/be nil 16 t nil)
    (def ub16set/le nil 16 t t)

    (def ub32ref/be nil 32 nil nil)
    (def ub32ref/le nil 32 nil t)
    (def ub32set/be nil 32 t nil)
    (def ub32set/le nil 32 t t)

    (def ub64ref/be nil 64 nil nil)
    (def ub64ref/le nil 64 nil t)
    (def ub64set/be nil 64 t nil)
    (def ub64set/le nil 64 t t)

    (def sb16ref/be t 16 nil nil)
    (def sb16ref/le t 16 nil t)
    (def sb16set/be t 16 t nil)
    (def sb16set/le t 16 t t)

    (def sb32ref/be t 32 nil nil)
    (def sb32ref/le t 32 nil t)
    (def sb32set/be t 32 t nil)
    (def sb32set/le t 32 t t)

    (def sb64ref/be t 64 nil nil)
    (def sb64ref/le t 64 nil t)
    (def sb64set/be t 64 t nil)
    (def sb64set/le t 64 t t)))
