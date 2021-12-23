(in-package "SB-NIBBLES")

(sb-c:defknown %check-bound
    ((simple-array (unsigned-byte 8) (*))
     sb-int:index
     (and fixnum sb-vm:word)
     (member 2 4 8 16))
  sb-int:index (sb-c:any))

(macrolet ((def (name signedp byte-size setterp le-p)
             (flet ((computed-name (signedp byte-size setterp le-p)
                      (let ((symbol-name (format nil "~:[UB~;SB~]~D~:[REF~;SET~]/~:[BE~;LE~]" signedp byte-size setterp le-p)))
                        (find-symbol symbol-name "SB-NIBBLES"))))
               (assert (eq name (computed-name signedp byte-size setterp le-p)))
               (let* ((value-type `(,(if signedp 'signed-byte 'unsigned-byte) ,byte-size))
                      (arg-types `((simple-array (unsigned-byte 8) (*)) sb-int:index ,@(when setterp (list value-type)))))
                 `(sb-c:defknown ,name ,arg-types ,value-type (sb-c:any))))))
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
  (def sb64set/le t 64 t t))
