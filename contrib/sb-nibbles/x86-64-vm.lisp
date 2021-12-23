(in-package "SB-VM")

(define-vop (sb-nibbles::%check-bound)
  (:translate sb-nibbles::%check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
         (bound :scs (any-reg))
         (index :scs (any-reg)))
  (:arg-types simple-array-unsigned-byte-8 positive-fixnum tagged-num
              (:constant (member 2 4 8 16)))
  (:info offset)
  (:temporary (:sc any-reg) temp)
  (:results (result :scs (any-reg)))
  (:result-types positive-fixnum)
  (:vop-var vop)
  (:generator 5
    (let ((error (generate-error-code vop 'invalid-array-index-error array bound temp)))
      (inst lea temp (ea (fixnumize (1- offset)) nil index))
      (inst cmp temp bound)
      (inst jmp :ae error)
      (move result index))))

(macrolet ((def (name signedp byte-size setterp le-p)
             (flet ((computed-name (signedp byte-size setterp le-p)
                      (let ((symbol-name (format nil "~:[UB~;SB~]~D~:[REF~;SET~]/~:[BE~;LE~]" signedp byte-size setterp le-p)))
                        (find-symbol symbol-name "SB-NIBBLES"))))
               (assert (eq name (computed-name signedp byte-size setterp le-p)))
               (let ((operand-size (ecase byte-size (16 :word) (32 :dword) (64 :qword)))
                     (ref-inst (ecase byte-size
                                 (16 (if le-p
                                         (if signedp 'movsx 'movzx)
                                         'movzx))
                                 (32 (if le-p
                                         (if signedp 'movsxd 'movzxd)
                                         'mov))
                                 (64 'mov)))
                     (result-sc (if signedp 'signed-reg 'unsigned-reg))
                     (result-type (if signedp 'signed-num 'unsigned-num)))
                 (flet ((movx (inst dest src src-size)
                          (cond
                            ((eq inst 'mov) `(inst ,inst ,dest ,src))
                            ((and (member inst '(movzx movzxd)) (eq src-size :dword))
                             `(inst mov :dword ,dest ,src))
                            (t
                             (let ((inst (case inst (movsxd 'movsx) (movzxd 'movzx) (t inst))))
                               `(inst ,inst '(,src-size :qword) ,dest ,src)))))
                        (swap (tn)
                          (ecase byte-size
                            (16 `(inst rol :word ,tn 8))
                            ((32 64) `(inst bswap ,operand-size ,tn)))))
                   `(define-vop (,name)
                      (:translate ,name)
                      (:policy :fast-safe)
                      (:args (vector :scs (descriptor-reg))
                             (index :scs (immediate unsigned-reg))
                             ,@(when setterp `((value* :scs (,result-sc)))))
                      (:arg-types simple-array-unsigned-byte-8 positive-fixnum
                                  ,@(when setterp `(,result-type)))
                      ,@(unless setterp
                          (list
                            `(:results (result :scs (,result-sc)))
                            `(:result-types ,result-type)))
                      ,@(when (and setterp (not le-p))
                          '((:temporary (:sc unsigned-reg :from (:load 0)) temp)))
                      (:generator 5
                        (let* ((base-disp (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
                               (memref
                                 (sc-case index
                                   (immediate (ea (+ (tn-value index) base-disp) vector))
                                   (t (ea base-disp vector index)))))
                          ,@(when (and setterp (not le-p))
                              `((inst mov temp value*)
                                ,(swap 'temp)))
                          ,(if setterp
                               `(inst mov ,operand-size memref ,(if le-p 'value* 'temp))
                               (movx ref-inst 'result 'memref operand-size))
                          ,@(unless setterp
                              (unless le-p
                                `(,(swap 'result)
                                  ,@(when (and (/= byte-size 64) signedp)
                                      `(,(movx 'movsx 'result 'result operand-size))))))))))))))
  (def sb-nibbles::ub16ref/be nil 16 nil nil)
  (def sb-nibbles::ub16ref/le nil 16 nil t)
  (def sb-nibbles::ub16set/be nil 16 t nil)
  (def sb-nibbles::ub16set/le nil 16 t t)

  (def sb-nibbles::ub32ref/be nil 32 nil nil)
  (def sb-nibbles::ub32ref/le nil 32 nil t)
  (def sb-nibbles::ub32set/be nil 32 t nil)
  (def sb-nibbles::ub32set/le nil 32 t t)

  (def sb-nibbles::ub64ref/be nil 64 nil nil)
  (def sb-nibbles::ub64ref/le nil 64 nil t)
  (def sb-nibbles::ub64set/be nil 64 t nil)
  (def sb-nibbles::ub64set/le nil 64 t t)

  (def sb-nibbles::sb16ref/be t 16 nil nil)
  (def sb-nibbles::sb16ref/le t 16 nil t)
  (def sb-nibbles::sb16set/be t 16 t nil)
  (def sb-nibbles::sb16set/le t 16 t t)

  (def sb-nibbles::sb32ref/be t 32 nil nil)
  (def sb-nibbles::sb32ref/le t 32 nil t)
  (def sb-nibbles::sb32set/be t 32 t nil)
  (def sb-nibbles::sb32set/le t 32 t t)

  (def sb-nibbles::sb64ref/be t 64 nil nil)
  (def sb-nibbles::sb64ref/le t 64 nil t)
  (def sb-nibbles::sb64set/be t 64 t nil)
  (def sb-nibbles::sb64set/le t 64 t t))
