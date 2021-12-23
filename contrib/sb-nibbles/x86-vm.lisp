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
      (inst lea temp (make-ea :dword :index index :disp (fixnumize (1- offset))))
      (inst cmp temp bound)
      (inst jmp :ae error)
      (move result index))))

(macrolet ((def/16 (name signedp setterp le-p)
             (flet ((computed-name (signedp setterp le-p)
                      (let ((symbol-name (format nil "~:[UB~;SB~]16~:[REF~;SET~]/~:[BE~;LE~]" signedp setterp le-p)))
                        (find-symbol symbol-name "SB-NIBBLES"))))
               (assert (eq name (computed-name signedp setterp le-p)))
               (let ((result-sc (if signedp 'signed-reg 'unsigned-reg))
                     (result-type (if signedp 'signed-num 'unsigned-num)))
                 `(define-vop (,name)
                    (:translate ,name)
                    (:policy :fast-safe)
                    (:args (vector :scs (descriptor-reg))
                           (index :scs (unsigned-reg))
                           ,@(when setterp `((value :scs (,result-sc)))))
                    (:arg-types simple-array-unsigned-byte-8 positive-fixnum
                                ,@(when setterp `(,result-type)))
                    ,@(unless setterp
                        (list
                         `(:results (result :scs (,result-sc)))
                         `(:result-types ,result-type)))
                    ,@(when (or setterp (not le-p))
                        `((:temporary (:sc unsigned-reg :offset eax-offset
                                       :from ,(if setterp '(:load 0) '(:argument 2))
                                       ,@(unless setterp '(:to (:result 0))))
                                      eax)))
                    (:generator 3
                       (let* ((base-disp (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
                              (memref
                                (sc-case index
                                  (immediate (make-ea :word :base vector :disp (+ base-disp (tn-value index))))
                                  (t (make-ea :word :base vector :disp base-disp :index index)))))
                         ,@(when setterp '((move eax value)))
                         ,@(when (and setterp (not le-p)) '((inst rol ax-tn 8)))
                         ,(if setterp
                              '(inst mov memref ax-tn)
                              (if le-p
                                  (if signedp
                                      '(inst movsx result memref)
                                      '(inst movzx result memref))
                                  '(inst mov ax-tn memref)))
                         ,@(unless setterp
                             (unless le-p
                               `(eax ; KLUDGE: refer to temporary explicitly to avoid notes
                                 (inst rol ax-tn 8)
                                 ,(if signedp
                                      '(inst movsx result ax-tn)
                                      '(inst movzx result ax-tn))))))))))))
  (def/16 sb-nibbles::ub16ref/be nil nil nil)
  (def/16 sb-nibbles::ub16ref/le nil nil t)
  (def/16 sb-nibbles::ub16set/be nil t nil)
  (def/16 sb-nibbles::ub16set/le nil t t)
  (def/16 sb-nibbles::sb16ref/be t nil nil)
  (def/16 sb-nibbles::sb16ref/le t nil t)
  (def/16 sb-nibbles::sb16set/be t t nil)
  (def/16 sb-nibbles::sb16set/le t t t))

(macrolet ((def/32 (name signedp setterp le-p)
             (flet ((computed-name (signedp setterp le-p)
                      (let ((symbol-name (format nil "~:[UB~;SB~]32~:[REF~;SET~]/~:[BE~;LE~]" signedp setterp le-p)))
                        (find-symbol symbol-name "SB-NIBBLES"))))
               (assert (eq name (computed-name signedp setterp le-p)))
               (let ((result-sc (if signedp 'signed-reg 'unsigned-reg))
                     (result-type (if signedp 'signed-num 'unsigned-num)))
                 `(define-vop (,name)
                    (:translate ,name)
                    (:policy :fast-safe)
                    (:args (vector :scs (descriptor-reg))
                           (index :scs (unsigned-reg))
                           ,@(when setterp `((value :scs (,result-sc)))))
                    (:arg-types simple-array-unsigned-byte-8 positive-fixnum
                                ,@(when setterp `(,result-type)))
                    ,@(unless setterp
                        (list
                          `(:results (result :scs (,result-sc)))
                          `(:result-types ,result-type)))
                    ,@(when (and setterp (not le-p))
                        `((:temporary (:sc unsigned-reg :from (:load 0)) temp)))
                    (:generator 3
                       (let* ((base-disp (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
                              (memref
                                (sc-case index
                                  (immediate (make-ea :dword :base vector :disp (+ base-disp (tn-value index))))
                                  (t (make-ea :dword :base vector :disp base-disp :index index)))))
                         ,@(when (and setterp (not le-p))
                             '((inst mov temp value)
                               (inst bswap temp)))
                         ,(if setterp
                              `(inst mov memref ,(if le-p 'value 'temp))
                              '(inst mov result memref))
                         ,@(unless setterp
                             (unless le-p
                               '((inst bswap result)))))))))))
  (def/32 sb-nibbles::ub32ref/be nil nil nil)
  (def/32 sb-nibbles::ub32ref/le nil nil t)
  (def/32 sb-nibbles::ub32set/be nil t nil)
  (def/32 sb-nibbles::ub32set/le nil t t)
  (def/32 sb-nibbles::sb32ref/be t nil nil)
  (def/32 sb-nibbles::sb32ref/le t nil t)
  (def/32 sb-nibbles::sb32set/be t t nil)
  (def/32 sb-nibbles::sb32set/le t t t))
