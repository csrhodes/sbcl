#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

(defsystem "sb-nibbles"
  :description "An sbcl-specific implementation of an internal nibbles interface"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "SBCL Developers <sbcl-devel@lists.sourceforge.net"
  :license "BSD-style (http://opensource.org/licenses/BSD-3-Clause)"
  :version "0.13"
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-NIBBLES;"
  :components ((:file "package")
               (:file "fndb" :depends-on ("package"))
               (:module "arch-vm"
                :pathname ""
                :depends-on ("fndb")
                :components
                ((:file "x86-vm" :if-feature :x86)
                 (:file "x86-64-vm" :if-feature :x86-64)))
               (:file "generic-vm" :depends-on ("arch-vm")))
  :perform (load-op :after (o c) (provide 'sb-nibbles))
  :in-order-to ((test-op (test-op "sb-nibbles/tests"))))

(defsystem "sb-nibbles/tests"
  :version "0.1"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :depends-on ("sb-nibbles" "sb-rt")
  :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system "sb-nibbles/tests"))))
  (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (error "test-op failed")))
