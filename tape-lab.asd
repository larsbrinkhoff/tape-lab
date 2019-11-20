;;;; tape-lab.asd

(asdf:defsystem #:tape-lab
  :description "Examine and recover data from broken tape images"
  :author "Lars Brinkhoff <lars@nocrew.org>"
  :license "GPL"
  :serial t
  :components ((:file "package")
               (:file "tape-lab")))
