(ql:quickload :cffi)
(ql:quickload :trivial-gray-streams)
(load "curses.lisp")

(defpackage #:roguishly
  (:use :asdf :cl :cffi :curses :trivial-gray-streams))

(asdf:defsystem :roguishly
  :serial t
  :components ((:file "roguishly")))
