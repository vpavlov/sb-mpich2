;;;; -*- lisp -*-
;;;;============================================================================
;;;; MPICH2 bindings for SBCL
;;;; Copyright (c) 2011 Valentin Pavlov <x.pavlov@gmail.com>
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.
;;;;----------------------------------------------------------------------------
;;;; SB-MPICH2 system definition. Loading this indicates that the user wants
;;;; to run a distributed system. In this case, some of the nodes may share
;;;; a storage and compiling files in parallel will lead to race conditions.
;;;; So, the first thing to do is to make asdf compile the systems in a
;;;; directory whose name depends on the MPI rank. For this we need some
;;;; early MPI definitions plus a little patch for the user cache location.
;;;;============================================================================
(defpackage #:sb-mpich2.system (:use #:cl #:asdf))
(in-package #:sb-mpich2.system)

(defun load-early-files (file-list)
  (mapcar #'(lambda (fname)
	      (load (merge-pathnames
		     fname
		     (make-pathname :name nil :type "lisp"
				    :defaults
				    (or *load-pathname*
					*default-pathname-defaults*)))
		    :verbose nil
		    :print nil))
	  file-list))

(load-early-files '("early-mpi" "patch-asdf"))

(defsystem sb-mpich2
    :description "MPICH2 bindings for SBCL"
    :version "1.0"
    :author "Valentin Pavlov <x.pavlov@gmail.com>"
    :license "MPI-style. See LICENSE for details"
    :serial t
    :components ((:file "defpackage")
		 (:file "mpi-h")
		 (:file "mpio-h")))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-mpich2))))
  (provide 'sb-mpich2))
