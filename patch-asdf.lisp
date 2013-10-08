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
;;;; Patch asdf to include the MPI rank of the node in the implementation
;;;; identifier in order to prevent race conditions when compiling files
;;;;============================================================================
(in-package #:asdf)

;;; This is sraped off from asdf version 2.015.3
;; (defun* implementation-identifier ()
;;   (labels
;;       ((maybe-warn (value fstring &rest args)
;;          (cond (value)
;;                (t (apply 'warn fstring args)
;;                   "unknown"))))
;;     (let ((lisp (maybe-warn (implementation-type)
;;                             (compatfmt
;; 			     "~@<No implementation feature found in ~a.~@:>")
;;                             *implementation-features*))
;;           (os   (maybe-warn (first-feature *os-features*)
;;                             (compatfmt
;; 			     "~@<No OS feature found in ~a.~@:>")
;; 			    *os-features*))
;;           (arch (or #-clisp
;;                     (maybe-warn (first-feature *architecture-features*)
;;                                 (compatfmt
;; 				 "~@<No architecture feature found in ~a.~@:>")
;;                                 *architecture-features*)))
;;           (version
;; 	   (maybe-warn (lisp-version-string)
;; 		       "Don't know how to get Lisp implementation version.")))
;;       ;; Include the MPI rank in the return value. The '~7,'0,'/3:D/' format
;;       ;; splits the rank in triples (e.g. 0/292/345) thus creating a
;;       ;; sub-directory structure, because we might end up with
;;       ;; hundreds of thousands of nodes and we don't want this in a single
;;       ;; directory.
;;       (substitute-if
;;        #\_ #'(lambda (x) (find x " :\\(){}[]$#`'\""))
;;        (format nil "~7,'0,'/3:D/~(~a~@{~@[-~a~]~}~)"
;; 	       sb-mpich2::*mpi-world-rank* lisp version os arch)))))

;;; asdf 2.26
(defun* implementation-identifier ()
  (substitute-if
   #\_ #'(lambda (x) (find x " /:;&^\\|?<>(){}[]$#`'\""))
   (format nil "~7,'0,'/3:D/~(~a~@{~@[-~a~]~}~)"
	   sb-mpich2::*mpi-world-rank*
           (or (implementation-type) (lisp-implementation-type))
           (or (lisp-version-string) (lisp-implementation-version))
           (or (operating-system) (software-type))
           (or (architecture) (machine-type)))))
