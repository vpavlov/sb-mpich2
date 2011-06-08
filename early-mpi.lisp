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
;;;; This file contains some early definitions needed to initialize MPI and
;;;; establish our rank before compile-file is called by asdf on the system's
;;;; components.
;;;;============================================================================
(defpackage #:sb-mpich2 (:use #:cl #:sb-alien))
(in-package #:sb-mpich2)

;;; Load the MPI shared libary (different set of libraries on the Blue Gene)
#-bgpcnk (load-shared-object "libmpich.so")
#+bgpcnk (load-shared-object "libpthread-2.4.so")
#+bgpcnk (load-shared-object "librt-2.4.so")
#+bgpcnk (load-shared-object "libSPI.cna.so")
#+bgpcnk (load-shared-object "libdcmfcoll.cnk.so")
#+bgpcnk (load-shared-object "libdcmf.cnk.so")
#+bgpcnk (load-shared-object "libmpich.cnk.so")

;;; Alien definitions

(defconstant MPI_SUCCESS          0)
(defconstant MPI_THREAD_MULTIPLE  3)
(defconstant MPI_COMM_WORLD       #x44000000)

(defvar *mpi-world-rank* 0 "The MPI process rank in the global communicator")

(define-alien-type MPI_Comm int)

;; int MPI_Init(int *, char ***);
(declaim (inline MPI_Init))
(define-alien-routine ("MPI_Init" MPI_Init)
    int
  (argc (* int))
  (argv (* (array c-string))))

;; int MPI_Init_thread(int *, char ***, int, int *);
(declaim (inline MPI_Init_thread))
(define-alien-routine ("MPI_Init_thread" MPI_Init_thread)
      int
    (argc (* int))
    (argv (* (array c-string)))
    (required int)
    (provided int :out))

;; int MPI_Comm_rank(MPI_Comm, int *);
(declaim (inline MPI_Comm_rank))
(define-alien-routine ("MPI_Comm_rank" MPI_Comm_rank)
    int
  (comm MPI_Comm)
  (rank int :out))

;; int MPI_Initialized(int *);
(declaim (inline MPI_Initialized))
(define-alien-routine ("MPI_Initialized" MPI_Initialized)
    int
  (flag int :out))

;;; Wrappers

(defmacro with-mpi-call ((fn-call &rest out-vals) &body body)
  (let ((retval (gensym)))
    `(multiple-value-bind (,retval ,@out-vals)
	 ,fn-call
       (when (/= ,retval MPI_SUCCESS)
	 (error "mpi call ~a returned ~x" ',fn-call ,retval))
       ,@body)))

(defun mpi-init ()
  (with-mpi-call ((MPI_Initialized) init?)
    (when (/= init? 0)
      (return-from mpi-init (values))))
  (let ((argv (make-alien (array c-string)))
	(argc (make-alien int)))
    (setf (deref argc) 0)
    #-sb-thread
    (with-mpi-call ((MPI_Init argc argv)))
    #+sb-thread
    (with-mpi-call ((MPI_Init_thread argc argv MPI_THREAD_MULTIPLE) provided))
    (free-alien argc)
    (free-alien argv))
;;  (with-mpi-call ((MPI_Comm_set_errhandler MPI_COMM_WORLD MPI_ERRORS_RETURN)))
  (with-mpi-call ((MPI_Comm_rank MPI_COMM_WORLD) rank)
    (setf *mpi-world-rank* rank))
  (if (= *mpi-world-rank* 0)
      (pushnew :mpi-master *features*)
      (pushnew :mpi-slave *features*))
  (values))

;;; Finally, perform initialization

(mpi-init)
