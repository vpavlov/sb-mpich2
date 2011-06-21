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
;;;; sb-bsd-sockets extension that emulates BSD socket interface over MPI
;;;;============================================================================
(in-package #:sb-mpich2)

;;; Class definition

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass mpi-socket (socket)
    ;; slots inherited from SOCKET, which must have
    ;; values that would prevent socket's shared-initialize to perform things
    ;; not applicable to MPI sockets (e.g. querying protocols, opening real
    ;; sockets, etc.)
    ((sb-bsd-sockets::file-descriptor :initform 0)
     (sb-bsd-sockets::family :initform sockint::AF-UNSPEC)
     (sb-bsd-sockets::type)
     
     ;; address-like information, which in the case of regular sockets is
     ;; maintained by the OS and accessed through file-descriptor
     (comm :initarg :comm :initform MPI_COMM_WORLD)
     (local-rank :initform nil)
     (remote-rank :initform nil)
     (tag :initform nil))
    (:documentation "Class representing MPI socket-like objects.")))

(defmethod sb-bsd-sockets::socket-namestring ((socket mpi-socket))
  (multiple-value-bind (comm addr tag)
      (socket-name socket)
    (format nil "~X/~A:~A" comm addr tag)))

(defmethod sb-bsd-sockets::socket-peerstring ((socket mpi-socket))
  (multiple-value-bind (comm addr tag)
      (socket-peername socket)
    (if addr
	(format nil "~X/~A:~A" comm addr tag))))

;; make sure that:
;;  - the type of socket is :DATAGRAM, since MPI is connectionless
;;  - cancel finalization, since the one setup by socket's shared-initialize
;;    closes the file descriptor (which we don't have here)
(defmethod shared-initialize :after ((socket mpi-socket) slot-names
				     &key comm &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value socket 'sb-bsd-sockets::type) :datagram)  
  (sb-ext:cancel-finalization socket))

(defmethod initialize-instance :after ((socket mpi-socket) &rest initargs
				       &key &allow-other-keys)
  (with-slots (comm local-rank) socket
    (with-mpi-call ((MPI_Comm_rank comm) rank)
      (setf local-rank rank))))

(defmethod socket-bind ((socket mpi-socket) &rest address)
  "Pass TAG as address"
  (setf (slot-value socket 'tag) (first address)))

(defun throw-socket-error (errno)
  (error (sb-bsd-sockets::condition-for-errno errno)
	 :errno errno :syscall 'throw-socket-error))

;; MPI sockets are connectionless, so pretend we received EOPNOTSUPP
;; from accept(2)
(defmethod socket-accept ((socket mpi-socket))
  (throw-socket-error sockint::EOPNOTSUPP))

(defmethod socket-connect ((socket mpi-socket) &rest peer)
  "Pass (REMOTE-RANK TAG) as peer"
  (with-slots (comm remote-rank tag) socket
    (let ((remote (first peer)))
      (with-mpi-call ((MPI_Comm_size comm) size)
	(when (>= remote size)
	  (throw-socket-error sockint::ECONNREFUSED)))
      (setf remote-rank remote tag (second peer)))))

(defmethod socket-peername ((socket mpi-socket))
  (with-slots (comm remote-rank tag) socket
    (values comm remote-rank tag)))

(defmethod socket-name ((socket mpi-socket))
  (with-slots (comm local-rank tag) socket
    (values comm local-rank tag)))

(defun derive-mpi-type (lisp-type)
  (cond ((equal lisp-type '(unsigned-byte 8)) MPI_BYTE)
	((equal lisp-type 'character) MPI_BYTE)  ; chars are sent as MPI_BYTE
	(t (throw-socket-error sockint::EINVAL))))

(defmethod socket-receive ((socket mpi-socket) buffer length
					  &key oob peek waitall dontwait
					  (element-type '(unsigned-byte 8)))
  (declare (ignore oob peek waitall))  ; N/A or no way to implement with MPI
  ;; handle various buffer/length supplied-p situations, except both being nil
  (if buffer
      (progn
	(unless length
	  (setf length (length buffer)))
	(setf element-type (array-element-type buffer)))
      (when length
	(setf buffer (make-array length :element-type element-type))))
  (let ((source (or (slot-value socket 'remote-rank) MPI_ANY_SOURCE))
	(tag (or (slot-value socket 'tag) MPI_ANY_TAG))
	(comm (slot-value socket 'comm))
	(datatype (derive-mpi-type element-type)))
    (flet ((recv-pending (status)
	     (with-mpi-call ((MPI_Get_count (alien-sap status) datatype) count)
	       (unless buffer
		 (setf buffer (make-array count :element-type element-type))
		 (setf length count))
	       (sb-bsd-sockets::with-vector-sap (sap buffer)
		 (with-mpi-call ((MPI_Recv sap length datatype
					   (slot status 'MPI_SOURCE)
					   (slot status 'MPI_TAG)
					   comm)))))
	     buffer))
      (if dontwait
	  (with-mpi-call ((MPI_IProbe source tag comm) flag status)
	    (when (not flag)
	      (throw-socket-error sockint::EAGAIN))
	    (recv-pending status))
	  (with-mpi-call ((MPI_Probe source tag comm) status)
	    (recv-pending status))))))

(defmethod socket-send ((socket mpi-socket) buffer length
                        &key
                        address
                        (external-format :default)
                        oob eor dontroute dontwait nosignal
                        #+linux confirm #+linux more)
  (declare (ignore oob eor dontroute dontwait nosignal confirm more))
  (let* ((buffer (etypecase buffer
                   (string
                    (sb-ext:string-to-octets buffer
					     :end (or length (length buffer))
                                             :external-format external-format
                                             :null-terminate nil))
                   ((simple-array (unsigned-byte 8))
                    (subseq buffer 0 (or length (length buffer))))
                   ((array (unsigned-byte 8))
                    (make-array (or length (length buffer))
                                :element-type '(unsigned-byte 8)
                                :initial-contents buffer))))
	 (comm (slot-value socket 'comm))
	 (remote-rank (if address (first address)
			  (slot-value socket 'remote-rank)))
	 (tag (if address (second address)
		   (slot-value socket 'tag)))
	 (datatype (derive-mpi-type (array-element-type buffer))))
    (sb-bsd-sockets::with-vector-sap (sap buffer)
      (setf length (length buffer))
      (unless (and remote-rank tag)
	(throw-socket-error sockint::EINVAL))
      (with-mpi-call ((MPI_Send sap length datatype
				remote-rank tag comm))))
    length))
    
(defmethod socket-listen ((socket mpi-socket) backlog)
  (throw-socket-error sockint::EOPNOTSUPP))

(defmethod socket-close ((socket mpi-socket) &key abort)
  nil)

(defmethod socket-make-stream ((socket mpi-socket)
                               &key input output
                               (element-type 'character)
                               buffering
                               (external-format :default)
                               timeout
                               auto-close
                               serve-events)
  (declare (ignore buffering timeout auto-close serve-events))
  (assert (equal element-type 'character))
  (let ((stream
	 (and (slot-boundp socket 'stream)
	      (slot-value socket 'stream))))
    (unless stream
      (setf stream
	    (cond ((and input output)
		   (make-instance 'mpi-character-io-stream
				  :in-socket socket
				  :out-socket socket
				  :external-format external-format))
		  (input
		   (make-instance 'mpi-character-input-stream
				  :in-socket socket
				  :external-format external-format))
		  (t
		   (make-instance 'mpi-character-output-stream
				  :out-socket socket))))
      (setf (slot-value socket 'stream) stream))
    stream))
