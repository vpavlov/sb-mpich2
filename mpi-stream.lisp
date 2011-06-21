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
;;;; sb-gray based character I/O streams on top of MPI sockets
;;;;============================================================================
(in-package #:sb-mpich2)

;;; Class definition

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass mpi-character-input-stream (fundamental-character-input-stream)
    ((in-socket :initarg :in-socket)
     (in-buffer :initform "")
     (in-index :initform 0)
     (external-format :initarg :external-format :initform :default)
     ))
    
  (defclass mpi-character-output-stream (fundamental-character-output-stream)
    ((out-socket :initarg :out-socket)
     (out-buffer :initform (make-string 8192))
     (out-index :initform 0)
     (column :initform 0)
     (external-format :initarg :external-format :initform :default)
     ))

  (defclass mpi-character-io-stream (mpi-character-input-stream
				     mpi-character-output-stream)
    ())
  )

;;; Implementation of methods. Follows the order in the manual.

(defmethod stream-clear-input ((stream mpi-character-input-stream))
  (with-slots (in-buffer in-index) stream
    (setf in-buffer "" in-index 0))
  nil)

(defmethod stream-clear-output ((stream mpi-character-output-stream))
  (with-slots (out-index) stream
    (setf out-index 0)))

(defmethod stream-finish-output ((stream mpi-character-output-stream))
  (with-slots (out-socket out-buffer out-index external-format) stream
    (unless (zerop out-index)
      (socket-send out-socket out-buffer out-index
		   :external-format external-format)
      (stream-clear-output stream)))
  nil)

(defmethod stream-force-output ((stream mpi-character-output-stream))
  (stream-finish-output stream))

(defmethod stream-read-char-no-hang ((stream mpi-character-input-stream))
  (with-slots (in-buffer in-index) stream
    (when (< in-index (length in-buffer))
      (prog1
	  (aref in-buffer in-index)
	(incf in-index)))))

(defmethod stream-read-char ((stream mpi-character-input-stream))
  (with-slots (in-socket in-buffer in-index external-format) stream
    (when (= in-index (length in-buffer))
      (let ((string (sb-ext:octets-to-string (socket-receive in-socket nil nil)
					     :external-format external-format)))
	(cond ((zerop (length string))
	       (return-from stream-read-char :eof))
	      (t (setf in-buffer string)
		 (setf in-index 0)))))
    (assert (plusp (length in-buffer)))
    (prog1
	(aref in-buffer in-index)
      (incf in-index))))

(defmethod stream-listen ((stream mpi-character-input-stream))
  (with-slots (in-buffer in-index) stream
    (< in-index (length in-buffer))))

(defmethod stream-unread-char ((stream mpi-character-input-stream) char)
  (with-slots (in-buffer in-index) stream
    (decf in-index)
    (cond ((eql (aref in-buffer in-index) char)
	   (setf (aref in-buffer in-index) char))
	  (t
	   (warn "stream-unread-char: ignoring ~S (expected ~S)"
		 char (aref in-buffer in-index)))))
  nil)

(defmethod stream-fresh-line ((stream mpi-character-output-stream))
  (with-slots (column) stream
    (cond ((zerop column) nil)
	  (t (terpri stream) t))))

(defmethod stream-line-column ((stream mpi-character-output-stream))
  (with-slots (column) stream
    column))

(defmethod stream-line-length ((stream mpi-character-output-stream))
  75)

(defmethod stream-write-char ((stream mpi-character-output-stream) character)
  (with-slots (out-buffer out-index column) stream
    (setf (schar out-buffer out-index) character)
    (incf out-index)
    (incf column)
    (when (char= #\newline character)
      (setf column 0))
    (when (= out-index (length out-buffer))
      (finish-output stream)))
  character)

(defmethod stream-write-string ((stream mpi-character-output-stream) string
                                &optional start end)
  (with-slots (out-socket out-buffer out-index column external-format) stream
    (let* ((start (or start 0))
           (end (or end (length string)))
           (len (length out-buffer))
           (count (- end start))
           (free (- len out-index)))
      (when (>= count free)
        (stream-finish-output stream))
      (cond ((< count len)
             (replace out-buffer string :start1 out-index
                      :start2 start :end2 end)
             (incf out-index count))
            (t
	     (socket-send out-socket (subseq string start end) count
			  :external-format external-format)))
      (let ((last-newline (position #\newline string :from-end t
                                    :start start :end end)))
        (setf column (if last-newline 
                         (- end last-newline 1)
                         (+ column count))))))
  string)
