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
;;;; This is an almost verbatim translation of <mpio.h>. It is based on the
;;;; version of the file that is included in V1R4M1 version of IBM Blue Gene/P
;;;; OS: /bgsys/drivers/ppcfloor/comm/default/include/mpi.h
;;;; NOTES: I've tried to keep the text as closed to the original C as possible.
;;;;        The comments are just lisp-commented in front, with /* and */
;;;;        deleted only for single-line comments at the end of #defines in
;;;;        order to fit the text in a single line. I also removed all tabs in
;;;;        order to align the text. Otherwise the translation includes:
;;;;          - replace #define ... with (defconstant
;;;;          - replace (1 << N) with (ash 1 N)
;;;;          - replace 0xN with #xN
;;;;          - remove irrelevant definitions (e.g. #ifdef _MPI_H_, etc.)
;;;;          - put comments, e.g. [VNP] where important stuff happens
;;;;============================================================================
(in-package #:sb-mpich2)

;; /* -*- Mode: C; c-basic-offset:4 ; -*- */
;; /* 
;;  *
;;  *   Copyright (C) 1997 University of Chicago. 
;;  *   See COPYRIGHT notice in top-level directory.
;;  */

;; /* user include file for MPI-IO programs */

;; #ifndef MPIO_INCLUDE
;; #define MPIO_INCLUDE

;; #include "mpi.h"

;; #if defined(__cplusplus)
;; extern "C" {
;; #endif

(defconstant ROMIO_VERSION 126)  ;; /* version 1.2.6 */

;; /* define MPI-IO datatypes and constants */

;; [VNP] This is defined in mpi-h.lisp
;; #ifndef MPI_FILE_DEFINED
;; typedef struct ADIOI_FileD *MPI_File;
;; #endif

;; #define HAVE_MPI_GREQUEST 1
;; #ifndef HAVE_MPI_GREQUEST
;; typedef struct ADIOI_RequestD *MPIO_Request;  
;; #else
(define-alien-type MPIO_Request MPI_Request)
;; #define MPIO_USES_MPI_REQUEST
;; /* Also rename the MPIO routines to get the MPI versions */
(defmacro MPIO_Wait (&rest body) `(MPI_Wait ,@body))
(defmacro MPIO_Test (&rest body) `(MPI_Test ,@body))
(defmacro PMPIO_Wait (&rest body) `(PMPI_Wait ,@body))
(defmacro PMPIO_Test (&rest body) `(PMPI_Test ,@body))
;; #endif
;; #define MPIO_REQUEST_DEFINED

;; [VNP] This is defined in mpi-h.lisp
;; #ifndef HAVE_MPI_OFFSET
;; typedef long long MPI_Offset;
;; /* If we needed to define MPI_Offset, then we also need to make
;;    this definition. */
;; #ifndef HAVE_MPI_DATAREP_FUNCTIONS
;; #define HAVE_MPI_DATAREP_FUNCTIONS
;; typedef int (MPI_Datarep_conversion_function)(void *, MPI_Datatype, int, 
;;              void *, MPI_Offset, void *);
;; typedef int (MPI_Datarep_extent_function)(MPI_Datatype datatype, MPI_Aint *,
;; 					  void *);
;; #endif
;; #endif

;; #ifndef NEEDS_MPI_FINT

;; #endif
;; #ifdef NEEDS_MPI_FINT
;; typedef int MPI_Fint; 
;; #endif

;; [VNP] This is handled in mpi-h.lisp
;; #ifndef HAVE_MPI_INFO
;; #define HAVE_MPI_INFO
;; #endif
;; #ifndef HAVE_MPI_INFO
;;   typedef struct MPIR_Info *MPI_Info;
;; # define MPI_INFO_NULL         ((MPI_Info) 0)
;; # define MPI_MAX_INFO_KEY       255
;; # define MPI_MAX_INFO_VAL      1024
;; #endif

(defconstant MPI_MODE_RDONLY              2) ;;  /* ADIO_RDONLY */
(defconstant MPI_MODE_RDWR                8) ;;  /* ADIO_RDWR  */
(defconstant MPI_MODE_WRONLY              4) ;;  /* ADIO_WRONLY  */
(defconstant MPI_MODE_CREATE              1) ;;  /* ADIO_CREATE */ 
(defconstant MPI_MODE_EXCL               64) ;;  /* ADIO_EXCL */
(defconstant MPI_MODE_DELETE_ON_CLOSE    16) ;;  /* ADIO_DELETE_ON_CLOSE */
(defconstant MPI_MODE_UNIQUE_OPEN        32) ;;  /* ADIO_UNIQUE_OPEN */
(defconstant MPI_MODE_APPEND            128) ;;  /* ADIO_APPEND */
(defconstant MPI_MODE_SEQUENTIAL        256) ;;  /* ADIO_SEQUENTIAL */

(defconstant MPI_DISPLACEMENT_CURRENT   -54278278)

;; [VNP] Well, this IS MPICH2
;; #ifndef MPICH2
;; /* FIXME: Make sure that we get a consistent definition of MPI_FILE_NULL
;; 	in MPICH2 */
;; /* MPICH2 defines null object handles differently */
;; (defconstant MPI_FILE_NULL           ((MPI_File) 0)
;; #endif

(defconstant MPIO_REQUEST_NULL       0)

(defconstant MPI_SEEK_SET            600)
(defconstant MPI_SEEK_CUR            602)
(defconstant MPI_SEEK_END            604)

(defconstant MPI_MAX_DATAREP_STRING  128)

;; [VNP] this is handled in mpi-h.lisp
;; #ifndef HAVE_MPI_DARRAY_SUBARRAY
;; (defconstant HAVE_MPI_DARRAY_SUBARRAY
;; #endif
;; #ifndef HAVE_MPI_DARRAY_SUBARRAY
;; #  define MPI_ORDER_C             56
;; #  define MPI_ORDER_FORTRAN       57
;; #  define MPI_DISTRIBUTE_BLOCK    121
;; #  define MPI_DISTRIBUTE_CYCLIC   122
;; #  define MPI_DISTRIBUTE_NONE     123
;; #  define MPI_DISTRIBUTE_DFLT_DARG -49767
;; #endif


;; /* MPI-IO function prototypes */

;; /* The compiler must support ANSI C style prototypes, otherwise 
;;    long long constants (e.g. 0) may get passed as ints. */

;; #ifndef HAVE_PRAGMA_HP_SEC_DEF

;; /* Section 9.2 */
;; /* Begin Prototypes */
;; int MPI_File_open(MPI_Comm, char *, int, MPI_Info, MPI_File *);
(declaim (inline MPI_File_open))
(define-alien-routine ("MPI_File_open" MPI_File_open)
    int
  (comm MPI_Comm)
  (filename c-string)
  (amode int)
  (info MPI_Info)
  (mpi_fh MPI_File :out))

;; int MPI_File_close(MPI_File *);
(declaim (inline MPI_File_close))
(define-alien-routine ("MPI_File_close" MPI_File_close)
    int
  (mpi_fh (* MPI_File)))

;; int MPI_File_delete(char *, MPI_Info);
(declaim (inline MPI_File_delete))
(define-alien-routine ("MPI_File_delete" MPI_File_delete)
    int
  (filename c-string)
  (info MPI_Info))

;; int MPI_File_set_size(MPI_File, MPI_Offset);
(declaim (inline MPI_File_set_size))
(define-alien-routine ("MPI_File_set_size" MPI_File_set_size)
    int
  (mpi_fh MPI_File)
  (size MPI_Offset))

;; int MPI_File_preallocate(MPI_File, MPI_Offset);
(declaim (inline MPI_File_preallocate))
(define-alien-routine ("MPI_File_preallocate" MPI_File_preallocate)
    int
  (mpi_fh MPI_File)
  (size MPI_Offset))

;; int MPI_File_get_size(MPI_File, MPI_Offset *);
(declaim (inline MPI_File_get_size))
(define-alien-routine ("MPI_File_get_size" MPI_File_get_size)
    int
  (mpi_fh MPI_File)
  (size MPI_Offset :out))

;; int MPI_File_get_group(MPI_File, MPI_Group *);
(declaim (inline MPI_File_get_group))
(define-alien-routine ("MPI_File_get_group" MPI_File_get_group)
    int
  (mpi_fh MPI_File)
  (group MPI_Group :out))

;; int MPI_File_get_amode(MPI_File, int *);
(declaim (inline MPI_File_get_amode))
(define-alien-routine ("MPI_File_get_amode" MPI_File_get_amode)
    int
  (mpi_fh MPI_File)
  (amode int :out))

;; int MPI_File_set_info(MPI_File, MPI_Info);
(declaim (inline MPI_File_set_info))
(define-alien-routine ("MPI_File_set_info" MPI_File_set_info)
    int
  (mpi_fh MPI_File)
  (info MPI_Info))

;; int MPI_File_get_info(MPI_File, MPI_Info *);
(declaim (inline MPI_File_get_info))
(define-alien-routine ("MPI_File_get_info" MPI_File_get_info)
    int
  (mpi_fh MPI_File)
  (info_used MPI_Info :out))


;; /* Section 9.3 */
;; int MPI_File_set_view(MPI_File, MPI_Offset, MPI_Datatype,
;; 	         MPI_Datatype, char *, MPI_Info);
(declaim (inline MPI_File_set_view))
(define-alien-routine ("MPI_File_set_view" MPI_File_set_view)
    int
  (mpi_fh MPI_File)
  (disp MPI_Offset)
  (etype MPI_Datatype)
  (filetype MPI_Datatype)
  (datarep c-string)
  (info MPI_Info))

;; int MPI_File_get_view(MPI_File, MPI_Offset *, 
;;                  MPI_Datatype *, MPI_Datatype *, char *);
(declaim (inline MPI_File_get_view))
(define-alien-routine ("MPI_File_get_view" MPI_File_get_view)
    int
  (mpi_fh MPI_File)
  (disp MPI_Offset :out)
  (etype MPI_Datatype :out)
  (filetype MPI_Datatype :out)
  (datarep (* char)))

;; /* Section 9.4.2 */
;; int MPI_File_read_at(MPI_File, MPI_Offset, void *,
;; 	      int, MPI_Datatype, MPI_Status *);
(declaim (inline MPI_File_read_at))
(define-alien-routine ("MPI_File_read_at" MPI_File_read_at)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int MPI_File_read_at_all(MPI_File, MPI_Offset, void *,
;; 	      int, MPI_Datatype, MPI_Status *);
(declaim (inline MPI_File_read_at_all))
(define-alien-routine ("MPI_File_read_at_all" MPI_File_read_at_all)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int MPI_File_write_at(MPI_File, MPI_Offset, void *,
;; 	      int, MPI_Datatype, MPI_Status *);
(declaim (inline MPI_File_write_at))
(define-alien-routine ("MPI_File_write_at" MPI_File_write_at)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int MPI_File_write_at_all(MPI_File, MPI_Offset, void *,
;; 	      int, MPI_Datatype, MPI_Status *);
(declaim (inline MPI_File_write_at_all))
(define-alien-routine ("MPI_File_write_at_all" MPI_File_write_at_all)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))


;; /* nonblocking calls currently use MPIO_Request, because generalized
;;    requests not yet implemented. For the same reason, MPIO_Test and 
;;    MPIO_Wait are used to test and wait on nonblocking I/O requests */ 

;; int MPI_File_iread_at(MPI_File, MPI_Offset, void *,
;; 	      int, MPI_Datatype, MPIO_Request *);
(declaim (inline MPI_File_iread_at))
(define-alien-routine ("MPI_File_iread_at" MPI_File_iread_at)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (request MPIO_Request :out))

;; int MPI_File_iwrite_at(MPI_File, MPI_Offset, void *,
;; 	      int, MPI_Datatype, MPIO_Request *);
(declaim (inline MPI_File_iwrite_at))
(define-alien-routine ("MPI_File_iwrite_at" MPI_File_iwrite_at)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (request MPIO_Request :out))


;; /* Section 9.4.3 */
;; int MPI_File_read(MPI_File, void *, int, MPI_Datatype, MPI_Status *); 
(declaim (inline MPI_File_read))
(define-alien-routine ("MPI_File_read" MPI_File_read)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int MPI_File_read_all(MPI_File, void *, int, MPI_Datatype, MPI_Status *); 
(declaim (inline MPI_File_read_all))
(define-alien-routine ("MPI_File_read_all" MPI_File_read_all)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int MPI_File_write(MPI_File, void *, int, MPI_Datatype, MPI_Status *);
(declaim (inline MPI_File_write))
(define-alien-routine ("MPI_File_write" MPI_File_write)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int MPI_File_write_all(MPI_File, void *, int, MPI_Datatype, MPI_Status *);
(declaim (inline MPI_File_write_all))
(define-alien-routine ("MPI_File_write_all" MPI_File_write_all)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; /* nonblocking calls currently use MPIO_Request, because generalized
;;    requests not yet implemented. For the same reason, MPIO_Test and 
;;    MPIO_Wait are used to test and wait on nonblocking I/O requests */ 

;; int MPI_File_iread(MPI_File, void *, int, MPI_Datatype, MPIO_Request *); 
(declaim (inline MPI_File_iread))
(define-alien-routine ("MPI_File_iread" MPI_File_iread)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (request MPI_Request :out))

;; int MPI_File_iwrite(MPI_File, void *, int, MPI_Datatype, MPIO_Request *);
(declaim (inline MPI_File_iwrite))
(define-alien-routine ("MPI_File_iwrite" MPI_File_iwrite)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (request MPI_Request :out))

;; int MPI_File_seek(MPI_File, MPI_Offset, int);
(declaim (inline MPI_File_seek))
(define-alien-routine ("MPI_File_seek" MPI_File_seek)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (whence int))

;; int MPI_File_get_position(MPI_File, MPI_Offset *);
(declaim (inline MPI_File_get_position))
(define-alien-routine ("MPI_File_get_position" MPI_File_get_position)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset :out))

;; int MPI_File_get_byte_offset(MPI_File, MPI_Offset, MPI_Offset *);
(declaim (inline MPI_File_get_byte_offset))
(define-alien-routine ("MPI_File_get_byte_offset" MPI_File_get_byte_offset)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (disp MPI_Offset :out))

;; /* Section 9.4.4 */
;; int MPI_File_read_shared(MPI_File, void *, int, MPI_Datatype, MPI_Status *);
(declaim (inline MPI_File_read_shared))
(define-alien-routine ("MPI_File_read_shared" MPI_File_read_shared)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int MPI_File_write_shared(MPI_File, void *, int, MPI_Datatype, MPI_Status *);
(declaim (inline MPI_File_write_shared))
(define-alien-routine ("MPI_File_write_shared" MPI_File_write_shared)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int MPI_File_iread_shared(MPI_File, void *, int, MPI_Datatype,
;;                           MPIO_Request *);
(declaim (inline MPI_File_iread_shared))
(define-alien-routine ("MPI_File_iread_shared" MPI_File_iread_shared)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (request MPI_Request :out))
    
;; int MPI_File_iwrite_shared(MPI_File, void *, int, 
;; 			   MPI_Datatype, MPIO_Request *);
(declaim (inline MPI_File_iwrite_shared))
(define-alien-routine ("MPI_File_iwrite_shared" MPI_File_iwrite_shared)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (request MPI_Request :out))

;; int MPI_File_read_ordered(MPI_File, void *, int, 
;;                           MPI_Datatype, MPI_Status *);
(declaim (inline MPI_File_read_ordered))
(define-alien-routine ("MPI_File_read_ordered" MPI_File_read_ordered)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int MPI_File_write_ordered(MPI_File, void *, int, 
;;                            MPI_Datatype, MPI_Status *);
(declaim (inline MPI_File_write_ordered))
(define-alien-routine ("MPI_File_write_ordered" MPI_File_write_ordered)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int MPI_File_seek_shared(MPI_File, MPI_Offset, int);
(declaim (inline MPI_File_seek_shared))
(define-alien-routine ("MPI_File_seek_shared" MPI_File_seek_shared)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (whence int))

;; int MPI_File_get_position_shared(MPI_File, MPI_Offset *);
(declaim (inline MPI_File_get_position_shared))
(define-alien-routine ("MPI_File_get_position_shared"
		       MPI_File_get_position_shared)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset :out))

;; /* Section 9.4.5 */
;; int MPI_File_read_at_all_begin(MPI_File, MPI_Offset, void *,
;;                                int, MPI_Datatype);
(declaim (inline MPI_File_read_at_all_begin))
(define-alien-routine ("MPI_File_read_at_all_begin" MPI_File_read_at_all_begin)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype))

;; int MPI_File_read_at_all_end(MPI_File, void *, MPI_Status *);
(declaim (inline MPI_File_read_at_all_end))
(define-alien-routine ("MPI_File_read_at_all_end" MPI_File_read_at_all_end)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (status MPI_Status :out))

;; int MPI_File_write_at_all_begin(MPI_File, MPI_Offset, void *,
;;                                 int, MPI_Datatype);
(declaim (inline MPI_File_write_at_all_begin))
(define-alien-routine ("MPI_File_write_at_all_begin"
		       MPI_File_write_at_all_begin)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype))

;; int MPI_File_write_at_all_end(MPI_File, void *, MPI_Status *);
(declaim (inline MPI_File_write_at_all_end))
(define-alien-routine ("MPI_File_write_at_all_end" MPI_File_write_at_all_end)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (status MPI_Status :out))

;; int MPI_File_read_all_begin(MPI_File, void *, int, MPI_Datatype);
(declaim (inline MPI_File_read_all_begin))
(define-alien-routine ("MPI_File_read_all_begin" MPI_File_read_all_begin)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype))

;; int MPI_File_read_all_end(MPI_File, void *, MPI_Status *);
(declaim (inline MPI_File_read_all_end))
(define-alien-routine ("MPI_File_read_all_end" MPI_File_read_all_end)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (status MPI_Status :out))

;; int MPI_File_write_all_begin(MPI_File, void *, int, MPI_Datatype);
(declaim (inline MPI_File_write_all_begin))
(define-alien-routine ("MPI_File_write_all_begin" MPI_File_write_all_begin)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype))

;; int MPI_File_write_all_end(MPI_File, void *, MPI_Status *);
(declaim (inline MPI_File_write_all_end))
(define-alien-routine ("MPI_File_write_all_end" MPI_File_write_all_end)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (status MPI_Status :out))

;; int MPI_File_read_ordered_begin(MPI_File, void *, int, MPI_Datatype);
(declaim (inline MPI_File_read_ordered_begin))
(define-alien-routine ("MPI_File_read_ordered_begin"
		       MPI_File_read_ordered_begin)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype))

;; int MPI_File_read_ordered_end(MPI_File, void *, MPI_Status *);
(declaim (inline MPI_File_read_ordered_end))
(define-alien-routine ("MPI_File_read_ordered_end" MPI_File_read_ordered_end)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (status MPI_Status :out))

;; int MPI_File_write_ordered_begin(MPI_File, void *, int, MPI_Datatype);
(declaim (inline MPI_File_write_ordered_begin))
(define-alien-routine ("MPI_File_write_ordered_begin"
		       MPI_File_write_ordered_begin)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype))

;; int MPI_File_write_ordered_end(MPI_File, void *, MPI_Status *);
(declaim (inline MPI_File_write_ordered_end))
(define-alien-routine ("MPI_File_write_ordered_end" MPI_File_write_ordered_end)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (status MPI_Status :out))


;; /* Section 9.5.1 */
;; int MPI_File_get_type_extent(MPI_File, MPI_Datatype, MPI_Aint *);
(declaim (inline MPI_File_get_type_extent))
(define-alien-routine ("MPI_File_get_type_extent" MPI_File_get_type_extent)
    int
  (mpi_fh MPI_File)
  (datatype MPI_Datatype)
  (extent MPI_Aint :out))


;; /* Section 9.5.3 */
;; int MPI_Register_datarep(char *,
;; 			 MPI_Datarep_conversion_function *,
;; 			 MPI_Datarep_conversion_function *,
;; 			 MPI_Datarep_extent_function *,
;; 			 void *);
(declaim (inline MPI_Register_datarep))
(define-alien-routine ("MPI_Register_datarep" MPI_Register_datarep)
    int
  (name c-string)
  (read_conv_fn MPI_Datarep_conversion_function)
  (write_conv_fn MPI_Datarep_conversion_function)
  (extent_fn MPI_Datarep_extent_function)
  (state (* t)))

;; /* Section 9.6.1 */
;; int MPI_File_set_atomicity(MPI_File, int);
(declaim (inline MPI_File_set_atomicity))
(define-alien-routine ("MPI_File_set_atomicity" MPI_File_set_atomicity)
    int
  (mpi_fh MPI_File)
  (flag int))

;; int MPI_File_get_atomicity(MPI_File, int *);
(declaim (inline MPI_File_get_atomicity))
(define-alien-routine ("MPI_File_get_atomicity" MPI_File_get_atomicity)
    int
  (mpi_fh MPI_File)
  (flag int :out))

;; int MPI_File_sync(MPI_File);
(declaim (inline MPI_File_sync))
(define-alien-routine ("MPI_File_sync" MPI_File_sync)
    int
  (mpi_fh MPI_File))


;; /* Section 4.13.3 */
;; #ifndef MPICH2
;; /* MPICH2 provides these definitions */
;; int MPI_File_set_errhandler( MPI_File, MPI_Errhandler );
;; int MPI_File_get_errhandler( MPI_File, MPI_Errhandler * );
;; #endif
;; /* End Prototypes */

;; #ifndef HAVE_MPI_DARRAY_SUBARRAY
;; /* Section 4.14.4 */
;; int MPI_Type_create_subarray(int, int *, int *, int *, int, 
;;                       MPI_Datatype, MPI_Datatype *);

;; /* Section 4.14.5 */
;; int MPI_Type_create_darray(int, int, int, 
;;                     int *, int *, int *, int *, 
;;                     int, MPI_Datatype, MPI_Datatype *);
;; #endif

;; /* The globus2 device has to rename MPI_ symbols in order to use the vendor
;;    MPI as one of its transport mechanisms.  Therefore, the following
;;    undefines should only happen if MPICH_RENAMING_MPI_FUNCS is not defined.
;;  */
;; /* Section 4.12.4 */
;; #if !defined(MPICH_RENAMING_MPI_FUNCS)
;; #ifdef MPI_File_f2c
;; #undef MPI_File_f2c
;; #endif
;; #ifdef MPI_File_c2f
;; #undef MPI_File_c2f
;; #endif
;; #endif
;; /* above needed for some versions of mpi.h in MPICH!! */
;; MPI_File MPI_File_f2c(MPI_Fint);
(declaim (inline MPI_File_f2c))
(define-alien-routine ("MPI_File_f2c" MPI_File_f2c)
    MPI_File
  (fh MPI_Fint))

;; MPI_Fint MPI_File_c2f(MPI_File);
(declaim (inline MPI_File_c2f))
(define-alien-routine ("MPI_File_c2f" MPI_File_c2f)
    MPI_Fint
  (mpi_fh MPI_File))

;; [VNP] Well, it seems that HAVE_MPI_GREQUEST is defined at the beginning of
;;       mpio.h as 1, so this does not hold
;; #ifndef HAVE_MPI_GREQUEST
;; /* The following functions are required if generalized requests are not
;;    available, because in that case, an MPIO_Request object
;;    is currently used for nonblocking I/O. */
;; int MPIO_Test(MPIO_Request *, int *, MPI_Status *);
;; int MPIO_Wait(MPIO_Request *, MPI_Status *);
;; int MPIO_Testall(int, MPIO_Request *, int *, MPI_Status *);
;; int MPIO_Waitall(int, MPIO_Request *, MPI_Status *);
;; int MPIO_Testany(int, MPIO_Request *, int *, int *, MPI_Status *);
;; int MPIO_Waitany(int, MPIO_Request *, int *, MPI_Status *);
;; int MPIO_Waitsome(int, MPIO_Request *, int *, int *, MPI_Status *);
;; int MPIO_Testsome(int, MPIO_Request *, int *, int *, MPI_Status *);

;; MPI_Fint MPIO_Request_c2f(MPIO_Request);
;; MPIO_Request MPIO_Request_f2c(MPI_Fint);
;; #endif /* HAVE_MPI_GREQUEST */

;; /* info functions if not defined in the MPI implementation */
;; #ifndef HAVE_MPI_INFO

;; int MPI_Info_create(MPI_Info *);
;; int MPI_Info_set(MPI_Info, char *, char *);
;; int MPI_Info_delete(MPI_Info, char *);
;; int MPI_Info_get(MPI_Info, char *, int, char *, int *);
;; int MPI_Info_get_valuelen(MPI_Info, char *, int *, int *);
;; int MPI_Info_get_nkeys(MPI_Info, int *);
;; int MPI_Info_get_nthkey(MPI_Info, int, char *);
;; int MPI_Info_dup(MPI_Info, MPI_Info *);
;; int MPI_Info_free(MPI_Info *);

;; /* The globus2 device has to rename MPI_ symbols in order to use the vendor
;;    MPI as one of its transport mechanisms.  Therefore, the following 
;;    defines should only happen if MPICH_RENAMING_MPI_FUNCS is not defined. */
;; #if !defined(MPICH_RENAMING_MPI_FUNCS)
;; #ifdef MPI_Info_f2c
;; #undef MPI_Info_f2c
;; #endif
;; #ifdef MPI_Info_c2f
;; #undef MPI_Info_c2f
;; #endif
;; #endif
;; /* above needed for some versions of mpi.h in MPICH!! */
;; MPI_Fint MPI_Info_c2f(MPI_Info);
;; MPI_Info MPI_Info_f2c(MPI_Fint);
;; #endif

;; #endif   /* HAVE_PRAGMA_HP_SEC_DEF */


;; /**************** BINDINGS FOR THE PROFILING INTERFACE ***************/


;; /* Section 9.2 */
;; int PMPI_File_open(MPI_Comm, char *, int, MPI_Info, MPI_File *);
(declaim (inline PMPI_File_open))
(define-alien-routine ("PMPI_File_open" PMPI_File_open)
    int
  (comm MPI_Comm)
  (filename c-string)
  (amode int)
  (info MPI_Info)
  (mpi_fh MPI_File :out))

;; int PMPI_File_close(MPI_File *);
(declaim (inline PMPI_File_close))
(define-alien-routine ("PMPI_File_close" PMPI_File_close)
    int
  (mpi_fh (* MPI_File)))

;; int PMPI_File_delete(char *, MPI_Info);
(declaim (inline PMPI_File_delete))
(define-alien-routine ("PMPI_File_delete" PMPI_File_delete)
    int
  (filename c-string)
  (info MPI_Info))

;; int PMPI_File_set_size(MPI_File, MPI_Offset);
(declaim (inline PMPI_File_set_size))
(define-alien-routine ("PMPI_File_set_size" PMPI_File_set_size)
    int
  (mpi_fh MPI_File)
  (size MPI_Offset))

;; int PMPI_File_preallocate(MPI_File, MPI_Offset);
(declaim (inline PMPI_File_preallocate))
(define-alien-routine ("PMPI_File_preallocate" PMPI_File_preallocate)
    int
  (mpi_fh MPI_File)
  (size MPI_Offset))

;; int PMPI_File_get_size(MPI_File, MPI_Offset *);
(declaim (inline PMPI_File_get_size))
(define-alien-routine ("PMPI_File_get_size" PMPI_File_get_size)
    int
  (mpi_fh MPI_File)
  (size MPI_Offset :out))

;; int PMPI_File_get_group(MPI_File, MPI_Group *);
(declaim (inline PMPI_File_get_group))
(define-alien-routine ("PMPI_File_get_group" PMPI_File_get_group)
    int
  (mpi_fh MPI_File)
  (group MPI_Group :out))

;; int PMPI_File_get_amode(MPI_File, int *);
(declaim (inline PMPI_File_get_amode))
(define-alien-routine ("PMPI_File_get_amode" PMPI_File_get_amode)
    int
  (mpi_fh MPI_File)
  (amode int :out))

;; int PMPI_File_set_info(MPI_File, MPI_Info);
(declaim (inline PMPI_File_set_info))
(define-alien-routine ("PMPI_File_set_info" PMPI_File_set_info)
    int
  (mpi_fh MPI_File)
  (info MPI_Info))

;; int PMPI_File_get_info(MPI_File, MPI_Info *);
(declaim (inline PMPI_File_get_info))
(define-alien-routine ("PMPI_File_get_info" PMPI_File_get_info)
    int
  (mpi_fh MPI_File)
  (info_used MPI_Info :out))

;; /* Section 9.3 */
;; int PMPI_File_set_view(MPI_File, MPI_Offset, MPI_Datatype,
;; 	         MPI_Datatype, char *, MPI_Info);
(declaim (inline PMPI_File_set_view))
(define-alien-routine ("PMPI_File_set_view" PMPI_File_set_view)
    int
  (mpi_fh MPI_File)
  (disp MPI_Offset)
  (etype MPI_Datatype)
  (filetype MPI_Datatype)
  (datarep c-string)
  (info MPI_Info))

;; int PMPI_File_get_view(MPI_File, MPI_Offset *, 
;;                  MPI_Datatype *, MPI_Datatype *, char *);
(declaim (inline PMPI_File_get_view))
(define-alien-routine ("PMPI_File_get_view" PMPI_File_get_view)
    int
  (mpi_fh MPI_File)
  (disp MPI_Offset :out)
  (etype MPI_Datatype :out)
  (filetype MPI_Datatype :out)
  (datarep (* char)))

;; /* Section 9.4.2 */
;; int PMPI_File_read_at(MPI_File, MPI_Offset, void *,
;; 	      int, MPI_Datatype, MPI_Status *);
(declaim (inline PMPI_File_read_at))
(define-alien-routine ("PMPI_File_read_at" PMPI_File_read_at)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int PMPI_File_read_at_all(MPI_File, MPI_Offset, void *,
;; 	      int, MPI_Datatype, MPI_Status *);
(declaim (inline PMPI_File_read_at_all))
(define-alien-routine ("PMPI_File_read_at_all" PMPI_File_read_at_all)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int PMPI_File_write_at(MPI_File, MPI_Offset, void *,
;; 	      int, MPI_Datatype, MPI_Status *);
(declaim (inline PMPI_File_write_at))
(define-alien-routine ("PMPI_File_write_at" PMPI_File_write_at)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int PMPI_File_write_at_all(MPI_File, MPI_Offset, void *,
;; 	      int, MPI_Datatype, MPI_Status *);
(declaim (inline PMPI_File_write_at_all))
(define-alien-routine ("PMPI_File_write_at_all" PMPI_File_write_at_all)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))


;; /* nonblocking calls currently use MPIO_Request, because generalized
;;    requests not yet implemented. For the same reason, MPIO_Test and 
;;    MPIO_Wait are used to test and wait on nonblocking I/O requests */ 

;; int PMPI_File_iread_at(MPI_File, MPI_Offset, void *,
;; 	      int, MPI_Datatype, MPIO_Request *);
(declaim (inline PMPI_File_iread_at))
(define-alien-routine ("PMPI_File_iread_at" PMPI_File_iread_at)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (request MPIO_Request :out))

;; int PMPI_File_iwrite_at(MPI_File, MPI_Offset, void *,
;; 	      int, MPI_Datatype, MPIO_Request *);
(declaim (inline PMPI_File_iwrite_at))
(define-alien-routine ("PMPI_File_iwrite_at" PMPI_File_iwrite_at)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (request MPIO_Request :out))


;; /* Section 9.4.3 */
;; int PMPI_File_read(MPI_File, void *, int, MPI_Datatype, MPI_Status *); 
(declaim (inline PMPI_File_read))
(define-alien-routine ("PMPI_File_read" PMPI_File_read)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int PMPI_File_read_all(MPI_File, void *, int, MPI_Datatype, MPI_Status *); 
(declaim (inline PMPI_File_read_all))
(define-alien-routine ("PMPI_File_read_all" PMPI_File_read_all)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int PMPI_File_write(MPI_File, void *, int, MPI_Datatype, MPI_Status *);
(declaim (inline PMPI_File_write))
(define-alien-routine ("PMPI_File_write" PMPI_File_write)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int PMPI_File_write_all(MPI_File, void *, int, MPI_Datatype, MPI_Status *);
(declaim (inline PMPI_File_write_all))
(define-alien-routine ("PMPI_File_write_all" PMPI_File_write_all)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; /* nonblocking calls currently use MPIO_Request, because generalized
;;    requests not yet implemented. For the same reason, MPIO_Test and 
;;    MPIO_Wait are used to test and wait on nonblocking I/O requests */ 

;; int PMPI_File_iread(MPI_File, void *, int, MPI_Datatype, MPIO_Request *); 
(declaim (inline PMPI_File_iread))
(define-alien-routine ("PMPI_File_iread" PMPI_File_iread)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (request MPI_Request :out))

;; int PMPI_File_iwrite(MPI_File, void *, int, MPI_Datatype, MPIO_Request *);
(declaim (inline PMPI_File_iwrite))
(define-alien-routine ("PMPI_File_iwrite" PMPI_File_iwrite)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (request MPI_Request :out))

;; int PMPI_File_seek(MPI_File, MPI_Offset, int);
(declaim (inline PMPI_File_seek))
(define-alien-routine ("PMPI_File_seek" PMPI_File_seek)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (whence int))

;; int PMPI_File_get_position(MPI_File, MPI_Offset *);
(declaim (inline PMPI_File_get_position))
(define-alien-routine ("PMPI_File_get_position" PMPI_File_get_position)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset :out))

;; int PMPI_File_get_byte_offset(MPI_File, MPI_Offset, MPI_Offset *);
(declaim (inline PMPI_File_get_byte_offset))
(define-alien-routine ("PMPI_File_get_byte_offset" PMPI_File_get_byte_offset)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (disp MPI_Offset :out))

;; /* Section 9.4.4 */
;; int PMPI_File_read_shared(MPI_File, void *, int, MPI_Datatype, MPI_Status *);
(declaim (inline PMPI_File_read_shared))
(define-alien-routine ("PMPI_File_read_shared" PMPI_File_read_shared)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int PMPI_File_write_shared(MPI_File, void *, int, MPI_Datatype,
;;                            MPI_Status *);
(declaim (inline PMPI_File_write_shared))
(define-alien-routine ("PMPI_File_write_shared" PMPI_File_write_shared)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int PMPI_File_iread_shared(MPI_File, void *, int, MPI_Datatype,
;;                           MPIO_Request *);
(declaim (inline PMPI_File_iread_shared))
(define-alien-routine ("PMPI_File_iread_shared" PMPI_File_iread_shared)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (request MPI_Request :out))
    
;; int PMPI_File_iwrite_shared(MPI_File, void *, int, 
;; 			   MPI_Datatype, MPIO_Request *);
(declaim (inline PMPI_File_iwrite_shared))
(define-alien-routine ("PMPI_File_iwrite_shared" PMPI_File_iwrite_shared)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (request MPI_Request :out))

;; int PMPI_File_read_ordered(MPI_File, void *, int, 
;;                           MPI_Datatype, MPI_Status *);
(declaim (inline PMPI_File_read_ordered))
(define-alien-routine ("PMPI_File_read_ordered" PMPI_File_read_ordered)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int PMPI_File_write_ordered(MPI_File, void *, int, 
;;                            MPI_Datatype, MPI_Status *);
(declaim (inline PMPI_File_write_ordered))
(define-alien-routine ("PMPI_File_write_ordered" PMPI_File_write_ordered)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (status MPI_Status :out))

;; int PMPI_File_seek_shared(MPI_File, MPI_Offset, int);
(declaim (inline PMPI_File_seek_shared))
(define-alien-routine ("PMPI_File_seek_shared" PMPI_File_seek_shared)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (whence int))

;; int PMPI_File_get_position_shared(MPI_File, MPI_Offset *);
(declaim (inline PMPI_File_get_position_shared))
(define-alien-routine ("PMPI_File_get_position_shared"
		       PMPI_File_get_position_shared)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset :out))

;; /* Section 9.4.5 */
;; int PMPI_File_read_at_all_begin(MPI_File, MPI_Offset, void *,
;;                                int, MPI_Datatype);
(declaim (inline PMPI_File_read_at_all_begin))
(define-alien-routine ("PMPI_File_read_at_all_begin"
		       PMPI_File_read_at_all_begin)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype))

;; int PMPI_File_read_at_all_end(MPI_File, void *, MPI_Status *);
(declaim (inline PMPI_File_read_at_all_end))
(define-alien-routine ("PMPI_File_read_at_all_end" PMPI_File_read_at_all_end)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (status MPI_Status :out))

;; int PMPI_File_write_at_all_begin(MPI_File, MPI_Offset, void *,
;;                                 int, MPI_Datatype);
(declaim (inline PMPI_File_write_at_all_begin))
(define-alien-routine ("PMPI_File_write_at_all_begin"
		       PMPI_File_write_at_all_begin)
    int
  (mpi_fh MPI_File)
  (offset MPI_Offset)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype))

;; int PMPI_File_write_at_all_end(MPI_File, void *, MPI_Status *);
(declaim (inline PMPI_File_write_at_all_end))
(define-alien-routine ("PMPI_File_write_at_all_end" PMPI_File_write_at_all_end)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (status MPI_Status :out))

;; int PMPI_File_read_all_begin(MPI_File, void *, int, MPI_Datatype);
(declaim (inline PMPI_File_read_all_begin))
(define-alien-routine ("PMPI_File_read_all_begin" PMPI_File_read_all_begin)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype))

;; int PMPI_File_read_all_end(MPI_File, void *, MPI_Status *);
(declaim (inline PMPI_File_read_all_end))
(define-alien-routine ("PMPI_File_read_all_end" PMPI_File_read_all_end)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (status MPI_Status :out))

;; int PMPI_File_write_all_begin(MPI_File, void *, int, MPI_Datatype);
(declaim (inline PMPI_File_write_all_begin))
(define-alien-routine ("PMPI_File_write_all_begin" PMPI_File_write_all_begin)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype))

;; int PMPI_File_write_all_end(MPI_File, void *, MPI_Status *);
(declaim (inline PMPI_File_write_all_end))
(define-alien-routine ("PMPI_File_write_all_end" PMPI_File_write_all_end)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (status MPI_Status :out))

;; int PMPI_File_read_ordered_begin(MPI_File, void *, int, MPI_Datatype);
(declaim (inline PMPI_File_read_ordered_begin))
(define-alien-routine ("PMPI_File_read_ordered_begin"
		       PMPI_File_read_ordered_begin)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype))

;; int PMPI_File_read_ordered_end(MPI_File, void *, MPI_Status *);
(declaim (inline PMPI_File_read_ordered_end))
(define-alien-routine ("PMPI_File_read_ordered_end" PMPI_File_read_ordered_end)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (status MPI_Status :out))

;; int PMPI_File_write_ordered_begin(MPI_File, void *, int, MPI_Datatype);
(declaim (inline PMPI_File_write_ordered_begin))
(define-alien-routine ("PMPI_File_write_ordered_begin"
		       PMPI_File_write_ordered_begin)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (count int)
  (datatype MPI_Datatype))

;; int PMPI_File_write_ordered_end(MPI_File, void *, MPI_Status *);
(declaim (inline PMPI_File_write_ordered_end))
(define-alien-routine ("PMPI_File_write_ordered_end"
		       PMPI_File_write_ordered_end)
    int
  (mpi_fh MPI_File)
  (buf (* t))
  (status MPI_Status :out))


;; /* Section 9.5.1 */
;; int PMPI_File_get_type_extent(MPI_File, MPI_Datatype, MPI_Aint *);
(declaim (inline PMPI_File_get_type_extent))
(define-alien-routine ("PMPI_File_get_type_extent" PMPI_File_get_type_extent)
    int
  (mpi_fh MPI_File)
  (datatype MPI_Datatype)
  (extent MPI_Aint :out))


;; /* Section 9.5.3 */
;; int PMPI_Register_datarep(char *,
;; 			 MPI_Datarep_conversion_function *,
;; 			 MPI_Datarep_conversion_function *,
;; 			 MPI_Datarep_extent_function *,
;; 			 void *);
(declaim (inline PMPI_Register_datarep))
(define-alien-routine ("PMPI_Register_datarep" PMPI_Register_datarep)
    int
  (name c-string)
  (read_conv_fn MPI_Datarep_conversion_function)
  (write_conv_fn MPI_Datarep_conversion_function)
  (extent_fn MPI_Datarep_extent_function)
  (state (* t)))

;; /* Section 9.6.1 */
;; int PMPI_File_set_atomicity(MPI_File, int);
(declaim (inline PMPI_File_set_atomicity))
(define-alien-routine ("PMPI_File_set_atomicity" PMPI_File_set_atomicity)
    int
  (mpi_fh MPI_File)
  (flag int))

;; int PMPI_File_get_atomicity(MPI_File, int *);
(declaim (inline PMPI_File_get_atomicity))
(define-alien-routine ("PMPI_File_get_atomicity" PMPI_File_get_atomicity)
    int
  (mpi_fh MPI_File)
  (flag int :out))

;; int PMPI_File_sync(MPI_File);
(declaim (inline PMPI_File_sync))
(define-alien-routine ("PMPI_File_sync" PMPI_File_sync)
    int
  (mpi_fh MPI_File))

;; /* Section 4.13.3 */
;; #ifndef MPICH2
;; /* MPICH2 provides these definitions */
;; int PMPI_File_set_errhandler( MPI_File, MPI_Errhandler );
;; int PMPI_File_get_errhandler( MPI_File, MPI_Errhandler * );
;; #endif

;; #ifndef HAVE_MPI_DARRAY_SUBARRAY
;; /* Section 4.14.4 */
;; int PMPI_Type_create_subarray(int, int *, int *, int *, int, 
;;                       MPI_Datatype, MPI_Datatype *);

;; /* Section 4.14.5 */
;; int PMPI_Type_create_darray(int, int, int, int *, int *, 
;;                     int *, int *, int, MPI_Datatype, MPI_Datatype *);
;; #endif

;; /* Section 4.12.4 */
;; MPI_File PMPI_File_f2c(MPI_Fint);
(declaim (inline PMPI_File_f2c))
(define-alien-routine ("PMPI_File_f2c" PMPI_File_f2c)
    MPI_File
  (fh MPI_Fint))

;; MPI_Fint PMPI_File_c2f(MPI_File);
(declaim (inline PMPI_File_c2f))
(define-alien-routine ("PMPI_File_c2f" PMPI_File_c2f)
    MPI_Fint
  (mpi_fh MPI_File))

;; #ifndef HAVE_MPI_GREQUEST
;; /* The following functions are required if generalized requests are not
;;    available, because in that case, an MPIO_Request object
;;    is currently used for nonblocking I/O. */
;; int PMPIO_Test(MPIO_Request *, int *, MPI_Status *);
;; int PMPIO_Wait(MPIO_Request *, MPI_Status *);
;; int PMPIO_Testall(int, MPIO_Request *, int *, MPI_Status *);
;; int PMPIO_Waitall(int, MPIO_Request *, MPI_Status *);
;; int PMPIO_Testany(int, MPIO_Request *, int *, int *, MPI_Status *);
;; int PMPIO_Waitany(int, MPIO_Request *, int *, MPI_Status *);
;; int PMPIO_Waitsome(int, MPIO_Request *, int *, int *, MPI_Status *);
;; int PMPIO_Testsome(int, MPIO_Request *, int *, int *, MPI_Status *);
;; MPI_Fint PMPIO_Request_c2f(MPIO_Request);
;; MPIO_Request PMPIO_Request_f2c(MPI_Fint);
;; #endif /* HAVE_MPI_GREQUEST */

;; /* info functions if not defined in the MPI implementation */
;; #ifndef HAVE_MPI_INFO

;; int PMPI_Info_create(MPI_Info *);
;; int PMPI_Info_set(MPI_Info, char *, char *);
;; int PMPI_Info_delete(MPI_Info, char *);
;; int PMPI_Info_get(MPI_Info, char *, int, char *, int *);
;; int PMPI_Info_get_valuelen(MPI_Info, char *, int *, int *);
;; int PMPI_Info_get_nkeys(MPI_Info, int *);
;; int PMPI_Info_get_nthkey(MPI_Info, int, char *);
;; int PMPI_Info_dup(MPI_Info, MPI_Info *);
;; int PMPI_Info_free(MPI_Info *);

;; MPI_Fint PMPI_Info_c2f(MPI_Info);
;; MPI_Info PMPI_Info_f2c(MPI_Fint);
;; #endif

;; #if defined(__cplusplus)
;; }
;; #endif

;; #endif
