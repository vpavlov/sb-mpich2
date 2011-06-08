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
;;;; This is an almost verbatim translation of <mpi.h>. It is based on the
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
;;  *  (C) 2001 by Argonne National Laboratory.
;;  *      See COPYRIGHT in top-level directory.
;;  */
;; /* src/include/mpi.h.  Generated from mpi.h.in by configure. */
;; #ifndef MPI_INCLUDED
;; #define MPI_INCLUDED

;; /* user include file for MPI programs */

;; /* Keep C++ compilers from getting confused */
;; #if defined(__cplusplus)
;; extern "C" {
;; #endif

;; /* Results of the compare operations. */
(defconstant MPI_IDENT      0)
(defconstant MPI_CONGRUENT  1)
(defconstant MPI_SIMILAR    2)
(defconstant MPI_UNEQUAL    3)

(define-alien-type MPI_Datatype int)

(defconstant MPI_CHAR                #x4c000101)
(defconstant MPI_SIGNED_CHAR         #x4c000118)
(defconstant MPI_UNSIGNED_CHAR       #x4c000102)
(defconstant MPI_BYTE                #x4c00010d)
(defconstant MPI_WCHAR               #x4c00040e)
(defconstant MPI_SHORT               #x4c000203)
(defconstant MPI_UNSIGNED_SHORT      #x4c000204)
(defconstant MPI_INT                 #x4c000405)
(defconstant MPI_UNSIGNED            #x4c000406)
(defconstant MPI_LONG                #x4c000407)
(defconstant MPI_UNSIGNED_LONG       #x4c000408)
(defconstant MPI_FLOAT               #x4c00040a)
(defconstant MPI_DOUBLE              #x4c00080b)
(defconstant MPI_LONG_DOUBLE         #x4c00100c)
(defconstant MPI_LONG_LONG_INT       #x4c000809)
(defconstant MPI_UNSIGNED_LONG_LONG  #x4c000819)
(defconstant MPI_LONG_LONG           MPI_LONG_LONG_INT)

(defconstant MPI_PACKED              #x4c00010f)
(defconstant MPI_LB                  #x4c000010)
(defconstant MPI_UB                  #x4c000011)

;; /* 
;;    The layouts for the types MPI_DOUBLE_INT etc are simply
;;    struct { 
;;        double var;
;;        int    loc;
;;    }
;;    This is documented in the man pages on the various datatypes.   
;;  */
(defconstant MPI_FLOAT_INT           #x8c000000)
(defconstant MPI_DOUBLE_INT          #x8c000001)
(defconstant MPI_LONG_INT            #x8c000002)
(defconstant MPI_SHORT_INT           #x8c000003)
(defconstant MPI_2INT                #x4c000816)
(defconstant MPI_LONG_DOUBLE_INT     #x8c000004)

;; /* Fortran types */
(defconstant MPI_COMPLEX             1275070494)
(defconstant MPI_DOUBLE_COMPLEX      1275072546)
(defconstant MPI_LOGICAL             1275069469)
(defconstant MPI_REAL                1275069468)
(defconstant MPI_DOUBLE_PRECISION    1275070495)
(defconstant MPI_INTEGER             1275069467)
(defconstant MPI_2INTEGER            1275070496)
(defconstant MPI_2COMPLEX            1275072548)
(defconstant MPI_2DOUBLE_COMPLEX     1275076645)
(defconstant MPI_2REAL               1275070497)
(defconstant MPI_2DOUBLE_PRECISION   1275072547)
(defconstant MPI_CHARACTER           1275068698)

;; /* Size-specific types (see MPI-2, 10.2.5) */
(defconstant MPI_REAL4               #x4c000427)
(defconstant MPI_REAL8               #x4c000829)
(defconstant MPI_REAL16              #x4c00102b)
(defconstant MPI_COMPLEX8            #x4c000828)
(defconstant MPI_COMPLEX16           #x4c00102a)
(defconstant MPI_COMPLEX32           #x4c00202c)
(defconstant MPI_INTEGER1            #x4c00012d)
(defconstant MPI_INTEGER2            #x4c00022f)
(defconstant MPI_INTEGER4            #x4c000430)
(defconstant MPI_INTEGER8            #x4c000831)

;; [VNP] This used to be forward decl a little bit down earlier, so I moved it
;;       here to satisfy MPI_INTEGER16
(defconstant MPI_DATATYPE_NULL       #x0c000000)

(defconstant MPI_INTEGER16           MPI_DATATYPE_NULL)

;; /* typeclasses */
(defconstant MPI_TYPECLASS_REAL     1)
(defconstant MPI_TYPECLASS_INTEGER  2)
(defconstant MPI_TYPECLASS_COMPLEX  3)

;; /* Communicators */
;; @@@REMOVE: this went to sb-mpich2.asd
;; (define-alien-type MPI_Comm int)
;; (defconstant MPI_COMM_WORLD  #x44000000)
(defconstant MPI_COMM_SELF   #x44000001)

;; /* Groups */
(define-alien-type MPI_Group int)
(defconstant MPI_GROUP_EMPTY  #x48000000)

;; /* RMA and Windows */
(define-alien-type MPI_Win int)
(defconstant MPI_WIN_NULL  #x20000000)

;; /* File and IO */
;; /* This define lets ROMIO know that MPI_File has been defined */
;; #define MPI_FILE_DEFINED
;; /* ROMIO uses a pointer for MPI_File objects.  This must be the same
;;    definition as in src/mpi/romio/include/mpio.h.in  */
(define-alien-type MPI_File int)   ;; [VNP] Actually (* (struct ADIOI_FileD))
(defconstant MPI_FILE_NULL  0)

;; /* Collective operations */
(define-alien-type MPI_Op int)

(defconstant MPI_MAX      #x58000001)
(defconstant MPI_MIN      #x58000002)
(defconstant MPI_SUM      #x58000003)
(defconstant MPI_PROD     #x58000004)
(defconstant MPI_LAND     #x58000005)
(defconstant MPI_BAND     #x58000006)
(defconstant MPI_LOR      #x58000007)
(defconstant MPI_BOR      #x58000008)
(defconstant MPI_LXOR     #x58000009)
(defconstant MPI_BXOR     #x5800000a)
(defconstant MPI_MINLOC   #x5800000b)
(defconstant MPI_MAXLOC   #x5800000c)
(defconstant MPI_REPLACE  #x5800000d)

;; /* Permanent key values */
;; /* C Versions (return pointer to value),
;;    Fortran Versions (return integer value).
;;    Handled directly by the attribute value routine
   
;;    DO NOT CHANGE THESE.  The values encode:
;;    builtin kind (0x1 in bit 30-31)
;;    Keyval object (0x9 in bits 26-29)
;;    for communicator (0x1 in bits 22-25)
   
;;    Fortran versions of the attributes are formed by adding one to
;;    the C version.
;;  */
(defconstant MPI_TAG_UB           #x64400001)
(defconstant MPI_HOST             #x64400003)
(defconstant MPI_IO               #x64400005)
(defconstant MPI_WTIME_IS_GLOBAL  #x64400007)
(defconstant MPI_UNIVERSE_SIZE    #x64400009)
(defconstant MPI_LASTUSEDCODE     #x6440000b)
(defconstant MPI_APPNUM           #x6440000d)

;; /* In addition, there are 3 predefined window attributes that are
;;    defined for every window */
(defconstant MPI_WIN_BASE         #x66000001)
(defconstant MPI_WIN_SIZE         #x66000003)
(defconstant MPI_WIN_DISP_UNIT    #x66000005)

;; /* Define some null objects */
(defconstant MPI_COMM_NULL        #x04000000)
(defconstant MPI_OP_NULL          #x18000000)
(defconstant MPI_GROUP_NULL       #x08000000)
;; (defconstant MPI_DATATYPE_NULL  ((MPI_Datatype)0x0c000000) [VNP] moved up
(defconstant MPI_REQUEST_NULL     #x2c000000)
(defconstant MPI_ERRHANDLER_NULL  #x14000000)

;; /* These are only guesses; make sure you change them in mpif.h as well */
(defconstant MPI_MAX_PROCESSOR_NAME  128)
(defconstant MPI_MAX_ERROR_STRING   1024)
(defconstant MPI_MAX_PORT_NAME       256)
(defconstant MPI_MAX_OBJECT_NAME     128)

;; /* Pre-defined constants */
(defconstant MPI_UNDEFINED       -32766)
(defconstant MPI_UNDEFINED_RANK  MPI_UNDEFINED)
(defconstant MPI_KEYVAL_INVALID  #x24000000)

;; /* Upper bound on the overhead in bsend for each message buffer */
(defconstant MPI_BSEND_OVERHEAD  56)

;; /* Topology types */
(define-alien-type MPIR_Topo_type
    (enum MPIR_Topo_type
	  (MPI_GRAPH 1)
	  (MPI_CART 2)))

(defconstant MPI_BOTTOM      0)

(defconstant MPI_PROC_NULL   -1)
(defconstant MPI_ANY_SOURCE  -2)
(defconstant MPI_ROOT        -3)
(defconstant MPI_ANY_TAG     -1)

(defconstant MPI_LOCK_EXCLUSIVE  234)
(defconstant MPI_LOCK_SHARED     235)

;; [VNP] Oops. functions with variable number of arguments are not supported
;;       per-se on SBCL. So, looking at mpich2's implementation, we find out
;;       that there are no other arguments passed when the handler is called.
;;       This means just ignore the ... in MPI_Handler_function,
;;       MPI_Comm_errhandler_fn, MPI_File_errhandler_fn and
;;       MPI_Win_errhandler_fn

;; /* C functions */
(define-alien-type MPI_Handler_function
    (* (function void (* MPI_Comm) (* int))))
(define-alien-type MPI_Comm_copy_attr_function
    (* (function int MPI_Comm int (* t) (* t) (* t) (* int))))
(define-alien-type MPI_Comm_delete_attr_function
    (* (function int MPI_Comm (* t) (* t))))
(define-alien-type MPI_Type_copy_attr_function
    (* (function int MPI_Datatype int (* t) (* t) (* t) (* int))))
(define-alien-type MPI_Type_delete_attr_function
    (* (function int MPI_Datatype int (* t) (* t))))
(define-alien-type MPI_Win_copy_attr_function
    (* (function int MPI_Win int (* t) (* t) (* t) (* int))))
(define-alien-type MPI_Win_delete_attr_function
    (* (function int MPI_Win int (* t) (* t))))
(define-alien-type MPI_Comm_errhandler_fn
    (* (function void (* MPI_Comm) (* int))))
(define-alien-type MPI_File_errhandler_fn
    (* (function void (* MPI_Comm) (* int))))
(define-alien-type MPI_Win_errhandler_fn
    (* (function void (* MPI_Comm) (* int))))

;; /* Built in (0x1 in 30-31), errhandler (0x5 in bits 26-29, allkind (0
;;    in 22-25), index in the low bits */
 (defconstant MPI_ERRORS_ARE_FATAL #x54000000)
 (defconstant MPI_ERRORS_RETURN    #x54000001)
;; /* MPIR_ERRORS_THROW_EXCEPTIONS is not part of the MPI standard, it is here
;;    to facilitate the c++ binding which has MPI::ERRORS_THROW_EXCEPTIONS. 
;;    Using the MPIR prefix preserved the MPI_ names for objects defined by
;;    the standard. */
(defconstant MPIR_ERRORS_THROW_EXCEPTIONS #x54000002)

(define-alien-type MPI_Errhandler int)

;; [VNP] moved from way below in order to satisfy forward references
;; int MPI_DUP_FN ( MPI_Comm, int, void *, void *, void *, int * );
(define-alien-routine ("MPIR_Dup_fn" MPIR_Dup_fn)
    int
  (comm MPI_Comm)
  (key int)
  (extra (* t))
  (attrin (* t))
  (attrout (* t))
  (flag int :out))

;; /* Make the C names for the dup function mixed case.
;;    This is required for systems that use all uppercase names for Fortran 
;;    externals.  */
;; /* MPI 1 names */
(defconstant MPI_NULL_COPY_FN nil)
(defconstant MPI_NULL_DELETE_FN nil)
;; #define MPI_DUP_FN         MPIR_Dup_fn  /* [VNP] handle manually */
;; /* MPI 2 names */
(defconstant MPI_COMM_NULL_COPY_FN    nil)
(defconstant MPI_COMM_NULL_DELETE_FN  nil)
(defparameter MPI_COMM_DUP_FN          #'MPIR_Dup_fn)
(defconstant MPI_WIN_NULL_COPY_FN     nil)
(defconstant MPI_WIN_NULL_DELETE_FN   nil)
(defparameter MPI_WIN_DUP_FN           #'MPIR_Dup_fn)
(defconstant MPI_TYPE_NULL_COPY_FN    nil)
(defconstant MPI_TYPE_NULL_DELETE_FN  nil)
(defparameter MPI_TYPE_DUP_FN          #'MPIR_Dup_fn)

;; /* MPI request opjects */
(define-alien-type MPI_Request int)

;; /* User combination function */
(define-alien-type MPI_User_function
    (* (function void (* t) (* t) (* int) (* MPI_Datatype))))

;; /* MPI Attribute copy and delete functions */
(define-alien-type MPI_Copy_function
    (* (function int MPI_Comm int (* t) (* t) (* t) (* int))))
(define-alien-type MPI_Delete_function
    (* (function int MPI_Comm int (* t) (* t))))

(defconstant MPI_VERSION    2)
(defconstant MPI_SUBVERSION 1)
(defconstant MPICH_NAME     2)
(defconstant MPICH2         1)
(defconstant MPICH_HAS_C2F  1)


;; /* MPICH2_VERSION is the version string. MPICH2_NUMVERSION is the
;;  * numeric version that can be used in numeric comparisons.
;;  *
;;  * MPICH2_VERSION uses the following format:
;;  * Version: [MAJ].[MIN].[REV][EXT][EXT_NUMBER]
;;  * Example: 1.0.7rc1 has
;;  *          MAJ = 1
;;  *          MIN = 0
;;  *          REV = 7
;;  *          EXT = rc
;;  *          EXT_NUMBER = 1
;;  *
;;  * MPICH2_NUMVERSION will convert EXT to a format number:
;;  *          ALPHA (a) = 0
;;  *          BETA (b)  = 1
;;  *          RC (rc)   = 2
;;  *          PATCH (p) = 3
;;  * Regular releases are treated as patch 0
;;  *
;;  * Numeric version will have 1 digit for MAJ, 2 digits for MIN, 2
;;  * digits for REV, 1 digit for EXT and 2 digits for EXT_NUMBER. So,
;;  * 1.0.7rc1 will have the numeric version 10007201.
;;  */

;; [VNP] defconstant with a string constants breaks loading in SBCL, so I
;;       switched to defparameter
(defparameter MPICH2_VERSION "1.1")
(defconstant MPICH2_NUMVERSION 10100300)

(defconstant MPICH2_RELEASE_TYPE_ALPHA  0)
(defconstant MPICH2_RELEASE_TYPE_BETA   1)
(defconstant MPICH2_RELEASE_TYPE_RC     2)
(defconstant MPICH2_RELEASE_TYPE_PATCH  3)

(defmacro MPICH2_CALC_VERSION (major minor revision type patch)
  `(+ (* ,major 10000000)
      (* ,minor 100000)
      (* ,revision 1000)
      (* ,type 100)
      ,patch))

;; /* for the datatype decoders */
(define-alien-type MPI_COMBINER_ENUM
    (enum MPI_COMBINER_ENUM
	  (MPI_COMBINER_NAMED             1)
	  (MPI_COMBINER_DUP               2)
	  (MPI_COMBINER_CONTIGUOUS        3)
	  (MPI_COMBINER_VECTOR            4)
	  (MPI_COMBINER_HVECTOR_INTEGER   5)
	  (MPI_COMBINER_HVECTOR           6)
	  (MPI_COMBINER_INDEXED           7)
	  (MPI_COMBINER_HINDEXED_INTEGER  8)
	  (MPI_COMBINER_HINDEXED          9)
	  (MPI_COMBINER_INDEXED_BLOCK     10)
	  (MPI_COMBINER_STRUCT_INTEGER    11)
	  (MPI_COMBINER_STRUCT            12)
	  (MPI_COMBINER_SUBARRAY          13)
	  (MPI_COMBINER_DARRAY            14)
	  (MPI_COMBINER_F90_REAL          15)
	  (MPI_COMBINER_F90_COMPLEX       16)
	  (MPI_COMBINER_F90_INTEGER       17)
	  (MPI_COMBINER_RESIZED           18)))

;; /* for info */
(define-alien-type MPI_Info int)
(defconstant MPI_INFO_NULL         #x1c000000)
(defconstant MPI_MAX_INFO_KEY       255)
(defconstant MPI_MAX_INFO_VAL      1024)

;; /* for subarray and darray constructors */
(defconstant MPI_ORDER_C              56)
(defconstant MPI_ORDER_FORTRAN        57)
(defconstant MPI_DISTRIBUTE_BLOCK    121)
(defconstant MPI_DISTRIBUTE_CYCLIC   122)
(defconstant MPI_DISTRIBUTE_NONE     123)
(defconstant MPI_DISTRIBUTE_DFLT_DARG -49767)

(defconstant MPI_IN_PLACE  -1)

;; /* asserts for one-sided communication */
(defconstant MPI_MODE_NOCHECK      1024)
(defconstant MPI_MODE_NOSTORE      2048)
(defconstant MPI_MODE_NOPUT        4096)
(defconstant MPI_MODE_NOPRECEDE    8192)
(defconstant MPI_MODE_NOSUCCEED   16384)

;; /* Definitions that are determined by configure. */
(define-alien-type MPI_Aint long-long)
(define-alien-type MPI_Fint int)
;; /* FIXME: The following two definition are not defined by MPI and must not be
;;    included in the mpi.h file, as the MPI namespace is reserved to the MPI 
;;    standard */
;; [VNP] and frankly, we don't need them in lisp, so don't define them at all
;; #define MPI_AINT_FMT_DEC_SPEC "%lld"
;; #define MPI_AINT_FMT_HEX_SPEC "%#llx"

;; /* Let ROMIO know that MPI_Offset is already defined */
;; #defconstant HAVE_MPI_OFFSET
;; /* MPI_OFFSET_TYPEDEF is set in configure and is 
;;       typedef $MPI_OFFSET MPI_Offset;
;;    where $MPI_OFFSET is the correct C type */
;; [VNP] KLUDGE: I checked, this is long long in both BG/P and my x86_64
;;       machine. Be ware -- it might be different on other platforms !!!
(define-alien-type MPI_Offset long-long)

;; /* The order of these elements must match that in mpif.h */
(define-alien-type MPI_Status
    (struct MPI_Status
	    (count int)
	    (cancelled int)
	    (MPI_SOURCE int)
	    (MPI_TAG int)
	    (MPI_ERROR int)))

;; /* Handle conversion types/functions */

;; /* Programs that need to convert types used in MPICH should use these */
(defmacro MPI_Comm_c2f (comm) comm)
(defmacro MPI_Comm_f2c (comm) comm)
(defmacro MPI_Type_c2f (datatype) datatype)
(defmacro MPI_Type_f2c (datatype) datatype)
(defmacro MPI_Group_c2f (group) group)
(defmacro MPI_Group_f2c (group) group)
(defmacro MPI_Info_c2f (info) info)
(defmacro MPI_Info_f2c (info) info)
(defmacro MPI_Request_c2f (request) request)
(defmacro MPI_Request_f2c (request) request)
(defmacro MPI_Op_c2f (op) op)
(defmacro MPI_Op_f2c (op) op)
(defmacro MPI_Errhandler_c2f (errhandler) errhandler)
(defmacro MPI_Errhandler_f2c (errhandler) errhandler)
(defmacro MPI_Win_c2f (win) win)
(defmacro MPI_Win_f2c (win) win)

;; /* PMPI versions of the handle transfer functions.  See section 4.17 */
(defmacro PMPI_Comm_c2f (comm) comm)
(defmacro PMPI_Comm_f2c (comm) comm)
(defmacro PMPI_Type_c2f (datatype) datatype)
(defmacro PMPI_Type_f2c (datatype) datatype)
(defmacro PMPI_Group_c2f (group) group)
(defmacro PMPI_Group_f2c (group) group)
(defmacro PMPI_Info_c2f (info) info)
(defmacro PMPI_Info_f2c (info) info)
(defmacro PMPI_Request_c2f (request) request)
(defmacro PMPI_Request_f2c (request) request)
(defmacro PMPI_Op_c2f (op) op)
(defmacro PMPI_Op_f2c (op) op)
(defmacro PMPI_Errhandler_c2f (errhandler) errhandler)
(defmacro PMPI_Errhandler_f2c (errhandler) errhandler)
(defmacro PMPI_Win_c2f (win) win)
(defmacro PMPI_Win_f2c (win) win)

(defconstant MPI_STATUS_IGNORE 1)
(defconstant MPI_STATUSES_IGNORE 1)
(defconstant MPI_ERRCODES_IGNORE 0)

;; [VNP] I don't see how this applies to lisp 
;; /* See 4.12.5 for MPI_F_STATUS(ES)_IGNORE */
;; #define MPIU_DLL_SPEC
;; extern MPIU_DLL_SPEC MPI_Fint * MPI_F_STATUS_IGNORE;
;; extern MPIU_DLL_SPEC MPI_Fint * MPI_F_STATUSES_IGNORE;
;; /* The annotation MPIU_DLL_SPEC to the extern statements is used 
;;    as a hook for systems that require C extensions to correctly construct
;;    DLLs, and is defined as an empty string otherwise
;;  */

;; /* The MPI standard requires that the ARGV_NULL values be the same as
;;    NULL (see 5.3.2) */
(defconstant MPI_ARGV_NULL 0)
(defconstant MPI_ARGVS_NULL 0)

;; /* For supported thread levels */
(defconstant MPI_THREAD_SINGLE 0)
(defconstant MPI_THREAD_FUNNELED 1)
(defconstant MPI_THREAD_SERIALIZED 2)
;; @@@REMOVE: this went to sb-mpich2.asd
;; (defconstant MPI_THREAD_MULTIPLE 3)

;; /* Typedefs for generalized requests */
(define-alien-type MPI_Grequest_cancel_function
    (* (function int (* t) int)))
(define-alien-type MPI_Grequest_free_function
    (* (function int (* t))))
(define-alien-type MPI_Grequest_query_function
    (* (function int (* t) (* MPI_Status))))

;; /* MPI's error classes */

;; @@@REMOVE: this went to sb-mpich2.asd
;;(defconstant MPI_SUCCESS          0)      ; /* Successful return code */

;; /* Communication argument parameters */
(defconstant MPI_ERR_BUFFER       1)      ; /* Invalid buffer pointer */
(defconstant MPI_ERR_COUNT        2)      ; /* Invalid count argument */
(defconstant MPI_ERR_TYPE         3)      ; /* Invalid datatype argument */
(defconstant MPI_ERR_TAG          4)      ; /* Invalid tag argument */
(defconstant MPI_ERR_COMM         5)      ; /* Invalid communicator */
(defconstant MPI_ERR_RANK         6)      ; /* Invalid rank */
(defconstant MPI_ERR_ROOT         7)      ; /* Invalid root */
(defconstant MPI_ERR_TRUNCATE    14)      ; /* Message truncated on receive */

;; /* MPI Objects (other than COMM) */
(defconstant MPI_ERR_GROUP        8)      ; /* Invalid group */
(defconstant MPI_ERR_OP           9)      ; /* Invalid operation */
(defconstant MPI_ERR_REQUEST     19)      ; /* Invalid mpi_request handle */

;; /* Special topology argument parameters */
(defconstant MPI_ERR_TOPOLOGY    10)      ; /* Invalid topology */
(defconstant MPI_ERR_DIMS        11)      ; /* Invalid dimension argument */

;; /* All other arguments.  This is a class with many kinds */
(defconstant MPI_ERR_ARG         12)      ; /* Invalid argument */

;; /* Other errors that are not simply an invalid argument */
(defconstant MPI_ERR_OTHER       15)      ; /* Other error; use Error_string */

(defconstant MPI_ERR_UNKNOWN     13)      ; /* Unknown error */
(defconstant MPI_ERR_INTERN      16)      ; /* Internal error code    */

;; /* Multiple completion has two special error classes */
(defconstant MPI_ERR_IN_STATUS   17)      ; /* Look in status for error value */
(defconstant MPI_ERR_PENDING     18)      ; /* Pending request */

;; /* New MPI-2 Error classes */
(defconstant MPI_ERR_FILE        27)      ; /* */
(defconstant MPI_ERR_ACCESS      20)      ; /* */
(defconstant MPI_ERR_AMODE       21)      ; /* */
(defconstant MPI_ERR_BAD_FILE    22)      ; /* */
(defconstant MPI_ERR_FILE_EXISTS 25)      ; /* */
(defconstant MPI_ERR_FILE_IN_USE 26)      ; /* */
(defconstant MPI_ERR_NO_SPACE    36)      ; /* */
(defconstant MPI_ERR_NO_SUCH_FILE 37)     ; /* */
(defconstant MPI_ERR_IO          32)      ; /* */
(defconstant MPI_ERR_READ_ONLY   40)      ; /* */
(defconstant MPI_ERR_CONVERSION  23)      ; /* */
(defconstant MPI_ERR_DUP_DATAREP 24)      ; /* */
(defconstant MPI_ERR_UNSUPPORTED_DATAREP   43)  ; /* */

;; /* MPI_ERR_INFO is NOT defined in the MPI-2 standard.  I believe that
;;    this is an oversight */
(defconstant MPI_ERR_INFO        28)      ; /* */
(defconstant MPI_ERR_INFO_KEY    29)      ; /* */
(defconstant MPI_ERR_INFO_VALUE  30)      ; /* */
(defconstant MPI_ERR_INFO_NOKEY  31)      ; /* */

(defconstant MPI_ERR_NAME        33)      ; /* */
(defconstant MPI_ERR_NO_MEM      34)      ; /* Alloc_mem could not allocate */
(defconstant MPI_ERR_NOT_SAME    35)      ; /* */
(defconstant MPI_ERR_PORT        38)      ; /* */
(defconstant MPI_ERR_QUOTA       39)      ; /* */
(defconstant MPI_ERR_SERVICE     41)      ; /* */
(defconstant MPI_ERR_SPAWN       42)      ; /* */
(defconstant MPI_ERR_UNSUPPORTED_OPERATION 44) ; /* */
(defconstant MPI_ERR_WIN         45)      ; /* */

(defconstant MPI_ERR_BASE        46)      ; /* */
(defconstant MPI_ERR_LOCKTYPE    47)      ; /* */
(defconstant MPI_ERR_KEYVAL      48)      ; /* Erroneous attribute key */
(defconstant MPI_ERR_RMA_CONFLICT 49)     ; /* */
(defconstant MPI_ERR_RMA_SYNC    50)      ; /* */ 
(defconstant MPI_ERR_SIZE        51)      ; /* */
(defconstant MPI_ERR_DISP        52)      ; /* */
(defconstant MPI_ERR_ASSERT      53)      ; /* */

(defconstant MPI_ERR_LASTCODE    #x3fffffff)  ; /* Last valid error code for a 
;; 					   predefined error class */
(defconstant MPICH_ERR_LAST_CLASS 53)     ; /* It is also helpful to know the
;; 				       last valid class */
;; /* End of MPI's error classes */

;; /* Function type defs */
(define-alien-type MPI_Datarep_conversion_function
    (* (function int (* t) MPI_Datatype int (* t) MPI_Offset (* t))))
(define-alien-type MPI_Datarep_extent_function
    (* (function int MPI_Datatype (* MPI_Aint) (* t))))
(defconstant MPI_CONVERSION_FN_NULL nil)

;; /* 
;;    For systems that may need to add additional definitions to support
;;    different declaration styles and options (e.g., different calling 
;;    conventions or DLL import/export controls).  
;; */
;; /* --Insert Additional Definitions Here-- */

;; /* Include any device specific definitions */
;; /* ... no device specific definitions ... */

;; /*
;;  * Normally, we provide prototypes for all MPI routines.  In a few wierd
;;  * cases, we need to suppress the prototypes.
;;  */
;; #ifndef MPICH_SUPPRESS_PROTOTYPES
;; /* We require that the C compiler support prototypes */
;; /* Begin Prototypes */

;; int MPI_Send(void*, int, MPI_Datatype, int, int, MPI_Comm);
(declaim (inline MPI_Send))
(define-alien-routine ("MPI_Send" MPI_Send)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm))

;; int MPI_Recv(void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Status *);
(declaim (inline MPI_Recv))
(define-alien-routine ("MPI_Recv" MPI_Recv)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (source int)
  (tag int)
  (comm MPI_Comm)
  (status MPI_Status :out))

;; int MPI_Get_count(MPI_Status *, MPI_Datatype, int *);
(declaim (inline MPI_Get_count))
(define-alien-routine ("MPI_Get_count" MPI_Get_count)
    int
  (status (* MPI_Status))
  (datatype MPI_Datatype)
  (count int :out))

;; int MPI_Bsend(void*, int, MPI_Datatype, int, int, MPI_Comm);
(declaim (inline MPI_Bsend))
(define-alien-routine ("MPI_Bsend" MPI_Bsend)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm))

;; int MPI_Ssend(void*, int, MPI_Datatype, int, int, MPI_Comm);
(declaim (inline MPI_Ssend))
(define-alien-routine ("MPI_Ssend" MPI_Ssend)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm))

;; int MPI_Rsend(void*, int, MPI_Datatype, int, int, MPI_Comm);
(declaim (inline MPI_Rsend))
(define-alien-routine ("MPI_Rsend" MPI_Rsend)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm))

;; int MPI_Buffer_attach( void* buffer, int);
(declaim (inline MPI_Buffer_attach))
(define-alien-routine ("MPI_Buffer_attach" MPI_Buffer_attach)
    int
  (buffer (* t))
  (size int))

;; int MPI_Buffer_detach( void* buffer, int *);
(declaim (inline MPI_Buffer_detach))
(define-alien-routine ("MPI_Buffer_detach" MPI_Buffer_detach)
    int
  (buffer int :out)
  (size int :out))

;; int MPI_Isend(void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
(declaim (inline MPI_Isend))
(define-alien-routine ("MPI_Isend" MPI_Isend)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int MPI_Ibsend(void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
(declaim (inline MPI_Ibsend))
(define-alien-routine ("MPI_Ibsend" MPI_Ibsend)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int MPI_Issend(void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
(declaim (inline MPI_Issend))
(define-alien-routine ("MPI_Issend" MPI_Issend)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int MPI_Irsend(void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
(declaim (inline MPI_Irsend))
(define-alien-routine ("MPI_Irsend" MPI_Irsend)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int MPI_Irecv(void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
(declaim (inline MPI_Irecv))
(define-alien-routine ("MPI_Irecv" MPI_Irecv)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (source int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int MPI_Wait(MPI_Request *, MPI_Status *);
(declaim (inline MPI_Wait))
(define-alien-routine ("MPI_Wait" MPI_Wait)
    int
  (request (* MPI_Request))
  (status MPI_Status :out))

;; int MPI_Test(MPI_Request *, int *, MPI_Status *);
(declaim (inline MPI_Test))
(define-alien-routine ("MPI_Test" MPI_Test)
    int
  (request (* MPI_Request))
  (flag int :out)
  (status MPI_Status :out))

;; int MPI_Request_free(MPI_Request *);
(declaim (inline MPI_Request_free))
(define-alien-routine ("MPI_Request_free" MPI_Request_free)
    int
  (request (* MPI_Request)))

;; int MPI_Waitany(int, MPI_Request *, int *, MPI_Status *);
(declaim (inline MPI_Waitany))
(define-alien-routine ("MPI_Waitany" MPI_Waitany)
    int
  (count int)
  (array_of_requests (* MPI_Request))
  (index int :out)
  (status MPI_Status :out))

;; int MPI_Testany(int, MPI_Request *, int *, int *, MPI_Status *);
(declaim (inline MPI_Testany))
(define-alien-routine ("MPI_Testany" MPI_Testany)
    int
  (count int)
  (array_of_requests (* MPI_Request))
  (index int :out)
  (flag int :out)
  (status MPI_Status :out))

;; int MPI_Waitall(int, MPI_Request *, MPI_Status *);
(declaim (inline MPI_Waitall))
(define-alien-routine ("MPI_Waitall" MPI_Waitall)
    int
  (count int)
  (array_of_requests (* MPI_Request))
  (array_of_statuses (* MPI_Status))) ; [VNP] :out, but :out arrays ???

;; int MPI_Testall(int, MPI_Request *, int *, MPI_Status *);
(declaim (inline MPI_Testall))
(define-alien-routine ("MPI_Testall" MPI_Testall)
    int
  (count int)
  (array_of_requests (* MPI_Request))
  (flag int :out)
  (array_of_statuses (* MPI_Status))) ; [VNP] :out, but :out arrays ???

;; int MPI_Waitsome(int, MPI_Request *, int *, int *, MPI_Status *);
(declaim (inline MPI_Waitsome))
(define-alien-routine ("MPI_Waitsome" MPI_Waitsome)
    int
  (incount int)
  (array_of_requests (* MPI_Request))
  (outcount int :out)
  (array_of_indices (* int))        ; [VNP] :out, but :out arrays ???
  (array_of_states (* MPI_Status))) ; [VNP] :out, but :out arrays ???

;; int MPI_Testsome(int, MPI_Request *, int *, int *, MPI_Status *);
(declaim (inline MPI_Testsome))
(define-alien-routine ("MPI_Testsome" MPI_Testsome)
    int
  (incount int)
  (array_of_requests (* MPI_Request))
  (outcount int :out)
  (array_of_indices (* int))        ; [VNP] :out, but :out arrays ???
  (array_of_states (* MPI_Status))) ; [VNP] :out, but :out arrays ???

;; int MPI_Iprobe(int, int, MPI_Comm, int *, MPI_Status *);
(declaim (inline MPI_Iprobe))
(define-alien-routine ("MPI_Iprobe" MPI_Iprobe)
    int
  (source int)
  (tag int)
  (comm MPI_Comm)
  (flag int :out)
  (status MPI_Status :out))

;; int MPI_Probe(int, int, MPI_Comm, MPI_Status *);
(declaim (inline MPI_Probe))
(define-alien-routine ("MPI_Probe" MPI_Probe)
    int
  (source int)
  (tag int)
  (comm MPI_Comm)
  (status MPI_Status :out))

;; int MPI_Cancel(MPI_Request *);
(declaim (inline MPI_Cancel))
(define-alien-routine ("MPI_Cancel" MPI_Cancel)
    int
  (request (* MPI_Request)))

;; int MPI_Test_cancelled(MPI_Status *, int *);
(declaim (inline MPI_Test_cancelled))
(define-alien-routine ("MPI_Test_cancelled" MPI_Test_cancelled)
    int
  (status (* MPI_Status))
  (flag int :out))

;; int MPI_Send_init(void*,int,MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
(declaim (inline MPI_Send_init))
(define-alien-routine ("MPI_Send_init" MPI_Send_init)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int MPI_Bsend_init(void*,int,MPI_Datatype, int, int, MPI_Comm,MPI_Request *);
(declaim (inline MPI_Bsend_init))
(define-alien-routine ("MPI_Bsend_init" MPI_Bsend_init)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int MPI_Ssend_init(void*,int,MPI_Datatype,int, int, MPI_Comm, MPI_Request *);
(declaim (inline MPI_Ssend_init))
(define-alien-routine ("MPI_Ssend_init" MPI_Ssend_init)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int MPI_Rsend_init(void*,int,MPI_Datatype,int, int, MPI_Comm, MPI_Request *);
(declaim (inline MPI_Rsend_init))
(define-alien-routine ("MPI_Rsend_init" MPI_Rsend_init)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int MPI_Recv_init(void*,int,MPI_Datatype,int, int, MPI_Comm, MPI_Request *);
(declaim (inline MPI_Recv_init))
(define-alien-routine ("MPI_Recv_init" MPI_Recv_init)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (source int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int MPI_Start(MPI_Request *);
(declaim (inline MPI_Start))
(define-alien-routine ("MPI_Start" MPI_Start)
    int
  (request (* MPI_Request)))

;; int MPI_Startall(int, MPI_Request *);
(declaim (inline MPI_Startall))
(define-alien-routine ("MPI_Startall" MPI_Startall)
    int
  (count int)
  (array_of_requests (* MPI_Request)))

;; int MPI_Sendrecv(void *, int, MPI_Datatype, int, int, void *, int,
;;                  MPI_Datatype, int, int, MPI_Comm, MPI_Status *);
(declaim (inline MPI_Sendrecv))
(define-alien-routine ("MPI_Sendrecv" MPI_Sendrecv)
    int
  (sendbuf (* t))
  (sendcount int)
  (sendtype MPI_Datatype)
  (dest int)
  (sendtag int)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcount int)
  (recvtype MPI_Datatype)
  (source int)
  (recvtag int)
  (comm MPI_Comm)
  (status MPI_Status :out))

;; int MPI_Sendrecv_replace(void*, int, MPI_Datatype, int, int, int, int,
;;                          MPI_Comm, MPI_Status *);
(declaim (inline MPI_Sendrecv_replace))
(define-alien-routine ("MPI_Sendrecv_replace" MPI_Sendrecv_replace)
    int
  (buf (* t)) ; [VNP] doc says :out, but contains initial address
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (sendtag int)
  (source int)
  (recvtag int)
  (comm MPI_Comm)
  (status MPI_Status :out))

;; int MPI_Type_contiguous(int, MPI_Datatype, MPI_Datatype *);
(declaim (inline MPI_Type_contiguous))
(define-alien-routine ("MPI_Type_contiguous" MPI_Type_contiguous)
    int
  (count int)
  (oldtype MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int MPI_Type_vector(int, int, int, MPI_Datatype, MPI_Datatype *);
(declaim (inline MPI_Type_vector))
(define-alien-routine ("MPI_Type_vector" MPI_Type_vector)
    int
  (count int)
  (blocklen int)
  (stride int)
  (old_type MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int MPI_Type_hvector(int, int, MPI_Aint, MPI_Datatype, MPI_Datatype *);
(declaim (inline MPI_Type_hvector))
(define-alien-routine ("MPI_Type_hvector" MPI_Type_hvector)
    int
  (count int)
  (blocklength int)
  (stride MPI_Aint)
  (old_type MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int MPI_Type_indexed(int, int *, int *, MPI_Datatype, MPI_Datatype *);
(declaim (inline MPI_Type_indexed))
(define-alien-routine ("MPI_Type_indexed" MPI_Type_indexed)
    int
  (count int)
  (blocklens (* int))
  (indices (* int))
  (old_type MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int MPI_Type_hindexed(int, int *, MPI_Aint *, MPI_Datatype, MPI_Datatype *);
(declaim (inline MPI_Type_hindexed))
(define-alien-routine ("MPI_Type_hindexed" MPI_Type_hindexed)
    int
  (count int)
  (blocklens (* int))
  (indices (* MPI_Aint))
  (old_type MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int MPI_Type_struct(int, int *, MPI_Aint *, MPI_Datatype *, MPI_Datatype *);
(declaim (inline MPI_Type_struct))
(define-alien-routine ("MPI_Type_struct" MPI_Type_struct)
    int
  (count int)
  (blocklens (* int))
  (indices (* MPI_Aint))
  (old_type MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int MPI_Address(void*, MPI_Aint *);
(declaim (inline MPI_Address))
(define-alien-routine ("MPI_Address" MPI_Address)
    int
  (location (* t))
  (address MPI_Aint :out))

;; /* We could add __attribute__((deprecated)) to routines like
;;    MPI_Type_extent */
;; int MPI_Type_extent(MPI_Datatype, MPI_Aint *);
(declaim (inline MPI_Type_extent))
(define-alien-routine ("MPI_Type_extent" MPI_Type_extent)
    int
  (datatype MPI_Datatype)
  (extent MPI_Aint :out))

;; /* See the 1.1 version of the Standard.  The standard made an 
;;    unfortunate choice here, however, it is the standard.  The size returned 
;;    by MPI_Type_size is specified as an int, not an MPI_Aint */
;; int MPI_Type_size(MPI_Datatype, int *);
(declaim (inline MPI_Type_size))
(define-alien-routine ("MPI_Type_size" MPI_Type_size)
    int
  (datatype MPI_Datatype)
  (size int :out))

;; /* MPI_Type_count was withdrawn in MPI 1.1 */

;; int MPI_Type_lb(MPI_Datatype, MPI_Aint *);
(declaim (inline MPI_Type_lb))
(define-alien-routine ("MPI_Type_lb" MPI_Type_lb)
    int
  (datatype MPI_Datatype)
  (displacement MPI_Aint :out))

;; int MPI_Type_ub(MPI_Datatype, MPI_Aint *);
(declaim (inline MPI_Type_ub))
(define-alien-routine ("MPI_Type_ub" MPI_Type_ub)
    int
  (datatype MPI_Datatype)
  (displacement MPI_Aint :out))

;; int MPI_Type_commit(MPI_Datatype *);
(declaim (inline MPI_Type_commit))
(define-alien-routine ("MPI_Type_commit" MPI_Type_commit)
    int
  (datatype (* MPI_Datatype)))

;; int MPI_Type_free(MPI_Datatype *);
(declaim (inline MPI_Type_free))
(define-alien-routine ("MPI_Type_free" MPI_Type_free)
    int
  (datatype (* MPI_Datatype)))

;; int MPI_Get_elements(MPI_Status *, MPI_Datatype, int *);
(declaim (inline MPI_Get_elements))
(define-alien-routine ("MPI_Get_elements" MPI_Get_elements)
    int
  (status (* MPI_Status))
  (datatype MPI_Datatype)
  (count int :out))

;; int MPI_Pack(void*, int, MPI_Datatype, void *, int, int *,  MPI_Comm);
(declaim (inline MPI_Pack))
(define-alien-routine ("MPI_Pack" MPI_Pack)
    int
  (inbuf (* t))
  (incount int)
  (datatype MPI_Datatype)
  (outbuf (* t))   ; [VNP] doc says :out, but contains initial address
  (outcount int)
  (position (* int))
  (comm MPI_Comm))

;; int MPI_Unpack(void*, int, int *, void *, int, MPI_Datatype, MPI_Comm);
(declaim (inline MPI_Unpack))
(define-alien-routine ("MPI_Unpack" MPI_Unpack)
    int
  (inbuf (* t))
  (insize int)
  (position (* int))
  (outbuf (* t))   ; [VNP] doc says :out, but contains initial address
  (outcount int)
  (datatype MPI_Datatype)
  (comm MPI_Comm))

;; int MPI_Pack_size(int, MPI_Datatype, MPI_Comm, int *);
(declaim (inline MPI_Pack_size))
(define-alien-routine ("MPI_Pack_size" MPI_Pack_size)
    int
  (incount int)
  (datatype MPI_Datatype)
  (comm MPI_Comm)
  (size int :out))

;; int MPI_Barrier(MPI_Comm );
(declaim (inline MPI_Barrier))
(define-alien-routine ("MPI_Barrier" MPI_Barrier)
    int
  (comm MPI_Comm))

;; int MPI_Bcast(void* buffer, int, MPI_Datatype, int, MPI_Comm );
(declaim (inline MPI_Bcast))
(define-alien-routine ("MPI_Bcast" MPI_Bcast)
    int
  (buffer (* t))
  (count int)
  (datatype MPI_Datatype)
  (root int)
  (comm MPI_Comm))

;; int MPI_Gather(void* , int, MPI_Datatype, void*, int,
;;                MPI_Datatype, int, MPI_Comm); 
(declaim (inline MPI_Gather))
(define-alien-routine ("MPI_Gather" MPI_Gather)
    int
  (sendbuf (* t))
  (sendcount int)
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcount int)
  (recvtype MPI_Datatype)
  (root int)
  (comm MPI_Comm))

;; int MPI_Gatherv(void* , int, MPI_Datatype, void*,
;;                 int *, int *, MPI_Datatype, int, MPI_Comm); 
(declaim (inline MPI_Gatherv))
(define-alien-routine ("MPI_Gatherv" MPI_Gatherv)
    int
  (sendbuf (* t))
  (sendcount int)
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcnts (* int))
  (displs (* int))
  (recvtype MPI_Datatype)
  (root int)
  (comm MPI_Comm))

;; int MPI_Scatter(void* , int, MPI_Datatype, void*, int,
;;                 MPI_Datatype, int, MPI_Comm);
(declaim (inline MPI_Scatter))
(define-alien-routine ("MPI_Scatter" MPI_Scatter)
    int
  (sendbuf (* t))
  (sendcount int)
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcount int)
  (recvtype MPI_Datatype)
  (root int)
  (comm MPI_Comm))

;; int MPI_Scatterv(void* , int *, int *displs,
;;                  MPI_Datatype, void*, int, MPI_Datatype, int, MPI_Comm);
(declaim (inline MPI_Scatterv))
(define-alien-routine ("MPI_Scatterv" MPI_Scatterv)
    int
  (sendbuf (* t))
  (sendcnts (* int))
  (displs (* int))
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcnt int)
  (recvtype MPI_Datatype)
  (root int)
  (comm MPI_Comm))

;; int MPI_Allgather(void* , int, MPI_Datatype, void*,
;;                   int, MPI_Datatype, MPI_Comm);
(declaim (inline MPI_Allgather))
(define-alien-routine ("MPI_Allgather" MPI_Allgather)
    int
  (sendbuf (* t))
  (sendcount int)
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcount int)
  (recvtype MPI_Datatype)
  (comm MPI_Comm))

;; int MPI_Allgatherv(void* , int, MPI_Datatype,
;;                    void*, int *, int *, MPI_Datatype, MPI_Comm);
(declaim (inline MPI_Allgatherv))
(define-alien-routine ("MPI_Allgatherv" MPI_Allgatherv)
    int
  (sendbuf (* t))
  (sendcount int)
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcnts (* int))
  (displs (* int))
  (recvtype MPI_Datatype)
  (comm MPI_Comm))

;; int MPI_Alltoall(void* , int, MPI_Datatype, void*,
;;                  int, MPI_Datatype, MPI_Comm);
(declaim (inline MPI_Alltoall))
(define-alien-routine ("MPI_Alltoall" MPI_Alltoall)
    int
  (sendbuf (* t))
  (sendcount int)
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcount int)
  (recvtype MPI_Datatype)
  (comm MPI_Comm))

;; int MPI_Alltoallv(void* , int *, int *, MPI_Datatype,
;;                    void*, int *, int *, MPI_Datatype, MPI_Comm);
(declaim (inline MPI_Alltoallv))
(define-alien-routine ("MPI_Alltoallv" MPI_Alltoallv)
    int
  (sendbuf (* t))
  (sendcounts (* int))
  (sdispls (* int))
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcnts (* int))
  (displs (* int))
  (recvtype MPI_Datatype)
  (comm MPI_Comm))

;; int MPI_Reduce(void* , void*, int, MPI_Datatype, MPI_Op, int, MPI_Comm);
(declaim (inline MPI_Reduce))
(define-alien-routine ("MPI_Reduce" MPI_Reduce)
    int
  (sendbuf (* t))
  (recvbuf (* t))
  (count int)
  (datatype MPI_Datatype)
  (op MPI_Op)
  (root int)
  (comm MPI_Comm))

;; int MPI_Op_create(MPI_User_function *, int, MPI_Op *);
(declaim (inline MPI_Op_create))
(define-alien-routine ("MPI_Op_create" MPI_Op_create)
    int
  (function MPI_User_function)
  (commute int)
  (op MPI_Op :out))

;; int MPI_Op_free( MPI_Op *);
(declaim (inline MPI_Op_free))
(define-alien-routine ("MPI_Op_free" MPI_Op_free)
    int
  (op (* MPI_Op)))

;; int MPI_Allreduce(void* , void*, int, MPI_Datatype, MPI_Op, MPI_Comm);
(declaim (inline MPI_Allreduce))
(define-alien-routine ("MPI_Allreduce" MPI_Allreduce)
    int
  (sendbuf (* t))
  (recvbuf (* t))
  (count int)
  (datatype MPI_Datatype)
  (op MPI_Op)
  (comm MPI_Comm))

;; int MPI_Reduce_scatter(void* , void*, int *, MPI_Datatype, MPI_Op, MPI_Comm);
(declaim (inline MPI_Reduce_scatter))
(define-alien-routine ("MPI_Reduce_scatter" MPI_Reduce_scatter)
    int
  (sendbuf (* t))
  (recvbuf (* t))
  (recvcnts (* int))
  (datatype MPI_Datatype)
  (op MPI_Op)
  (comm MPI_Comm))

;; int MPI_Scan(void* , void*, int, MPI_Datatype, MPI_Op, MPI_Comm );
(declaim (inline MPI_Scan))
(define-alien-routine ("MPI_Scan" MPI_Scan)
    int
  (sendbuf (* t))
  (recvbuf (* t))
  (count int)
  (datatype MPI_Datatype)
  (op MPI_Op)
  (comm MPI_Comm))

;; int MPI_Group_size(MPI_Group, int *);
(declaim (inline MPI_Group_size))
(define-alien-routine ("MPI_Group_size" MPI_Group_size)
    int
  (group MPI_Group)
  (size int :out))

;; int MPI_Group_rank(MPI_Group, int *);
(declaim (inline MPI_Group_rank))
(define-alien-routine ("MPI_Group_rank" MPI_Group_rank)
    int
  (group MPI_Group)
  (rank int :out))

;; int MPI_Group_translate_ranks (MPI_Group, int, int *, MPI_Group, int *);
(declaim (inline MPI_Group_translate_ranks))
(define-alien-routine ("MPI_Group_translate_ranks" MPI_Group_translate_ranks)
    int
  (group1 MPI_Group)
  (n int)
  (ranks1 (* int))
  (group2 MPI_Group)
  (ranks2 (* int)))

;; int MPI_Group_compare(MPI_Group, MPI_Group, int *);
(declaim (inline MPI_Group_compare))
(define-alien-routine ("MPI_Group_compare" MPI_Group_compare)
    int
  (group1 MPI_Group)
  (group2 MPI_Group)
  (result int :out))

;; int MPI_Comm_group(MPI_Comm, MPI_Group *);
(declaim (inline MPI_Comm_group))
(define-alien-routine ("MPI_Comm_group" MPI_Comm_group)
    int
  (comm MPI_Comm)
  (group MPI_Group :out))

;; int MPI_Group_union(MPI_Group, MPI_Group, MPI_Group *);
(declaim (inline MPI_Group_union))
(define-alien-routine ("MPI_Group_union" MPI_Group_union)
    int
  (group1 MPI_Group)
  (group2 MPI_Group)
  (newgroup MPI_Group :out))

;; int MPI_Group_intersection(MPI_Group, MPI_Group, MPI_Group *);
(declaim (inline MPI_Group_intersection))
(define-alien-routine ("MPI_Group_intersection" MPI_Group_intersection)
    int
  (group1 MPI_Group)
  (group2 MPI_Group)
  (newgroup MPI_Group :out))

;; int MPI_Group_difference(MPI_Group, MPI_Group, MPI_Group *);
(declaim (inline MPI_Group_difference))
(define-alien-routine ("MPI_Group_difference" MPI_Group_difference)
    int
  (group1 MPI_Group)
  (group2 MPI_Group)
  (newgroup MPI_Group :out))

;; int MPI_Group_incl(MPI_Group, int, int *, MPI_Group *);
(declaim (inline MPI_Group_incl))
(define-alien-routine ("MPI_Group_incl" MPI_Group_incl)
    int
  (group MPI_Group)
  (n int)
  (ranks (* int))
  (newgroup MPI_Group :out))

;; int MPI_Group_excl(MPI_Group, int, int *, MPI_Group *);
(declaim (inline MPI_Group_excl))
(define-alien-routine ("MPI_Group_excl" MPI_Group_excl)
    int
  (group MPI_Group)
  (n int)
  (ranks (* int))
  (newgroup MPI_Group :out))

;; int MPI_Group_range_incl(MPI_Group, int, int [][3], MPI_Group *);
(declaim (inline MPI_Group_range_incl))
(define-alien-routine ("MPI_Group_range_incl" MPI_Group_range_incl)
    int
  (group MPI_Group)
  (n int)
  (ranges (* int))
  (newgroup MPI_Group :out))

;; int MPI_Group_range_excl(MPI_Group, int, int [][3], MPI_Group *);
(declaim (inline MPI_Group_range_excl))
(define-alien-routine ("MPI_Group_range_excl" MPI_Group_range_excl)
    int
  (group MPI_Group)
  (n int)
  (ranges (* int))
  (newgroup MPI_Group :out))

;; int MPI_Group_free(MPI_Group *);
(declaim (inline MPI_Group_gree))
(define-alien-routine ("MPI_Group_free" MPI_Group_free)
    int
  (group (* MPI_Group)))

;; int MPI_Comm_size(MPI_Comm, int *);
(declaim (inline MPI_Comm_size))
(define-alien-routine ("MPI_Comm_size" MPI_Comm_size)
    int
  (comm MPI_Comm)
  (size int :out))

;; @@@REMOVE: this went to sb-mpich2.asd
;; int MPI_Comm_rank(MPI_Comm, int *);
;; (declaim (inline MPI_Comm_rank))
;; (define-alien-routine ("MPI_Comm_rank" MPI_Comm_rank)
;;     int
;;   (comm MPI_Comm)
;;   (rank int :out))

;; int MPI_Comm_compare(MPI_Comm, MPI_Comm, int *);
(declaim (inline MPI_Comm_compare))
(define-alien-routine ("MPI_Comm_compare" MPI_Comm_compare)
    int
  (comm1 MPI_Comm)
  (comm2 MPI_Comm)
  (result int :out))

;; int MPI_Comm_dup(MPI_Comm, MPI_Comm *);
(declaim (inline MPI_Comm_dup))
(define-alien-routine ("MPI_Comm_dup" MPI_Comm_dup)
    int
  (incomm MPI_Comm)
  (outcomm MPI_Comm :out))

;; int MPI_Comm_create(MPI_Comm, MPI_Group, MPI_Comm *);
(declaim (inline MPI_Comm_create))
(define-alien-routine ("MPI_Comm_create" MPI_Comm_create)
    int
  (incomm MPI_Comm)
  (group MPI_Group)
  (outcomm MPI_Comm :out))

;; int MPI_Comm_split(MPI_Comm, int, int, MPI_Comm *);
(declaim (inline MPI_Comm_split))
(define-alien-routine ("MPI_Comm_split" MPI_Comm_split)
    int
  (comm MPI_Comm)
  (color int)
  (key int)
  (newcomm MPI_Comm :out))

;; int MPI_Comm_free(MPI_Comm *);
(declaim (inline MPI_Comm_free))
(define-alien-routine ("MPI_Comm_free" MPI_Comm_free)
    int
  (comm (* MPI_Comm)))

;; int MPI_Comm_test_inter(MPI_Comm, int *);
(declaim (inline MPI_Comm_test_inter))
(define-alien-routine ("MPI_Comm_test_inter" MPI_Comm_test_inter)
    int
  (comm MPI_Comm)
  (flag int :out))

;; int MPI_Comm_remote_size(MPI_Comm, int *);
(declaim (inline MPI_Comm_remote_size))
(define-alien-routine ("MPI_Comm_remote_size" MPI_Comm_remote_size)
    int
  (comm MPI_Comm)
  (size int :out))

;; int MPI_Comm_remote_group(MPI_Comm, MPI_Group *);
(declaim (inline MPI_Comm_remote_group))
(define-alien-routine ("MPI_Comm_remote_group" MPI_Comm_remote_group)
    int
  (comm MPI_Comm)
  (group MPI_Group :out))

;; int MPI_Intercomm_create(MPI_Comm, int, MPI_Comm, int, int, MPI_Comm *);
(declaim (inline MPI_Intercomm_create))
(define-alien-routine ("MPI_Intercomm_create" MPI_Intercomm_create)
    int
  (local_comm MPI_Comm)
  (local_leader int)
  (peer_comm MPI_Comm)
  (remote_leader int)
  (tag int)
  (comm_out MPI_Comm :out))

;; int MPI_Intercomm_merge(MPI_Comm, int, MPI_Comm *);
(declaim (inline MPI_Intercomm_merge))
(define-alien-routine ("MPI_Intercomm_merge" MPI_Intercomm_merge)
    int
  (comm MPI_Comm)
  (high int)
  (comm_out MPI_Comm :out))

;; int MPI_Keyval_create(MPI_Copy_function *, MPI_Delete_function *,
;;                        int *, void*);
(declaim (inline MPI_Keyval_create))
(define-alien-routine ("MPI_Keyval_create" MPI_Keyval_create)
    int
  (copy_fn MPI_Copy_function)
  (delete_fn MPI_Delete_function)
  (keyval int :out)
  (extra_state (* t)))

;; int MPI_Keyval_free(int *);
(declaim (inline MPI_Keyval_free))
(define-alien-routine ("MPI_Keyval_free" MPI_Keyval_free)
    int
  (keyval (* int)))

;; int MPI_Attr_put(MPI_Comm, int, void*);
(declaim (inline MPI_Attr_put))
(define-alien-routine ("MPI_Attr_put" MPI_Attr_put)
    int
  (comm MPI_Comm)
  (keyval int)
  (attr_val (* t)))

;; int MPI_Attr_get(MPI_Comm, int, void *, int *);
(declaim (inline MPI_Attr_get))
(define-alien-routine ("MPI_Attr_get" MPI_Attr_get)
    int
  (comm MPI_Comm)
  (keyval int)
  (attr_value (* t))
  (flag int :out))

;; int MPI_Attr_delete(MPI_Comm, int);
(declaim (inline MPI_Attr_delete))
(define-alien-routine ("MPI_Attr_delete" MPI_Attr_delete)
    int
  (comm MPI_Comm)
  (keyval int))

;; int MPI_Topo_test(MPI_Comm, int *);
(declaim (inline MPI_Topo_test))
(define-alien-routine ("MPI_Topo_test" MPI_Topo_test)
    int
  (comm MPI_Comm)
  (top_type int :out))

;; int MPI_Cart_create(MPI_Comm, int, int *, int *, int, MPI_Comm *);
(declaim (inline MPI_Cart_create))
(define-alien-routine ("MPI_Cart_create" MPI_Cart_create)
    int
  (comm_old MPI_Comm)
  (ndims int)
  (dims (* int))
  (periods (* int))
  (reorder int)
  (comm_cart MPI_Comm :out))

;; int MPI_Dims_create(int, int, int *);
(declaim (inline MPI_Dims_create))
(define-alien-routine ("MPI_Dims_create" MPI_Dims_create)
    int
  (nnodes int)
  (ndims int)
  (dims (* int)))

;; int MPI_Graph_create(MPI_Comm, int, int *, int *, int, MPI_Comm *);
(declaim (inline MPI_Graph_create))
(define-alien-routine ("MPI_Graph_create" MPI_Graph_create)
    int
  (comm_old MPI_Comm)
  (nnodes int)
  (index (* int))
  (edges (* int))
  (reorder int)
  (comm_graph MPI_Comm :out))

;; int MPI_Graphdims_get(MPI_Comm, int *, int *);
(declaim (inline MPI_Graphdims_get))
(define-alien-routine ("MPI_Graphdims_get" MPI_Graphdims_get)
    int
  (comm MPI_Comm)
  (nnodes int :out)
  (nedges int :out))

;; int MPI_Graph_get(MPI_Comm, int, int, int *, int *);
(declaim (inline MPI_Graph_get))
(define-alien-routine ("MPI_Graph_get" MPI_Graph_get)
    int
  (comm MPI_Comm)
  (maxindex int)
  (maxedges int)
  (index (* int))
  (edges (* int)))

;; int MPI_Cartdim_get(MPI_Comm, int *);
(declaim (inline MPI_Cartdim_get))
(define-alien-routine ("MPI_Cartdim_get" MPI_Cartdim_get)
    int
  (comm MPI_Comm)
  (ndims int :out))

;; int MPI_Cart_get(MPI_Comm, int, int *, int *, int *);
(declaim (inline MPI_Cart_get))
(define-alien-routine ("MPI_Cart_get" MPI_Cart_get)
    int
  (comm MPI_Comm)
  (maxdims int)
  (dims (* int))
  (periods (* int))
  (coords (* int)))

;; int MPI_Cart_rank(MPI_Comm, int *, int *);
(declaim (inline MPI_Cart_rank))
(define-alien-routine ("MPI_Cart_rank" MPI_Cart_rank)
    int
  (comm MPI_Comm)
  (coords (* int))
  (rank int :out))

;; int MPI_Cart_coords(MPI_Comm, int, int, int *);
(declaim (inline MPI_Cart_coords))
(define-alien-routine ("MPI_Cart_coords" MPI_Cart_coords)
    int
  (comm MPI_Comm)
  (rank int)
  (maxdims int)
  (coords (* int)))

;; int MPI_Graph_neighbors_count(MPI_Comm, int, int *);
(declaim (inline MPI_Graph_neighbors_count))
(define-alien-routine ("MPI_Graph_neighbors_count" MPI_Graph_neighbors_count)
    int
  (comm MPI_Comm)
  (rank int)
  (neighbours int :out))

;; int MPI_Graph_neighbors(MPI_Comm, int, int, int *);
(declaim (inline MPI_Graph_neighbors))
(define-alien-routine ("MPI_Graph_neighbors" MPI_Graph_neighbors)
    int
  (comm MPI_Comm)
  (rank int)
  (maxneighbors int)
  (neighbors (* int)))

;; int MPI_Cart_shift(MPI_Comm, int, int, int *, int *);
(declaim (inline MPI_Cart_shift))
(define-alien-routine ("MPI_Cart_shift" MPI_Cart_shift)
    int
  (comm MPI_Comm)
  (direction int)
  (displ int)
  (rank_source int :out)
  (rank_dest int :out))

;; int MPI_Cart_sub(MPI_Comm, int *, MPI_Comm *);
(declaim (inline MPI_Cart_sub))
(define-alien-routine ("MPI_Cart_sub" MPI_Cart_sub)
    int
  (comm MPI_Comm)
  (remain_dims (* int))
  (newcomm MPI_Comm :out))

;; int MPI_Cart_map(MPI_Comm, int, int *, int *, int *);
(declaim (inline MPI_Cart_map))
(define-alien-routine ("MPI_Cart_map" MPI_Cart_map)
    int
  (comm_old MPI_Comm)
  (ndims int)
  (dims (* int))
  (periods (* int))
  (newrank int :out))

;; int MPI_Graph_map(MPI_Comm, int, int *, int *, int *);
(declaim (inline MPI_Graph_map))
(define-alien-routine ("MPI_Graph_map" MPI_Graph_map)
    int
  (comm MPI_Comm)
  (nnodes int)
  (index (* int))
  (edges (* int))
  (newrank int :out))

;; int MPI_Get_processor_name(char *, int *);
(declaim (inline MPI_Get_processor_name))
(define-alien-routine ("MPI_Get_processor_name" MPI_Get_processor_name)
    int
  (name (* char))
  (resultlen int :out))

;; int MPI_Get_version(int *, int *);
(declaim (inline MPI_Get_version))
(define-alien-routine ("MPI_Get_version" MPI_Get_version)
    int
  (version int :out)
  (subversion int :out))

;; int MPI_Errhandler_create(MPI_Handler_function *, MPI_Errhandler *);
(declaim (inline MPI_Errhandler_create))
(define-alien-routine ("MPI_Errhandler_create" MPI_Errhandler_create)
    int
  (function MPI_Handler_function)
  (errhandler MPI_Errhandler :out))

;; int MPI_Errhandler_set(MPI_Comm, MPI_Errhandler);
(declaim (inline MPI_Errhandler_set))
(define-alien-routine ("MPI_Errhandler_set" MPI_Errhandler_set)
    int
  (comm MPI_Comm)
  (errhandler MPI_Errhandler))

;; int MPI_Errhandler_get(MPI_Comm, MPI_Errhandler *);
(declaim (inline MPI_Errhandler_get))
(define-alien-routine ("MPI_Errhandler_get" MPI_Errhandler_get)
    int
  (comm MPI_Comm)
  (errhandler MPI_Errhandler :out))

;; int MPI_Errhandler_free(MPI_Errhandler *);
(declaim (inline MPI_Errhandler_free))
(define-alien-routine ("MPI_Errhandler_free" MPI_Errhandler_free)
    int
  (errhandler (* MPI_Errhandler)))

;; int MPI_Error_string(int, char *, int *);
(declaim (inline MPI_Error_string))
(define-alien-routine ("MPI_Error_string" MPI_Error_string)
    int
  (errcode int)
  (string (* char))
  (resultlen int :out))

;; int MPI_Error_class(int, int *);
(declaim (inline MPI_Error_class))
(define-alien-routine ("MPI_Error_class" MPI_Error_class)
    int
  (errorcode int)
  (errorclass int :out))

;; double MPI_Wtime(void);
(declaim (inline MPI_Wtime))
(define-alien-routine ("MPI_Wtime" MPI_Wtime)
    double)

;; double MPI_Wtick(void);
(declaim (inline MPI_Wtick))
(define-alien-routine ("MPI_Wtick" MPI_Wtick)
    double)

;; @@@REMOVE: this went into sb-mpich2.asd
;; int MPI_Init(int *, char ***);
;; (declaim (inline MPI_Init))
;; (define-alien-routine ("MPI_Init" MPI_Init)
;;     int
;;   (argc (* int))
;;   (argv (* (array c-string))))

;; int MPI_Finalize(void);
(declaim (inline MPI_Finalize))
(define-alien-routine ("MPI_Finalize" MPI_Finalize)
    int)

;; @@@REMOVE: this went into sb-mpich2.asd
;; int MPI_Initialized(int *);
;; (declaim (inline MPI_Initialized))
;; (define-alien-routine ("MPI_Initialized" MPI_Initialized)
;;     int
;;   (flag int :out))

;; int MPI_Abort(MPI_Comm, int);
(declaim (inline MPI_Abort))
(define-alien-routine ("MPI_Abort" MPI_Abort)
    int
  (comm MPI_Comm)
  (errcode int))


;; /* Note that we may need to define a @PCONTROL_LIST@ depending on whether 
;;    stdargs are supported */
;; int MPI_Pcontrol(const int, ...);
(declaim (inline MPI_Pcontrol))
(define-alien-routine ("MPI_Pcontrol" MPI_Pcontrol)
    int
  (level int))

;; [VNP] MPIR_Dup_fn moved up in order to fix forward references

;; /* MPI-2 functions */

;; /* Process Creation and Management */
;; int MPI_Close_port(char *);
(declaim (inline MPI_Close_port))
(define-alien-routine ("MPI_Close_port" MPI_Close_port)
    int
  (port_name c-string))

;; int MPI_Comm_accept(char *, MPI_Info, int, MPI_Comm, MPI_Comm *);
(declaim (inline MPI_Comm_accept))
(define-alien-routine ("MPI_Comm_accept" MPI_Comm_accept)
    int
  (port_name c-string)
  (info MPI_Info)
  (root int)
  (comm MPI_Comm)
  (new_comm MPI_Comm :out))
  
;; int MPI_Comm_connect(char *, MPI_Info, int, MPI_Comm, MPI_Comm *);
(declaim (inline MPI_Comm_connect))
(define-alien-routine ("MPI_Comm_connect" MPI_Comm_connect)
    int
  (port_name c-string)
  (info MPI_Info)
  (root int)
  (comm MPI_Comm)
  (new_comm MPI_Comm :out))

;; int MPI_Comm_disconnect(MPI_Comm *);
(declaim (inline MPI_Comm_disconnect))
(define-alien-routine ("MPI_Comm_disconnect" MPI_Comm_disconnect)
    int
  (comm (* MPI_Comm)))

;; int MPI_Comm_get_parent(MPI_Comm *);
(declaim (inline MPI_Comm_get_parent))
(define-alien-routine ("MPI_Comm_get_parent" MPI_Comm_get_parent)
    int
  (comm MPI_Comm :out))

;; int MPI_Comm_join(int, MPI_Comm *);
(declaim (inline MPI_Comm_join))
(define-alien-routine ("MPI_Comm_join" MPI_Comm_join)
    int
  (fd int)
  (intercomm MPI_Comm :out))

;; int MPI_Comm_spawn(char *, char *[], int, MPI_Info,
;;                    int, MPI_Comm, MPI_Comm *, int []);
(declaim (inline MPI_Comm_spawn))
(define-alien-routine ("MPI_Comm_spawn" MPI_Comm_spawn)
    int
  (command c-string)
  (argv (array c-string))
  (maxprocs int)
  (info MPI_Info)
  (root int)
  (comm MPI_Comm)
  (intercomm MPI_Comm :out)
  (array_of_errcodes (* int)))

;; int MPI_Comm_spawn_multiple(int, char *[], char **[], int [],
;;                             MPI_Info [], int, MPI_Comm, MPI_Comm *, int []);
(declaim (inline MPI_Comm_spawn_multiple))
(define-alien-routine ("MPI_Comm_spawn_multiple" MPI_Comm_spawn_multiple)
    int
  (count int)
  (array_of_commands (array c-string))
  (array_of_argv (* (array c-string)))
  (array_of_maxprocs (* int))
  (array_of_info (* MPI_Info))
  (root int)
  (comm MPI_Comm)
  (intercomm MPI_Comm :out)
  (array_of_errcodes (* int)))

;; int MPI_Lookup_name(char *, MPI_Info, char *);
(declaim (inline MPI_Lookup_name))
(define-alien-routine ("MPI_Lookup_name" MPI_Lookup_name)
    int
  (service_name c-string)
  (info MPI_Info)
  (port_name (* char)))

;; int MPI_Open_port(MPI_Info, char *);
(declaim (inline MPI_Open_port))
(define-alien-routine ("MPI_Open_port" MPI_Open_port)
    int
  (info MPI_Info)
  (port_name (* char)))

;; int MPI_Publish_name(char *, MPI_Info, char *);
(declaim (inline MPI_Publish_name))
(define-alien-routine ("MPI_Publish_name" MPI_Publish_name)
    int
  (service_name c-string)
  (info MPI_Info)
  (port_name (* char)))

;; int MPI_Unpublish_name(char *, MPI_Info, char *);
(declaim (inline MPI_Unpublish_name))
(define-alien-routine ("MPI_Unpublish_name" MPI_Unpublish_name)
    int
  (service_name c-string)
  (info MPI_Info)
  (port_name (* char)))

;; /* One-Sided Communications */
;; int MPI_Accumulate(void *, int, MPI_Datatype, int, MPI_Aint, int, 
;; 		      MPI_Datatype,  MPI_Op, MPI_Win);
(declaim (inline MPI_Accumulate))
(define-alien-routine ("MPI_Accumulate" MPI_Accumulate)
    int
  (origin_addr (* t))
  (origin_count int)
  (origin_datatype MPI_Datatype)
  (target_rank int)
  (target_disp MPI_Aint)
  (target_count int)
  (target_datatype MPI_Datatype)
  (op MPI_Op)
  (win MPI_Win))

;; int MPI_Get(void *, int, MPI_Datatype, int, MPI_Aint, int, MPI_Datatype, 
;; 	       MPI_Win);
(declaim (inline MPI_Get))
(define-alien-routine ("MPI_Get" MPI_Get)
    int
  (origin_addr (* t))
  (origin_count int)
  (origin_datatype MPI_Datatype)
  (target_rank int)
  (target_disp MPI_Aint)
  (target_count int)
  (target_datatype MPI_Datatype)
  (win MPI_Win))
  
;; int MPI_Put(void *, int, MPI_Datatype, int, MPI_Aint, int, MPI_Datatype, 
;; 	       MPI_Win);
(declaim (inline MPI_Put))
(define-alien-routine ("MPI_Put" MPI_Put)
    int
  (origin_addr (* t))
  (origin_count int)
  (origin_datatype MPI_Datatype)
  (target_rank int)
  (target_disp MPI_Aint)
  (target_count int)
  (target_datatype MPI_Datatype)
  (win MPI_Win))

;; int MPI_Win_complete(MPI_Win);
(declaim (inline MPI_Win_complete))
(define-alien-routine ("MPI_Win_complete" MPI_Win_complete)
    int
  (win MPI_Win))

;; int MPI_Win_create(void *, MPI_Aint, int, MPI_Info, MPI_Comm, MPI_Win *);
(declaim (inline MPI_Win_create))
(define-alien-routine ("MPI_Win_create" MPI_Win_create)
    int
  (base (* t))
  (size MPI_Aint)
  (disp_unit int)
  (info MPI_Info)
  (comm MPI_Comm)
  (win MPI_Win :out))

;; int MPI_Win_fence(int, MPI_Win);
(declaim (inline MPI_Win_fence))
(define-alien-routine ("MPI_Win_fence" MPI_Win_fence)
    int
  (_assert int)
  (win MPI_Win))

;; int MPI_Win_free(MPI_Win *);
(declaim (inline MPI_Win_free))
(define-alien-routine ("MPI_Win_free" MPI_Win_free)
    int
  (win (* MPI_Win)))

;; int MPI_Win_get_group(MPI_Win, MPI_Group *);
(declaim (inline MPI_Win_get_group))
(define-alien-routine ("MPI_Win_get_group" MPI_Win_get_group)
    int
  (win MPI_Win)
  (group MPI_Group :out))

;; int MPI_Win_lock(int, int, int, MPI_Win);
(declaim (inline MPI_Win_lock))
(define-alien-routine ("MPI_Win_lock" MPI_Win_lock)
    int
  (lock_type int)
  (rank int)
  (_assert int)
  (win MPI_Win))

;; int MPI_Win_post(MPI_Group, int, MPI_Win);
(declaim (inline MPI_Win_post))
(define-alien-routine ("MPI_Win_post" MPI_Win_post)
    int
  (group MPI_Group)
  (_assert int)
  (win MPI_Win))

;; int MPI_Win_start(MPI_Group, int, MPI_Win);
(declaim (inline MPI_Win_start))
(define-alien-routine ("MPI_Win_start" MPI_Win_start)
    int
  (group MPI_Group)
  (_assert int)
  (win MPI_Win))

;; int MPI_Win_test(MPI_Win, int *);
(declaim (inline MPI_Win_test))
(define-alien-routine ("MPI_Win_test" MPI_Win_test)
    int
  (win MPI_Win)
  (flag int :out))

;; int MPI_Win_unlock(int, MPI_Win);
(declaim (inline MPI_Win_unlock))
(define-alien-routine ("MPI_Win_unlock" MPI_Win_unlock)
    int
  (rank int)
  (win MPI_Win))

;; int MPI_Win_wait(MPI_Win);
(declaim (inline MPI_Win_wait))
(define-alien-routine ("MPI_Win_wait" MPI_Win_wait)
    int
  (win MPI_Win))
 
;; /* Extended Collective Operations */
;; int MPI_Alltoallw(void *, int [], int [], MPI_Datatype [], void *, int [], 
;; 		     int [], MPI_Datatype [], MPI_Comm);
(declaim (inline MPI_Alltoallw))
(define-alien-routine ("MPI_Alltoallw" MPI_Alltoallw)
    int
  (sendbuf (* t))
  (sendcnts (* int))
  (sdispls (* int))
  (sendtypes (* MPI_Datatype))
  (recvbuf (* t))
  (recvcnts (* int))
  (rdispls (* int))
  (recvtypes (* MPI_Datatype))
  (comm MPI_Comm))

;; int MPI_Exscan(void *, void *, int, MPI_Datatype, MPI_Op, MPI_Comm) ;
(declaim (inline MPI_Exscan))
(define-alien-routine ("MPI_Exscan" MPI_Exscan)
    int
  (sendbuf (* t))
  (recvbuf (* t))
  (count int)
  (datatype MPI_Datatype)
  (op MPI_Op)
  (comm MPI_Comm))
 
;; /* External Interfaces */
;; int MPI_Add_error_class(int *);
(declaim (inline MPI_Add_error_class))
(define-alien-routine ("MPI_Add_error_class" MPI_Add_error_class)
    int
  (erroclass int :out))

;; int MPI_Add_error_code(int, int *);
(declaim (inline MPI_Add_error_code))
(define-alien-routine ("MPI_Add_error_code" MPI_Add_error_code)
    int
  (errorclass int)
  (errorcode int :out))

;; int MPI_Add_error_string(int, char *);
(declaim (inline MPI_Add_error_string))
(define-alien-routine ("MPI_Add_error_string" MPI_Add_error_string)
    int
  (errcode int)
  (string c-string))

;; int MPI_Comm_call_errhandler(MPI_Comm, int);
(declaim (inline MPI_Comm_call_errhandler))
(define-alien-routine ("MPI_Comm_call_errhandler" MPI_Comm_call_errhandler)
    int
  (comm MPI_Comm)
  (errcode int))

;; int MPI_Comm_create_keyval(MPI_Comm_copy_attr_function *, 
;;                            MPI_Comm_delete_attr_function *, int *, void *);
(declaim (inline MPI_Comm_create_keyval))
(define-alien-routine ("MPI_Comm_create_keyval" MPI_Comm_create_keyval)
    int
  (comm_copy_attr_fn MPI_Comm_copy_attr_function)
  (comm_delete_attr_fn MPI_Comm_delete_attr_function)
  (comm_keyval int :out)
  (extra_state (* t)))

;; int MPI_Comm_delete_attr(MPI_Comm, int);
(declaim (inline MPI_Comm_delete_attr))
(define-alien-routine ("MPI_Comm_delete_attr" MPI_Comm_delete_attr)
    int
  (comm MPI_Comm)
  (comm_keyval int))

;; int MPI_Comm_free_keyval(int *);
(declaim (inline MPI_Comm_free_keyval))
(define-alien-routine ("MPI_Comm_free_keyval" MPI_Comm_free_keyval)
    int
  (comm_keyval (* int)))

;; int MPI_Comm_get_attr(MPI_Comm, int, void *, int *);
(declaim (inline MPI_Comm_get_attr))
(define-alien-routine ("MPI_Comm_get_attr" MPI_Comm_get_attr)
    int
  (comm MPI_Comm)
  (comm_keyval int)
  (attribute_val (* t))
  (flag int :out))

;; int MPI_Comm_get_name(MPI_Comm, char *, int *);
(declaim (inline MPI_Comm_get_name))
(define-alien-routine ("MPI_Comm_get_name" MPI_Comm_get_name)
    int
  (comm MPI_Comm)
  (comm_name (* char))
  (resultlen int :out))

;; int MPI_Comm_set_attr(MPI_Comm, int, void *);
(declaim (inline MPI_Comm_Set_attr))
(define-alien-routine ("MPI_Comm_Set_attr" MPI_Comm_Set_attr)
    int
  (comm MPI_Comm)
  (comm_keyval int)
  (attribute_val (* t)))

;; int MPI_Comm_set_name(MPI_Comm, char *);
(declaim (inline MPI_Comm_set_name))
(define-alien-routine ("MPI_Comm_set_name" MPI_Comm_set_name)
    int
  (comm MPI_Comm)
  (comm_name c-string))

;; int MPI_File_call_errhandler(MPI_File, int);
(declaim (inline MPI_File_call_errhandler))
(define-alien-routine ("MPI_File_call_errhandler" MPI_File_call_errhandler)
    int
  (fh MPI_File)
  (errcode int))

;; int MPI_Grequest_complete(MPI_Request);
(declaim (inline MPI_Grequest_complete))
(define-alien-routine ("MPI_Grequest_complete" MPI_Grequest_complete)
    int
  (request MPI_Request))

;; int MPI_Grequest_start(MPI_Grequest_query_function *, 
;;                        MPI_Grequest_free_function *, 
;;                        MPI_Grequest_cancel_function *, void *,
;;                        MPI_Request *);
(declaim (inline MPI_Grequest_start))
(define-alien-routine ("MPI_Grequest_start" MPI_Grequest_start)
    int
  (query_fn MPI_Grequest_query_function)
  (free_fn MPI_Grequest_free_function)
  (cancel_fn MPI_Grequest_cancel_function)
  (extra_state (* t))
  (request MPI_Request :out))

;; @@@REMOVE: This went to sb-mpich2.asd
;; int MPI_Init_thread(int *, char ***, int, int *);
;; (declaim (inline MPI_Init_thread))
;; (define-alien-routine ("MPI_Init_thread" MPI_Init_thread)
;;     int
;;   (argc (* int))
;;   (argv (* (array c-string)))
;;   (required int)
;;   (provided int :out))

;; int MPI_Is_thread_main(int *);
(declaim (inline MPI_Is_thread_main))
(define-alien-routine ("MPI_Is_thread_main" MPI_Is_thread_main)
    int
  (flag int :out))

;; int MPI_Query_thread(int *);
(declaim (inline MPI_Query_thread))
(define-alien-routine ("MPI_Query_thread" MPI_Query_thread)
    int
  (provided int :out))

;; int MPI_Status_set_cancelled(MPI_Status *, int);
(declaim (inline MPI_Status_set_cancelled))
(define-alien-routine ("MPI_Status_set_cancelled" MPI_Status_set_cancelled)
    int
  (status (* MPI_Status))
  (flag int))

;; int MPI_Status_set_elements(MPI_Status *, MPI_Datatype, int);
(declaim (inline MPI_Status_set_elements))
(define-alien-routine ("MPI_Status_set_elements" MPI_Status_set_elements)
    int
  (status (* MPI_Status))
  (datatype MPI_Datatype)
  (count int))

;; int MPI_Type_create_keyval(MPI_Type_copy_attr_function *, 
;;                            MPI_Type_delete_attr_function *, int *, void *);
(declaim (inline MPI_Type_create_keyval))
(define-alien-routine ("MPI_Type_create_keyval" MPI_Type_create_keyval)
    int
  (type_copy_attr_fn MPI_Type_copy_attr_function)
  (type_delete_attr_fn MPI_Type_delete_attr_function)
  (type_keyval int :out)
  (extra_state (* t)))

;; int MPI_Type_delete_attr(MPI_Datatype, int);
(declaim (inline MPI_Type_delete_attr))
(define-alien-routine ("MPI_Type_delete_attr" MPI_Type_delete_attr)
    int
  (type MPI_Datatype)
  (type_keyval int))

;; int MPI_Type_dup(MPI_Datatype, MPI_Datatype *);
(declaim (inline MPI_Type_dup))
(define-alien-routine ("MPI_Type_dup" MPI_Type_dup)
    int
  (datatype MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int MPI_Type_free_keyval(int *);
(declaim (inline MPI_Type_free_keyval))
(define-alien-routine ("MPI_Type_free_keyval" MPI_Type_free_keyval)
    int
  (type_keyval (* int)))

;; int MPI_Type_get_attr(MPI_Datatype, int, void *, int *);
(declaim (inline MPI_Type_get_attr))
(define-alien-routine ("MPI_Type_get_attr" MPI_Type_get_attr)
    int
  (type MPI_Datatype)
  (type_keyval int)
  (attribute_val (* t))
  (flag int :out))

;; int MPI_Type_get_contents(MPI_Datatype, int, int, int, int [], MPI_Aint [], 
;;                           MPI_Datatype []);
(declaim (inline MPI_Type_get_contents))
(define-alien-routine ("MPI_Type_get_contents" MPI_Type_get_contents)
    int
  (datatype MPI_Datatype)
  (max_integers int)
  (max_addresses int)
  (max_datatypes int)
  (array_of_integers (* int))
  (array_of_addresses (* MPI_Aint))
  (array_of_datatypes (* MPI_Datatype)))

;; int MPI_Type_get_envelope(MPI_Datatype, int *, int *, int *, int *);
(declaim (inline MPI_Type_get_envelope))
(define-alien-routine ("MPI_Type_get_envelope" MPI_Type_get_envelope)
    int
  (datatype MPI_Datatype)
  (num_integers int :out)
  (num_addresses int :out)
  (num_datatypes int :out)
  (combiner int :out))

;; int MPI_Type_get_name(MPI_Datatype, char *, int *);
(declaim (inline MPI_Type_get_name))
(define-alien-routine ("MPI_Type_get_name" MPI_Type_get_name)
    int
  (datatype MPI_Datatype)
  (type_name (* char))
  (resultlen int :out))

;; int MPI_Type_set_attr(MPI_Datatype, int, void *);
(declaim (inline MPI_Type_set_attr))
(define-alien-routine ("MPI_Type_set_attr" MPI_Type_set_attr)
    int
  (type MPI_Datatype)
  (type_keyval int)
  (attribute_val (* t)))

;; int MPI_Type_set_name(MPI_Datatype, char *);
(declaim (inline MPI_Type_set_name))
(define-alien-routine ("MPI_Type_set_name" MPI_Type_set_name)
    int
  (type MPI_Datatype)
  (type_name c-string))

;; int MPI_Type_match_size( int, int, MPI_Datatype *);
(declaim (inline MPI_Type_match_size))
(define-alien-routine ("MPI_Type_match_size" MPI_Type_match_size)
    int
  (typeclass int)
  (size int)
  (datatype MPI_Datatype :out))

;; int MPI_Win_call_errhandler(MPI_Win, int);
(declaim (inline MPI_Win_call_errhandler))
(define-alien-routine ("MPI_Win_call_errhandler" MPI_Win_call_errhandler)
    int
  (win MPI_Win)
  (errorcode int))

;; int MPI_Win_create_keyval(MPI_Win_copy_attr_function *, 
;;                          MPI_Win_delete_attr_function *, int *, void *);
(declaim (inline MPI_Win_create_keyval))
(define-alien-routine ("MPI_Win_create_keyval" MPI_Win_create_keyval)
    int
  (win_copy_attr_fn MPI_Win_copy_attr_function)
  (win_delete_attr_fn MPI_Win_delete_attr_function)
  (win_keyval int :out)
  (extra_state (* t)))

;; int MPI_Win_delete_attr(MPI_Win, int);
(declaim (inline MPI_Win_delete_attr))
(define-alien-routine ("MPI_Win_delete_attr" MPI_Win_delete_attr)
    int
  (win MPI_Win)
  (win_keyval int))

;; int MPI_Win_free_keyval(int *);
(declaim (inline MPI_Win_free_keyval))
(define-alien-routine ("MPI_Win_free_keyval" MPI_Win_free_keyval)
    int
  (win_keyval (* int)))

;; int MPI_Win_get_attr(MPI_Win, int, void *, int *);
(declaim (inline MPI_Win_get_attr))
(define-alien-routine ("MPI_Win_get_attr" MPI_Win_get_attr)
    int
  (win MPI_Win)
  (win_keyval int)
  (attribute_val (* t))
  (flag int :out))

;; int MPI_Win_get_name(MPI_Win, char *, int *);
(declaim (inline MPI_Win_get_name))
(define-alien-routine ("MPI_Win_get_name" MPI_Win_get_name)
    int
  (win MPI_Win)
  (win_name (* char))
  (resultlen int :out))

;; int MPI_Win_set_attr(MPI_Win, int, void *);
(declaim (inline MPI_Win_set_attr))
(define-alien-routine ("MPI_Win_set_attr" MPI_Win_set_attr)
    int
  (win MPI_Win)
  (win_keyval int)
  (attribute_val (* t)))

;; int MPI_Win_set_name(MPI_Win, char *);
(declaim (inline MPI_Win_set_name))
(define-alien-routine ("MPI_Win_set_name" MPI_Win_set_name)
    int
  (win MPI_Win)
  (win_name c-string))

;; /* Miscellany */
;; #ifdef FOO
;; MPI_Comm MPI_Comm_f2c(MPI_Fint);
;; MPI_Datatype MPI_Type_f2c(MPI_Fint);
;; MPI_File MPI_File_f2c(MPI_Fint);
;; MPI_Fint MPI_Comm_c2f(MPI_Comm);
;; MPI_Fint MPI_File_c2f(MPI_File);
;; MPI_Fint MPI_Group_c2f(MPI_Group);
;; MPI_Fint MPI_Info_c2f(MPI_Info);
;; MPI_Fint MPI_Op_c2f(MPI_Op);
;; MPI_Fint MPI_Request_c2f(MPI_Request);
;; MPI_Fint MPI_Type_c2f(MPI_Datatype);
;; MPI_Fint MPI_Win_c2f(MPI_Win);
;; MPI_Group MPI_Group_f2c(MPI_Fint);
;; MPI_Info MPI_Info_f2c(MPI_Fint);
;; MPI_Op MPI_Op_f2c(MPI_Fint);
;; MPI_Request MPI_Request_f2c(MPI_Fint);
;; MPI_Win MPI_Win_f2c(MPI_Fint);
;; #endif

;; int MPI_Alloc_mem(MPI_Aint, MPI_Info info, void *baseptr);
(declaim (inline MPI_Alloc_mem))
(define-alien-routine ("MPI_Alloc_mem" MPI_Alloc_mem)
    int
  (size MPI_Aint)
  (info MPI_Info)
  (baseptr int :out))  ;; [VNP] actually void *, but int :out is easier to do

;; int MPI_Comm_create_errhandler(MPI_Comm_errhandler_fn *, MPI_Errhandler *);
(declaim (inline MPI_Comm_create_errhandler))
(define-alien-routine ("MPI_Comm_create_errhandler" MPI_Comm_create_errhandler)
    int
  (function MPI_Comm_errhandler_fn)
  (errhandler MPI_Errhandler :out))

;; int MPI_Comm_get_errhandler(MPI_Comm, MPI_Errhandler *);
(declaim (inline MPI_Comm_get_errhandler))
(define-alien-routine ("MPI_Comm_get_errhandler" MPI_Comm_get_errhandler)
    int
  (comm MPI_Comm)
  (errhandler MPI_Errhandler :out))

;; int MPI_Comm_set_errhandler(MPI_Comm, MPI_Errhandler);
(declaim (inline MPI_Comm_set_errhandler))
(define-alien-routine ("MPI_Comm_set_errhandler" MPI_Comm_set_errhandler)
    int
  (comm MPI_Comm)
  (errhandler MPI_Errhandler))

;; int MPI_File_create_errhandler(MPI_File_errhandler_fn *, MPI_Errhandler *);
(declaim (inline MPI_File_create_errhandler))
(define-alien-routine ("MPI_File_create_errhandler" MPI_File_create_errhandler)
    int
  (function MPI_File_errhandler_fn)
  (errhandler MPI_Errhandler :out))

;; int MPI_File_get_errhandler(MPI_File, MPI_Errhandler *);
(declaim (inline MPI_File_get_errhandler))
(define-alien-routine ("MPI_File_get_errhandler" MPI_File_get_errhandler)
    int
  (file MPI_File)
  (errhandler MPI_Errhandler :out))

;; int MPI_File_set_errhandler(MPI_File, MPI_Errhandler);
(declaim (inline MPI_File_set_errhandler))
(define-alien-routine ("MPI_File_set_errhandler" MPI_File_set_errhandler)
    int
  (file MPI_File)
  (errhandler MPI_Errhandler))

;; int MPI_Finalized(int *);
(declaim (inline MPI_Finalized))
(define-alien-routine ("MPI_Finalized" MPI_Finalized)
    int
  (flag int :out))

;; int MPI_Free_mem(void *);
(declaim (inline MPI_Free_mem))
(define-alien-routine ("MPI_Free_mem" MPI_Free_mem)
    int
  (base (* t)))

;; int MPI_Get_address(void *, MPI_Aint *);
(declaim (inline MPI_Get_address))
(define-alien-routine ("MPI_Get_address" MPI_Get_address)
    int
  (location (* t))
  (address MPI_Aint :out))

;; int MPI_Info_create(MPI_Info *);
(declaim (inline MPI_Info_create))
(define-alien-routine ("MPI_Info_create" MPI_Info_create)
    int
  (info MPI_Info :out))

;; int MPI_Info_delete(MPI_Info, char *);
(declaim (inline MPI_Info_delete))
(define-alien-routine ("MPI_Info_delete" MPI_Info_delete)
    int
  (info MPI_Info)
  (key c-string))

;; int MPI_Info_dup(MPI_Info, MPI_Info *);
(declaim (inline MPI_Info_dup))
(define-alien-routine ("MPI_Info_dup" MPI_Info_dup)
    int
  (info MPI_Info)
  (newinfo MPI_Info :out))

;; int MPI_Info_free(MPI_Info *info);
(declaim (inline MPI_Info_free))
(define-alien-routine ("MPI_Info_free" MPI_Info_free)
    int
  (info (* MPI_Info)))

;; int MPI_Info_get(MPI_Info, char *, int, char *, int *);
(declaim (inline MPI_Info_get))
(define-alien-routine ("MPI_Info_get" MPI_Info_get)
    int
  (info MPI_Info)
  (key c-string)
  (valuelen int)
  (value (* char))
  (flag int :out))

;; int MPI_Info_get_nkeys(MPI_Info, int *);
(declaim (inline MPI_Info_get_nkeys))
(define-alien-routine ("MPI_Info_get_nkeys" MPI_Info_get_nkeys)
    int
  (info MPI_Info)
  (nkeys int :out))

;; int MPI_Info_get_nthkey(MPI_Info, int, char *);
(declaim (inline MPI_Info_get_nthkey))
(define-alien-routine ("MPI_Info_get_nthkey" MPI_Info_get_nthkey)
    int
  (info MPI_Info)
  (n int)
  (key (* char)))

;; int MPI_Info_get_valuelen(MPI_Info, char *, int *, int *);
(declaim (inline MPI_Info_get_valuelen))
(define-alien-routine ("MPI_Info_get_valuelen" MPI_Info_get_valuelen)
    int
  (info MPI_Info)
  (key c-string)
  (valuelen int :out)
  (flag int :out))

;; int MPI_Info_set(MPI_Info, char *, char *);
(declaim (inline MPI_Info_set))
(define-alien-routine ("MPI_Info_set" MPI_Info_set)
    int
  (info MPI_Info)
  (key c-string)
  (value c-string))

;; int MPI_Pack_external(char *, void *, int, MPI_Datatype, void *, MPI_Aint, 
;;                       MPI_Aint *); 
(declaim (inline MPI_Pack_external))
(define-alien-routine ("MPI_Pack_external" MPI_Pack_external)
    int
  (datarep c-string)
  (inbuf (* t))
  (incount int)
  (datatype MPI_Datatype)
  (outbuf (* t))
  (outcount MPI_Aint)
  (position MPI_Aint :in-out))

;; int MPI_Pack_external_size(char *, int, MPI_Datatype, MPI_Aint *); 
(declaim (inline MPI_Pack_external_size))
(define-alien-routine ("MPI_Pack_external_size" MPI_Pack_external_size)
    int
  (datarep c-string)
  (incount int)
  (datatype MPI_Datatype)
  (size MPI_Aint :out))

;; int MPI_Request_get_status(MPI_Request, int *, MPI_Status *);
(declaim (inline MPI_Request_get_status))
(define-alien-routine ("MPI_Request_get_status" MPI_Request_get_status)
    int
  (request MPI_Request)
  (flag int :out)
  (status MPI_Status :out))

;; int MPI_Status_c2f(MPI_Status *, MPI_Fint *);
(declaim (inline MPI_Status_c2f))
(define-alien-routine ("MPI_Status_c2f" MPI_Status_c2f)
    int
  (c_status (* MPI_Status))
  (f_status MPI_Fint :out))

;; int MPI_Status_f2c(MPI_Fint *, MPI_Status *);
(declaim (inline MPI_Status_f2c))
(define-alien-routine ("MPI_Status_f2c" MPI_Status_f2c)
    int
  (f_status (* MPI_Fint))
  (c_status MPI_Status :out))

;; int MPI_Type_create_darray(int, int, int, int [], int [], int [], int [],
;;                            int, MPI_Datatype, MPI_Datatype *);
(declaim (inline MPI_Type_create_darray))
(define-alien-routine ("MPI_Type_create_darray" MPI_Type_create_darray)
    int
  (size int)
  (rank int)
  (ndims int)
  (array_of_gsizes (* int))
  (array_of_distribs (* int))
  (array_of_dargs (* int))
  (array_of_psizes (* int))
  (order int)
  (oldtype MPI_Datatype)
  (newtype MPI_Datatype :out))
		   
;; int MPI_Type_create_hindexed(int, int [], MPI_Aint [], MPI_Datatype, 
;;                              MPI_Datatype *);
(declaim (inline MPI_Type_create_hindexed))
(define-alien-routine ("MPI_Type_create_hindexed" MPI_Type_create_hindexed)
    int
  (count int)
  (blocklengths (* int))
  (displacements (* int))
  (oldtype MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int MPI_Type_create_hvector(int, int, MPI_Aint, MPI_Datatype,
;;                             MPI_Datatype *);
(declaim (inline MPI_Type_create_hvector))
(define-alien-routine ("MPI_Type_create_hvector" MPI_Type_create_hvector)
    int
  (count int)
  (blocklength int)
  (stride MPI_Aint)
  (oldtype MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int MPI_Type_create_indexed_block(int, int, int [], MPI_Datatype, 
;;                                   MPI_Datatype *);
(declaim (inline MPI_Type_create_indexed_block))
(define-alien-routine ("MPI_Type_create_indexed_block"
		       MPI_Type_create_indexed_block)
    int
  (count int)
  (blocklength int)
  (array_of_displacements (* int))
  (oldtype MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int MPI_Type_create_resized(MPI_Datatype, MPI_Aint,
;;                             MPI_Aint, MPI_Datatype *);
(declaim (inline MPI_Type_create_resized))
(define-alien-routine ("MPI_Type_create_resized" MPI_Type_create_resized)
    int
  (oldtype MPI_Datatype)
  (lb MPI_Aint)
  (extent MPI_Aint)
  (newtype MPI_Datatype :out))

;; int MPI_Type_create_struct(int, int [], MPI_Aint [], MPI_Datatype [], 
;;                            MPI_Datatype *);
(declaim (inline MPI_Type_create_struct))
(define-alien-routine ("MPI_Type_create_struct" MPI_Type_create_struct)
    int
  (count int)
  (array_of_blocklengths (* int))
  (array_of_displacements (* MPI_Aint))
  (array_of_types (* MPI_Datatype))
  (newtype MPI_Datatype :out))

;; int MPI_Type_create_subarray(int, int [], int [], int [], int, MPI_Datatype, 
;;                              MPI_Datatype *);
(declaim (inline MPI_Type_create_subarray))
(define-alien-routine ("MPI_Type_create_subarray" MPI_Type_create_subarray)
    int
  (ndims int)
  (array_of_sizes (* int))
  (array_of_subsizes (* int))
  (array_of_starts (* int))
  (order int)
  (oldtype MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int MPI_Type_get_extent(MPI_Datatype, MPI_Aint *, MPI_Aint *);
(declaim (inline MPI_Type_get_extent))
(define-alien-routine ("MPI_Type_get_extent" MPI_Type_get_extent)
    int
  (datatype MPI_Datatype)
  (lb MPI_Aint :out)
  (extent MPI_Aint :out))

;; int MPI_Type_get_true_extent(MPI_Datatype, MPI_Aint *, MPI_Aint *);
(declaim (inline MPI_Type_get_true_extent))
(define-alien-routine ("MPI_Type_get_true_extent" MPI_Type_get_true_extent)
    int
  (datatype MPI_Datatype)
  (true_lb MPI_Aint :out)
  (true_extent MPI_Aint :out))

;; int MPI_Unpack_external(char *, void *, MPI_Aint, MPI_Aint *, void *, int, 
;;                         MPI_Datatype); 
(declaim (inline MPI_Unpack_external))
(define-alien-routine ("MPI_Unpack_external" MPI_Unpack_external)
    int
  (datarep c-string)
  (inbuf (* t))
  (insize MPI_Aint)
  (position MPI_Aint :in-out)
  (outbuf (* t))
  (outcount int)
  (datatype MPI_Datatype))

;; int MPI_Win_create_errhandler(MPI_Win_errhandler_fn *, MPI_Errhandler *);
(declaim (inline MPI_Win_create_errhandler))
(define-alien-routine ("MPI_Win_create_errhandler" MPI_Win_create_errhandler)
    int
  (function MPI_Win_errhandler_fn)
  (errhandler MPI_Errhandler :out))

;; int MPI_Win_get_errhandler(MPI_Win, MPI_Errhandler *);
(declaim (inline MPI_Win_get_errhandler))
(define-alien-routine ("MPI_Win_get_errhandler" MPI_Win_get_errhandler)
    int
  (win MPI_Win)
  (errhandler MPI_Errhandler :out))

;; int MPI_Win_set_errhandler(MPI_Win, MPI_Errhandler);
(declaim (inline MPI_Win_set_errhandler))
(define-alien-routine ("MPI_Win_set_errhandler" MPI_Win_set_errhandler)
    int
  (win MPI_Win)
  (errhandler MPI_Errhandler))

;; /* Fortran 90-related functions.  These routines are available only if
;;    Fortran 90 support is enabled 
;; */
;; int MPI_Type_create_f90_integer( int, MPI_Datatype * );
(declaim (inline MPI_Type_create_f90_integer))
(define-alien-routine ("MPI_Type_create_f90_integer"
		       MPI_Type_create_f90_integer)
    int
  (r int)
  (newtype MPI_Datatype :out))

;; int MPI_Type_create_f90_real( int, int, MPI_Datatype * );
(declaim (inline MPI_Type_create_f90_real))
(define-alien-routine ("MPI_Type_create_f90_real" MPI_Type_create_f90_real)
    int
  (r int)
  (p int)
  (newtype MPI_Datatype :out))

;; int MPI_Type_create_f90_complex( int, int, MPI_Datatype * );
(declaim (inline MPI_Type_create_f90_complex))
(define-alien-routine ("MPI_Type_create_f90_complex" MPI_Type_create_f90_complex)
    int
  (r int)
  (p int)
  (newtype MPI_Datatype :out))

;; /* End Prototypes */
;; #endif /* MPICH_SUPPRESS_PROTOTYPES */



;; /* Here are the bindings of the profiling routines */
;; #if !defined(MPI_BUILD_PROFILING)

;; int PMPI_Send(void*, int, MPI_Datatype, int, int, MPI_Comm);
(declaim (inline PMPI_Send))
(define-alien-routine ("PMPI_Send" PMPI_Send)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm))

;; int PMPI_Recv(void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Status *);
(declaim (inline PMPI_Recv))
(define-alien-routine ("PMPI_Recv" PMPI_Recv)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (source int)
  (tag int)
  (comm MPI_Comm)
  (status MPI_Status :out))

;; int PMPI_Get_count(MPI_Status *, MPI_Datatype, int *);
(declaim (inline PMPI_Get_count))
(define-alien-routine ("PMPI_Get_count" PMPI_Get_count)
    int
  (status (* MPI_Status))
  (datatype MPI_Datatype)
  (count int :out))

;; int PMPI_Bsend(void*, int, MPI_Datatype, int, int, MPI_Comm);
(declaim (inline PMPI_Bsend))
(define-alien-routine ("PMPI_Bsend" PMPI_Bsend)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm))

;; int PMPI_Ssend(void*, int, MPI_Datatype, int, int, MPI_Comm);
(declaim (inline PMPI_Ssend))
(define-alien-routine ("PMPI_Ssend" PMPI_Ssend)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm))

;; int PMPI_Rsend(void*, int, MPI_Datatype, int, int, MPI_Comm);
(declaim (inline PMPI_Rsend))
(define-alien-routine ("PMPI_Rsend" PMPI_Rsend)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm))

;; int PMPI_Buffer_attach( void* buffer, int);
(declaim (inline PMPI_Buffer_attach))
(define-alien-routine ("PMPI_Buffer_attach" PMPI_Buffer_attach)
    int
  (buffer (* t))
  (size int))

;; int PMPI_Buffer_detach( void* buffer, int *);
(declaim (inline PMPI_Buffer_detach))
(define-alien-routine ("PMPI_Buffer_detach" PMPI_Buffer_detach)
    int
  (buffer int :out)
  (size int :out))

;; int PMPI_Isend(void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
(declaim (inline PMPI_Isend))
(define-alien-routine ("PMPI_Isend" PMPI_Isend)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int PMPI_Ibsend(void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
(declaim (inline PMPI_Ibsend))
(define-alien-routine ("PMPI_Ibsend" PMPI_Ibsend)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int PMPI_Issend(void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
(declaim (inline PMPI_Issend))
(define-alien-routine ("PMPI_Issend" PMPI_Issend)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int PMPI_Irsend(void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
(declaim (inline PMPI_Irsend))
(define-alien-routine ("PMPI_Irsend" PMPI_Irsend)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int PMPI_Irecv(void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
(declaim (inline PMPI_Irecv))
(define-alien-routine ("PMPI_Irecv" PMPI_Irecv)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (source int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int PMPI_Wait(MPI_Request *, MPI_Status *);
(declaim (inline PMPI_Wait))
(define-alien-routine ("PMPI_Wait" PMPI_Wait)
    int
  (request (* MPI_Request))
  (status MPI_Status :out))

;; int PMPI_Test(MPI_Request *, int *, MPI_Status *);
(declaim (inline PMPI_Test))
(define-alien-routine ("PMPI_Test" PMPI_Test)
    int
  (request (* MPI_Request))
  (flag int :out)
  (status MPI_Status :out))

;; int PMPI_Request_free(MPI_Request *);
(declaim (inline PMPI_Request_free))
(define-alien-routine ("PMPI_Request_free" PMPI_Request_free)
    int
  (request (* MPI_Request)))

;; int PMPI_Waitany(int, MPI_Request *, int *, MPI_Status *);
(declaim (inline PMPI_Waitany))
(define-alien-routine ("PMPI_Waitany" PMPI_Waitany)
    int
  (count int)
  (array_of_requests (* MPI_Request))
  (index int :out)
  (status MPI_Status :out))

;; int PMPI_Testany(int, MPI_Request *, int *, int *, MPI_Status *);
(declaim (inline PMPI_Testany))
(define-alien-routine ("PMPI_Testany" PMPI_Testany)
    int
  (count int)
  (array_of_requests (* MPI_Request))
  (index int :out)
  (flag int :out)
  (status MPI_Status :out))

;; int PMPI_Waitall(int, MPI_Request *, MPI_Status *);
(declaim (inline PMPI_Waitall))
(define-alien-routine ("PMPI_Waitall" PMPI_Waitall)
    int
  (count int)
  (array_of_requests (* MPI_Request))
  (array_of_statuses (* MPI_Status))) ; [VNP] :out, but :out arrays ???

;; int PMPI_Testall(int, MPI_Request *, int *, MPI_Status *);
(declaim (inline PMPI_Testall))
(define-alien-routine ("PMPI_Testall" PMPI_Testall)
    int
  (count int)
  (array_of_requests (* MPI_Request))
  (flag int :out)
  (array_of_statuses (* MPI_Status))) ; [VNP] :out, but :out arrays ???

;; int PMPI_Waitsome(int, MPI_Request *, int *, int *, MPI_Status *);
(declaim (inline PMPI_Waitsome))
(define-alien-routine ("PMPI_Waitsome" PMPI_Waitsome)
    int
  (incount int)
  (array_of_requests (* MPI_Request))
  (outcount int :out)
  (array_of_indices (* int))        ; [VNP] :out, but :out arrays ???
  (array_of_states (* MPI_Status))) ; [VNP] :out, but :out arrays ???

;; int PMPI_Testsome(int, MPI_Request *, int *, int *, MPI_Status *);
(declaim (inline PMPI_Testsome))
(define-alien-routine ("PMPI_Testsome" PMPI_Testsome)
    int
  (incount int)
  (array_of_requests (* MPI_Request))
  (outcount int :out)
  (array_of_indices (* int))        ; [VNP] :out, but :out arrays ???
  (array_of_states (* MPI_Status))) ; [VNP] :out, but :out arrays ???

;; int PMPI_Iprobe(int, int, MPI_Comm, int *, MPI_Status *);
(declaim (inline PMPI_Iprobe))
(define-alien-routine ("PMPI_Iprobe" PMPI_Iprobe)
    int
  (source int)
  (tag int)
  (comm MPI_Comm)
  (flag int :out)
  (status MPI_Status :out))

;; int PMPI_Probe(int, int, MPI_Comm, MPI_Status *);
(declaim (inline PMPI_Probe))
(define-alien-routine ("PMPI_Probe" PMPI_Probe)
    int
  (source int)
  (tag int)
  (comm MPI_Comm)
  (status MPI_Status :out))

;; int PMPI_Cancel(MPI_Request *);
(declaim (inline PMPI_Cancel))
(define-alien-routine ("PMPI_Cancel" PMPI_Cancel)
    int
  (request (* MPI_Request)))

;; int PMPI_Test_cancelled(MPI_Status *, int *);
(declaim (inline PMPI_Test_cancelled))
(define-alien-routine ("PMPI_Test_cancelled" PMPI_Test_cancelled)
    int
  (status (* MPI_Status))
  (flag int :out))

;; int PMPI_Send_init(void*,int,MPI_Datatype, int, int, MPI_Comm, MPI_Request *);
(declaim (inline PMPI_Send_init))
(define-alien-routine ("PMPI_Send_init" PMPI_Send_init)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int PMPI_Bsend_init(void*,int,MPI_Datatype, int, int, MPI_Comm,MPI_Request *);
(declaim (inline PMPI_Bsend_init))
(define-alien-routine ("PMPI_Bsend_init" PMPI_Bsend_init)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int PMPI_Ssend_init(void*,int,MPI_Datatype,int, int, MPI_Comm, MPI_Request *);
(declaim (inline PMPI_Ssend_init))
(define-alien-routine ("PMPI_Ssend_init" PMPI_Ssend_init)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int PMPI_Rsend_init(void*,int,MPI_Datatype,int, int, MPI_Comm, MPI_Request *);
(declaim (inline PMPI_Rsend_init))
(define-alien-routine ("PMPI_Rsend_init" PMPI_Rsend_init)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int PPMPI_Recv_init(void*,int,MPI_Datatype,int, int, MPI_Comm, MPI_Request *);
(declaim (inline PMPI_Recv_init))
(define-alien-routine ("PMPI_Recv_init" PMPI_Recv_init)
    int
  (buf (* t))
  (count int)
  (datatype MPI_Datatype)
  (source int)
  (tag int)
  (comm MPI_Comm)
  (request MPI_Request :out))

;; int PMPI_Start(MPI_Request *);
(declaim (inline PMPI_Start))
(define-alien-routine ("PMPI_Start" PMPI_Start)
    int
  (request (* MPI_Request)))

;; int PMPI_Startall(int, MPI_Request *);
(declaim (inline PMPI_Startall))
(define-alien-routine ("PMPI_Startall" PMPI_Startall)
    int
  (count int)
  (array_of_requests (* MPI_Request)))

;; int PMPI_Sendrecv(void *, int, MPI_Datatype, int, int, void *, int,
;;                  MPI_Datatype, int, int, MPI_Comm, MPI_Status *);
(declaim (inline PMPI_Sendrecv))
(define-alien-routine ("PMPI_Sendrecv" PMPI_Sendrecv)
    int
  (sendbuf (* t))
  (sendcount int)
  (sendtype MPI_Datatype)
  (dest int)
  (sendtag int)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcount int)
  (recvtype MPI_Datatype)
  (source int)
  (recvtag int)
  (comm MPI_Comm)
  (status MPI_Status :out))

;; int PMPI_Sendrecv_replace(void*, int, MPI_Datatype, int, int, int, int,
;;                          MPI_Comm, MPI_Status *);
(declaim (inline PMPI_Sendrecv_replace))
(define-alien-routine ("PMPI_Sendrecv_replace" PMPI_Sendrecv_replace)
    int
  (buf (* t)) ; [VNP] doc says :out, but contains initial address
  (count int)
  (datatype MPI_Datatype)
  (dest int)
  (sendtag int)
  (source int)
  (recvtag int)
  (comm MPI_Comm)
  (status MPI_Status :out))

;; int PMPI_Type_contiguous(int, MPI_Datatype, MPI_Datatype *);
(declaim (inline PMPI_Type_contiguous))
(define-alien-routine ("PMPI_Type_contiguous" PMPI_Type_contiguous)
    int
  (count int)
  (oldtype MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int PMPI_Type_vector(int, int, int, MPI_Datatype, MPI_Datatype *);
(declaim (inline PMPI_Type_vector))
(define-alien-routine ("PMPI_Type_vector" PMPI_Type_vector)
    int
  (count int)
  (blocklen int)
  (stride int)
  (old_type MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int PMPI_Type_hvector(int, int, MPI_Aint, MPI_Datatype, MPI_Datatype *);
(declaim (inline PMPI_Type_hvector))
(define-alien-routine ("PMPI_Type_hvector" PMPI_Type_hvector)
    int
  (count int)
  (blocklength int)
  (stride MPI_Aint)
  (old_type MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int PMPI_Type_indexed(int, int *, int *, MPI_Datatype, MPI_Datatype *);
(declaim (inline PMPI_Type_indexed))
(define-alien-routine ("PMPI_Type_indexed" PMPI_Type_indexed)
    int
  (count int)
  (blocklens (* int))
  (indices (* int))
  (old_type MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int PMPI_Type_hindexed(int, int *, MPI_Aint *, MPI_Datatype, MPI_Datatype *);
(declaim (inline PMPI_Type_hindexed))
(define-alien-routine ("PMPI_Type_hindexed" PMPI_Type_hindexed)
    int
  (count int)
  (blocklens (* int))
  (indices (* MPI_Aint))
  (old_type MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int PMPI_Type_struct(int, int *, MPI_Aint *, MPI_Datatype *, MPI_Datatype *);
(declaim (inline PMPI_Type_struct))
(define-alien-routine ("PMPI_Type_struct" PMPI_Type_struct)
    int
  (count int)
  (blocklens (* int))
  (indices (* MPI_Aint))
  (old_type MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int PMPI_Address(void*, MPI_Aint *);
(declaim (inline PMPI_Address))
(define-alien-routine ("PMPI_Address" PMPI_Address)
    int
  (location (* t))
  (address MPI_Aint :out))

;; /* We could add __attribute__((deprecated)) to routines like
;;    PMPI_Type_extent */
;; int PMPI_Type_extent(MPI_Datatype, MPI_Aint *);
(declaim (inline PMPI_Type_extent))
(define-alien-routine ("PMPI_Type_extent" PMPI_Type_extent)
    int
  (datatype MPI_Datatype)
  (extent MPI_Aint :out))

;; /* See the 1.1 version of the Standard.  The standard made an 
;;    unfortunate choice here, however, it is the standard.  The size returned 
;;    by PMPI_Type_size is specified as an int, not an MPI_Aint */
;; int PMPI_Type_size(MPI_Datatype, int *);
(declaim (inline PMPI_Type_size))
(define-alien-routine ("PMPI_Type_size" PMPI_Type_size)
    int
  (datatype MPI_Datatype)
  (size int :out))

;; /* PMPI_Type_count was withdrawn in PMPI 1.1 */

;; int PMPI_Type_lb(MPI_Datatype, MPI_Aint *);
(declaim (inline PMPI_Type_lb))
(define-alien-routine ("PMPI_Type_lb" PMPI_Type_lb)
    int
  (datatype MPI_Datatype)
  (displacement MPI_Aint :out))

;; int PMPI_Type_ub(MPI_Datatype, MPI_Aint *);
(declaim (inline PMPI_Type_ub))
(define-alien-routine ("PMPI_Type_ub" PMPI_Type_ub)
    int
  (datatype MPI_Datatype)
  (displacement MPI_Aint :out))

;; int PMPI_Type_commit(MPI_Datatype *);
(declaim (inline PMPI_Type_commit))
(define-alien-routine ("PMPI_Type_commit" PMPI_Type_commit)
    int
  (datatype (* MPI_Datatype)))

;; int PMPI_Type_free(MPI_Datatype *);
(declaim (inline PMPI_Type_free))
(define-alien-routine ("PMPI_Type_free" PMPI_Type_free)
    int
  (datatype (* MPI_Datatype)))

;; int PMPI_Get_elements(MPI_Status *, MPI_Datatype, int *);
(declaim (inline PMPI_Get_elements))
(define-alien-routine ("PMPI_Get_elements" PMPI_Get_elements)
    int
  (status (* MPI_Status))
  (datatype MPI_Datatype)
  (count int :out))

;; int PMPI_Pack(void*, int, MPI_Datatype, void *, int, int *,  MPI_Comm);
(declaim (inline PMPI_Pack))
(define-alien-routine ("PMPI_Pack" PMPI_Pack)
    int
  (inbuf (* t))
  (incount int)
  (datatype MPI_Datatype)
  (outbuf (* t))   ; [VNP] doc says :out, but contains initial address
  (outcount int)
  (position (* int))
  (comm MPI_Comm))

;; int PMPI_Unpack(void*, int, int *, void *, int, MPI_Datatype, MPI_Comm);
(declaim (inline PMPI_Unpack))
(define-alien-routine ("PMPI_Unpack" PMPI_Unpack)
    int
  (inbuf (* t))
  (insize int)
  (position (* int))
  (outbuf (* t))   ; [VNP] doc says :out, but contains initial address
  (outcount int)
  (datatype MPI_Datatype)
  (comm MPI_Comm))

;; int PMPI_Pack_size(int, MPI_Datatype, MPI_Comm, int *);
(declaim (inline PMPI_Pack_size))
(define-alien-routine ("PMPI_Pack_size" PMPI_Pack_size)
    int
  (incount int)
  (datatype MPI_Datatype)
  (comm MPI_Comm)
  (size int :out))

;; int PMPI_Barrier(MPI_Comm );
(declaim (inline PMPI_Barrier))
(define-alien-routine ("PMPI_Barrier" PMPI_Barrier)
    int
  (comm MPI_Comm))

;; int PMPI_Bcast(void* buffer, int, MPI_Datatype, int, MPI_Comm );
(declaim (inline PMPI_Bcast))
(define-alien-routine ("PMPI_Bcast" PMPI_Bcast)
    int
  (buffer (* t))
  (count int)
  (datatype MPI_Datatype)
  (root int)
  (comm MPI_Comm))

;; int PMPI_Gather(void* , int, MPI_Datatype, void*, int,
;;                MPI_Datatype, int, MPI_Comm); 
(declaim (inline PMPI_Gather))
(define-alien-routine ("PMPI_Gather" PMPI_Gather)
    int
  (sendbuf (* t))
  (sendcount int)
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcount int)
  (recvtype MPI_Datatype)
  (root int)
  (comm MPI_Comm))

;; int PMPI_Gatherv(void* , int, MPI_Datatype, void*,
;;                 int *, int *, MPI_Datatype, int, MPI_Comm); 
(declaim (inline PMPI_Gatherv))
(define-alien-routine ("PMPI_Gatherv" PMPI_Gatherv)
    int
  (sendbuf (* t))
  (sendcount int)
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcnts (* int))
  (displs (* int))
  (recvtype MPI_Datatype)
  (root int)
  (comm MPI_Comm))

;; int PMPI_Scatter(void* , int, MPI_Datatype, void*, int,
;;                 MPI_Datatype, int, MPI_Comm);
(declaim (inline PMPI_Scatter))
(define-alien-routine ("PMPI_Scatter" PMPI_Scatter)
    int
  (sendbuf (* t))
  (sendcount int)
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcount int)
  (recvtype MPI_Datatype)
  (root int)
  (comm MPI_Comm))

;; int PMPI_Scatterv(void* , int *, int *displs,
;;                  MPI_Datatype, void*, int, MPI_Datatype, int, MPI_Comm);
(declaim (inline PMPI_Scatterv))
(define-alien-routine ("PMPI_Scatterv" PMPI_Scatterv)
    int
  (sendbuf (* t))
  (sendcnts (* int))
  (displs (* int))
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcnt int)
  (recvtype MPI_Datatype)
  (root int)
  (comm MPI_Comm))

;; int PMPI_Allgather(void* , int, MPI_Datatype, void*,
;;                   int, MPI_Datatype, MPI_Comm);
(declaim (inline PMPI_Allgather))
(define-alien-routine ("PMPI_Allgather" PMPI_Allgather)
    int
  (sendbuf (* t))
  (sendcount int)
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcount int)
  (recvtype MPI_Datatype)
  (comm MPI_Comm))

;; int PMPI_Allgatherv(void* , int, MPI_Datatype,
;;                    void*, int *, int *, MPI_Datatype, MPI_Comm);
(declaim (inline PMPI_Allgatherv))
(define-alien-routine ("PMPI_Allgatherv" PMPI_Allgatherv)
    int
  (sendbuf (* t))
  (sendcount int)
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcnts (* int))
  (displs (* int))
  (recvtype MPI_Datatype)
  (comm MPI_Comm))

;; int PMPI_Alltoall(void* , int, MPI_Datatype, void*,
;;                  int, MPI_Datatype, MPI_Comm);
(declaim (inline PMPI_Alltoall))
(define-alien-routine ("PMPI_Alltoall" PMPI_Alltoall)
    int
  (sendbuf (* t))
  (sendcount int)
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcount int)
  (recvtype MPI_Datatype)
  (comm MPI_Comm))

;; int PMPI_Alltoallv(void* , int *, int *, MPI_Datatype,
;;                    void*, int *, int *, MPI_Datatype, MPI_Comm);
(declaim (inline PMPI_Alltoallv))
(define-alien-routine ("PMPI_Alltoallv" PMPI_Alltoallv)
    int
  (sendbuf (* t))
  (sendcounts (* int))
  (sdispls (* int))
  (sendtype MPI_Datatype)
  (recvbuf (* t))  ; [VNP] doc says :out, but contains initial address
  (recvcnts (* int))
  (displs (* int))
  (recvtype MPI_Datatype)
  (comm MPI_Comm))

;; int PMPI_Reduce(void* , void*, int, MPI_Datatype, MPI_Op, int, MPI_Comm);
(declaim (inline PMPI_Reduce))
(define-alien-routine ("PMPI_Reduce" PMPI_Reduce)
    int
  (sendbuf (* t))
  (recvbuf (* t))
  (count int)
  (datatype MPI_Datatype)
  (op MPI_Op)
  (root int)
  (comm MPI_Comm))

;; int PMPI_Op_create(MPI_User_function *, int, MPI_Op *);
(declaim (inline PMPI_Op_create))
(define-alien-routine ("PMPI_Op_create" PMPI_Op_create)
    int
  (function MPI_User_function)
  (commute int)
  (op MPI_Op :out))

;; int PMPI_Op_free( MPI_Op *);
(declaim (inline PMPI_Op_free))
(define-alien-routine ("PMPI_Op_free" PMPI_Op_free)
    int
  (op (* MPI_Op)))

;; int PMPI_Allreduce(void* , void*, int, MPI_Datatype, MPI_Op, MPI_Comm);
(declaim (inline PMPI_Allreduce))
(define-alien-routine ("PMPI_Allreduce" PMPI_Allreduce)
    int
  (sendbuf (* t))
  (recvbuf (* t))
  (count int)
  (datatype MPI_Datatype)
  (op MPI_Op)
  (comm MPI_Comm))

;; int PMPI_Reduce_scatter(void* , void*, int *, MPI_Datatype, MPI_Op, MPI_Comm);
(declaim (inline PMPI_Reduce_scatter))
(define-alien-routine ("PMPI_Reduce_scatter" PMPI_Reduce_scatter)
    int
  (sendbuf (* t))
  (recvbuf (* t))
  (recvcnts (* int))
  (datatype MPI_Datatype)
  (op MPI_Op)
  (comm MPI_Comm))

;; int PMPI_Scan(void* , void*, int, MPI_Datatype, MPI_Op, MPI_Comm );
(declaim (inline PMPI_Scan))
(define-alien-routine ("PMPI_Scan" PMPI_Scan)
    int
  (sendbuf (* t))
  (recvbuf (* t))
  (count int)
  (datatype MPI_Datatype)
  (op MPI_Op)
  (comm MPI_Comm))

;; int PMPI_Group_size(MPI_Group, int *);
(declaim (inline PMPI_Group_size))
(define-alien-routine ("PMPI_Group_size" PMPI_Group_size)
    int
  (group MPI_Group)
  (size int :out))

;; int PMPI_Group_rank(MPI_Group, int *);
(declaim (inline PMPI_Group_rank))
(define-alien-routine ("PMPI_Group_rank" PMPI_Group_rank)
    int
  (group MPI_Group)
  (rank int :out))

;; int PMPI_Group_translate_ranks (MPI_Group, int, int *, MPI_Group, int *);
(declaim (inline PMPI_Group_translate_ranks))
(define-alien-routine ("PMPI_Group_translate_ranks" PMPI_Group_translate_ranks)
    int
  (group1 MPI_Group)
  (n int)
  (ranks1 (* int))
  (group2 MPI_Group)
  (ranks2 (* int)))

;; int PMPI_Group_compare(MPI_Group, MPI_Group, int *);
(declaim (inline PMPI_Group_compare))
(define-alien-routine ("PMPI_Group_compare" PMPI_Group_compare)
    int
  (group1 MPI_Group)
  (group2 MPI_Group)
  (result int :out))

;; int PMPI_Comm_group(MPI_Comm, MPI_Group *);
(declaim (inline PMPI_Comm_group))
(define-alien-routine ("PMPI_Comm_group" PMPI_Comm_group)
    int
  (comm MPI_Comm)
  (group MPI_Group :out))

;; int PMPI_Group_union(MPI_Group, MPI_Group, MPI_Group *);
(declaim (inline PMPI_Group_union))
(define-alien-routine ("PMPI_Group_union" PMPI_Group_union)
    int
  (group1 MPI_Group)
  (group2 MPI_Group)
  (newgroup MPI_Group :out))

;; int PMPI_Group_intersection(MPI_Group, MPI_Group, MPI_Group *);
(declaim (inline PMPI_Group_intersection))
(define-alien-routine ("PMPI_Group_intersection" PMPI_Group_intersection)
    int
  (group1 MPI_Group)
  (group2 MPI_Group)
  (newgroup MPI_Group :out))

;; int PMPI_Group_difference(MPI_Group, MPI_Group, MPI_Group *);
(declaim (inline PMPI_Group_difference))
(define-alien-routine ("PMPI_Group_difference" PMPI_Group_difference)
    int
  (group1 MPI_Group)
  (group2 MPI_Group)
  (newgroup MPI_Group :out))

;; int PMPI_Group_incl(MPI_Group, int, int *, MPI_Group *);
(declaim (inline PMPI_Group_incl))
(define-alien-routine ("PMPI_Group_incl" PMPI_Group_incl)
    int
  (group MPI_Group)
  (n int)
  (ranks (* int))
  (newgroup MPI_Group :out))

;; int PMPI_Group_excl(MPI_Group, int, int *, MPI_Group *);
(declaim (inline PMPI_Group_excl))
(define-alien-routine ("PMPI_Group_excl" PMPI_Group_excl)
    int
  (group MPI_Group)
  (n int)
  (ranks (* int))
  (newgroup MPI_Group :out))

;; int PMPI_Group_range_incl(MPI_Group, int, int [][3], MPI_Group *);
(declaim (inline PMPI_Group_range_incl))
(define-alien-routine ("PMPI_Group_range_incl" PMPI_Group_range_incl)
    int
  (group MPI_Group)
  (n int)
  (ranges (* int))
  (newgroup MPI_Group :out))

;; int PMPI_Group_range_excl(MPI_Group, int, int [][3], MPI_Group *);
(declaim (inline PMPI_Group_range_excl))
(define-alien-routine ("PMPI_Group_range_excl" PMPI_Group_range_excl)
    int
  (group MPI_Group)
  (n int)
  (ranges (* int))
  (newgroup MPI_Group :out))

;; int PMPI_Group_free(MPI_Group *);
(declaim (inline PMPI_Group_gree))
(define-alien-routine ("PMPI_Group_free" PMPI_Group_free)
    int
  (group (* MPI_Group)))

;; int PMPI_Comm_size(MPI_Comm, int *);
(declaim (inline PMPI_Comm_size))
(define-alien-routine ("PMPI_Comm_size" PMPI_Comm_size)
    int
  (comm MPI_Comm)
  (size int :out))

;; int PMPI_Comm_rank(MPI_Comm, int *);
(declaim (inline PMPI_Comm_rank))
(define-alien-routine ("PMPI_Comm_rank" PMPI_Comm_rank)
    int
  (comm MPI_Comm)
  (rank int :out))

;; int PMPI_Comm_compare(MPI_Comm, MPI_Comm, int *);
(declaim (inline PMPI_Comm_compare))
(define-alien-routine ("PMPI_Comm_compare" PMPI_Comm_compare)
    int
  (comm1 MPI_Comm)
  (comm2 MPI_Comm)
  (result int :out))

;; int PMPI_Comm_dup(MPI_Comm, MPI_Comm *);
(declaim (inline PMPI_Comm_dup))
(define-alien-routine ("PMPI_Comm_dup" PMPI_Comm_dup)
    int
  (incomm MPI_Comm)
  (outcomm MPI_Comm :out))

;; int PMPI_Comm_create(MPI_Comm, MPI_Group, MPI_Comm *);
(declaim (inline PMPI_Comm_create))
(define-alien-routine ("PMPI_Comm_create" PMPI_Comm_create)
    int
  (incomm MPI_Comm)
  (group MPI_Group)
  (outcomm MPI_Comm :out))

;; int PMPI_Comm_split(MPI_Comm, int, int, MPI_Comm *);
(declaim (inline PMPI_Comm_split))
(define-alien-routine ("PMPI_Comm_split" PMPI_Comm_split)
    int
  (comm MPI_Comm)
  (color int)
  (key int)
  (newcomm MPI_Comm :out))

;; int PMPI_Comm_free(MPI_Comm *);
(declaim (inline PMPI_Comm_free))
(define-alien-routine ("PMPI_Comm_free" PMPI_Comm_free)
    int
  (comm (* MPI_Comm)))

;; int PMPI_Comm_test_inter(MPI_Comm, int *);
(declaim (inline PMPI_Comm_test_inter))
(define-alien-routine ("PMPI_Comm_test_inter" PMPI_Comm_test_inter)
    int
  (comm MPI_Comm)
  (flag int :out))

;; int PMPI_Comm_remote_size(MPI_Comm, int *);
(declaim (inline PMPI_Comm_remote_size))
(define-alien-routine ("PMPI_Comm_remote_size" PMPI_Comm_remote_size)
    int
  (comm MPI_Comm)
  (size int :out))

;; int PMPI_Comm_remote_group(MPI_Comm, MPI_Group *);
(declaim (inline PMPI_Comm_remote_group))
(define-alien-routine ("PMPI_Comm_remote_group" PMPI_Comm_remote_group)
    int
  (comm MPI_Comm)
  (group MPI_Group :out))

;; int PMPI_Intercomm_create(MPI_Comm, int, MPI_Comm, int, int, MPI_Comm *);
(declaim (inline PMPI_Intercomm_create))
(define-alien-routine ("PMPI_Intercomm_create" PMPI_Intercomm_create)
    int
  (local_comm MPI_Comm)
  (local_leader int)
  (peer_comm MPI_Comm)
  (remote_leader int)
  (tag int)
  (comm_out MPI_Comm :out))

;; int PMPI_Intercomm_merge(MPI_Comm, int, MPI_Comm *);
(declaim (inline PMPI_Intercomm_merge))
(define-alien-routine ("PMPI_Intercomm_merge" PMPI_Intercomm_merge)
    int
  (comm MPI_Comm)
  (high int)
  (comm_out MPI_Comm :out))

;; int PMPI_Keyval_create(MPI_Copy_function *, MPI_Delete_function *,
;;                        int *, void*);
(declaim (inline PMPI_Keyval_create))
(define-alien-routine ("PMPI_Keyval_create" PMPI_Keyval_create)
    int
  (copy_fn MPI_Copy_function)
  (delete_fn MPI_Delete_function)
  (keyval int :out)
  (extra_state (* t)))

;; int PMPI_Keyval_free(int *);
(declaim (inline PMPI_Keyval_free))
(define-alien-routine ("PMPI_Keyval_free" PMPI_Keyval_free)
    int
  (keyval (* int)))

;; int PMPI_Attr_put(MPI_Comm, int, void*);
(declaim (inline PMPI_Attr_put))
(define-alien-routine ("PMPI_Attr_put" PMPI_Attr_put)
    int
  (comm MPI_Comm)
  (keyval int)
  (attr_val (* t)))

;; int PMPI_Attr_get(MPI_Comm, int, void *, int *);
(declaim (inline PMPI_Attr_get))
(define-alien-routine ("PMPI_Attr_get" PMPI_Attr_get)
    int
  (comm MPI_Comm)
  (keyval int)
  (attr_value (* t))
  (flag int :out))

;; int PMPI_Attr_delete(MPI_Comm, int);
(declaim (inline PMPI_Attr_delete))
(define-alien-routine ("PMPI_Attr_delete" PMPI_Attr_delete)
    int
  (comm MPI_Comm)
  (keyval int))

;; int PMPI_Topo_test(MPI_Comm, int *);
(declaim (inline PMPI_Topo_test))
(define-alien-routine ("PMPI_Topo_test" PMPI_Topo_test)
    int
  (comm MPI_Comm)
  (top_type int :out))

;; int PMPI_Cart_create(MPI_Comm, int, int *, int *, int, MPI_Comm *);
(declaim (inline PMPI_Cart_create))
(define-alien-routine ("PMPI_Cart_create" PMPI_Cart_create)
    int
  (comm_old MPI_Comm)
  (ndims int)
  (dims (* int))
  (periods (* int))
  (reorder int)
  (comm_cart MPI_Comm :out))

;; int PMPI_Dims_create(int, int, int *);
(declaim (inline PMPI_Dims_create))
(define-alien-routine ("PMPI_Dims_create" PMPI_Dims_create)
    int
  (nnodes int)
  (ndims int)
  (dims (* int)))

;; int PMPI_Graph_create(MPI_Comm, int, int *, int *, int, MPI_Comm *);
(declaim (inline PMPI_Graph_create))
(define-alien-routine ("PMPI_Graph_create" PMPI_Graph_create)
    int
  (comm_old MPI_Comm)
  (nnodes int)
  (index (* int))
  (edges (* int))
  (reorder int)
  (comm_graph MPI_Comm :out))

;; int PMPI_Graphdims_get(MPI_Comm, int *, int *);
(declaim (inline PMPI_Graphdims_get))
(define-alien-routine ("PMPI_Graphdims_get" PMPI_Graphdims_get)
    int
  (comm MPI_Comm)
  (nnodes int :out)
  (nedges int :out))

;; int PMPI_Graph_get(MPI_Comm, int, int, int *, int *);
(declaim (inline PMPI_Graph_get))
(define-alien-routine ("PMPI_Graph_get" PMPI_Graph_get)
    int
  (comm MPI_Comm)
  (maxindex int)
  (maxedges int)
  (index (* int))
  (edges (* int)))

;; int PMPI_Cartdim_get(MPI_Comm, int *);
(declaim (inline PMPI_Cartdim_get))
(define-alien-routine ("PMPI_Cartdim_get" PMPI_Cartdim_get)
    int
  (comm MPI_Comm)
  (ndims int :out))

;; int PMPI_Cart_get(MPI_Comm, int, int *, int *, int *);
(declaim (inline PMPI_Cart_get))
(define-alien-routine ("PMPI_Cart_get" PMPI_Cart_get)
    int
  (comm MPI_Comm)
  (maxdims int)
  (dims (* int))
  (periods (* int))
  (coords (* int)))

;; int PMPI_Cart_rank(MPI_Comm, int *, int *);
(declaim (inline PMPI_Cart_rank))
(define-alien-routine ("PMPI_Cart_rank" PMPI_Cart_rank)
    int
  (comm MPI_Comm)
  (coords (* int))
  (rank int :out))

;; int PMPI_Cart_coords(MPI_Comm, int, int, int *);
(declaim (inline PMPI_Cart_coords))
(define-alien-routine ("PMPI_Cart_coords" PMPI_Cart_coords)
    int
  (comm MPI_Comm)
  (rank int)
  (maxdims int)
  (coords (* int)))

;; int PMPI_Graph_neighbors_count(MPI_Comm, int, int *);
(declaim (inline PMPI_Graph_neighbors_count))
(define-alien-routine ("PMPI_Graph_neighbors_count" PMPI_Graph_neighbors_count)
    int
  (comm MPI_Comm)
  (rank int)
  (neighbours int :out))

;; int PMPI_Graph_neighbors(MPI_Comm, int, int, int *);
(declaim (inline PMPI_Graph_neighbors))
(define-alien-routine ("PMPI_Graph_neighbors" PMPI_Graph_neighbors)
    int
  (comm MPI_Comm)
  (rank int)
  (maxneighbors int)
  (neighbors (* int)))

;; int PMPI_Cart_shift(MPI_Comm, int, int, int *, int *);
(declaim (inline PMPI_Cart_shift))
(define-alien-routine ("PMPI_Cart_shift" PMPI_Cart_shift)
    int
  (comm MPI_Comm)
  (direction int)
  (displ int)
  (rank_source int :out)
  (rank_dest int :out))

;; int PMPI_Cart_sub(MPI_Comm, int *, MPI_Comm *);
(declaim (inline PMPI_Cart_sub))
(define-alien-routine ("PMPI_Cart_sub" PMPI_Cart_sub)
    int
  (comm MPI_Comm)
  (remain_dims (* int))
  (newcomm MPI_Comm :out))

;; int PMPI_Cart_map(MPI_Comm, int, int *, int *, int *);
(declaim (inline PMPI_Cart_map))
(define-alien-routine ("PMPI_Cart_map" PMPI_Cart_map)
    int
  (comm_old MPI_Comm)
  (ndims int)
  (dims (* int))
  (periods (* int))
  (newrank int :out))

;; int PMPI_Graph_map(MPI_Comm, int, int *, int *, int *);
(declaim (inline PMPI_Graph_map))
(define-alien-routine ("PMPI_Graph_map" PMPI_Graph_map)
    int
  (comm MPI_Comm)
  (nnodes int)
  (index (* int))
  (edges (* int))
  (newrank int :out))

;; int PMPI_Get_processor_name(char *, int *);
(declaim (inline PMPI_Get_processor_name))
(define-alien-routine ("PMPI_Get_processor_name" PMPI_Get_processor_name)
    int
  (name (* char))
  (resultlen int :out))

;; int PMPI_Get_version(int *, int *);
(declaim (inline PMPI_Get_version))
(define-alien-routine ("PMPI_Get_version" PMPI_Get_version)
    int
  (version int :out)
  (subversion int :out))

;; int PMPI_Errhandler_create(MPI_Handler_function *, MPI_Errhandler *);
(declaim (inline PMPI_Errhandler_create))
(define-alien-routine ("PMPI_Errhandler_create" PMPI_Errhandler_create)
    int
  (function MPI_Handler_function)
  (errhandler MPI_Errhandler :out))

;; int PMPI_Errhandler_set(MPI_Comm, MPI_Errhandler);
(declaim (inline PMPI_Errhandler_set))
(define-alien-routine ("PMPI_Errhandler_set" PMPI_Errhandler_set)
    int
  (comm MPI_Comm)
  (errhandler MPI_Errhandler))

;; int PMPI_Errhandler_get(MPI_Comm, MPI_Errhandler *);
(declaim (inline PMPI_Errhandler_get))
(define-alien-routine ("PMPI_Errhandler_get" PMPI_Errhandler_get)
    int
  (comm MPI_Comm)
  (errhandler MPI_Errhandler :out))

;; int PMPI_Errhandler_free(MPI_Errhandler *);
(declaim (inline PMPI_Errhandler_free))
(define-alien-routine ("PMPI_Errhandler_free" PMPI_Errhandler_free)
    int
  (errhandler (* MPI_Errhandler)))

;; int PMPI_Error_string(int, char *, int *);
(declaim (inline PMPI_Error_string))
(define-alien-routine ("PMPI_Error_string" PMPI_Error_string)
    int
  (errcode int)
  (string (* char))
  (resultlen int :out))

;; int PPMPI_Error_class(int, int *);
(declaim (inline PMPI_Error_class))
(define-alien-routine ("PMPI_Error_class" PMPI_Error_class)
    int
  (errorcode int)
  (errorclass int :out))

;; #ifndef PMPI_Wtime
;; double PMPI_Wtime(void);
(declaim (inline PMPI_Wtime))
(define-alien-routine ("PMPI_Wtime" PMPI_Wtime)
    double)

;; double PMPI_Wtick(void);
(declaim (inline PMPI_Wtick))
(define-alien-routine ("PMPI_Wtick" PMPI_Wtick)
    double)
;; #endif

;; int PMPI_Init(int *, char ***);
(declaim (inline PMPI_Init))
(define-alien-routine ("PMPI_Init" PMPI_Init)
    int
  (argc (* int))
  (argv (* (array c-string))))

;; int PMPI_Finalize(void);
(declaim (inline PMPI_Finalize))
(define-alien-routine ("PMPI_Finalize" PMPI_Finalize)
    int)

;; int PMPI_Initialized(int *);
(declaim (inline PMPI_Initialized))
(define-alien-routine ("PMPI_Initialized" PMPI_Initialized)
    int
  (flag int :out))

;; int PMPI_Abort(MPI_Comm, int);
(declaim (inline PMPI_Abort))
(define-alien-routine ("PMPI_Abort" PMPI_Abort)
    int
  (comm MPI_Comm)
  (errcode int))


;; /* Note that we may need to define a @PCONTROL_LIST@ depending on whether 
;;    stdargs are supported */
;; int PMPI_Pcontrol(const int, ...);
(declaim (inline PMPI_Pcontrol))
(define-alien-routine ("PMPI_Pcontrol" PMPI_Pcontrol)
    int
  (level int))

;; /* PMPI-2 functions */

;; /* Process Creation and Management */
;; int PMPI_Close_port(char *);
(declaim (inline PMPI_Close_port))
(define-alien-routine ("PMPI_Close_port" PMPI_Close_port)
    int
  (port_name c-string))

;; int PMPI_Comm_accept(char *, MPI_Info, int, MPI_Comm, MPI_Comm *);
(declaim (inline PMPI_Comm_accept))
(define-alien-routine ("PMPI_Comm_accept" PMPI_Comm_accept)
    int
  (port_name c-string)
  (info MPI_Info)
  (root int)
  (comm MPI_Comm)
  (new_comm MPI_Comm :out))
  
;; int PMPI_Comm_connect(char *, MPI_Info, int, MPI_Comm, MPI_Comm *);
(declaim (inline PMPI_Comm_connect))
(define-alien-routine ("PMPI_Comm_connect" PMPI_Comm_connect)
    int
  (port_name c-string)
  (info MPI_Info)
  (root int)
  (comm MPI_Comm)
  (new_comm MPI_Comm :out))

;; int PMPI_Comm_disconnect(MPI_Comm *);
(declaim (inline PMPI_Comm_disconnect))
(define-alien-routine ("PMPI_Comm_disconnect" PMPI_Comm_disconnect)
    int
  (comm (* MPI_Comm)))

;; int PMPI_Comm_get_parent(MPI_Comm *);
(declaim (inline PMPI_Comm_get_parent))
(define-alien-routine ("PMPI_Comm_get_parent" PMPI_Comm_get_parent)
    int
  (comm MPI_Comm :out))

;; int PMPI_Comm_join(int, MPI_Comm *);
(declaim (inline PMPI_Comm_join))
(define-alien-routine ("PMPI_Comm_join" PMPI_Comm_join)
    int
  (fd int)
  (intercomm MPI_Comm :out))

;; int PMPI_Comm_spawn(char *, char *[], int, MPI_Info,
;;                    int, MPI_Comm, MPI_Comm *, int []);
(declaim (inline PMPI_Comm_spawn))
(define-alien-routine ("PMPI_Comm_spawn" PMPI_Comm_spawn)
    int
  (command c-string)
  (argv (array c-string))
  (maxprocs int)
  (info MPI_Info)
  (root int)
  (comm MPI_Comm)
  (intercomm MPI_Comm :out)
  (array_of_errcodes (* int)))

;; int PMPI_Comm_spawn_multiple(int, char *[], char **[], int [],
;;                             MPI_Info [], int, MPI_Comm, MPI_Comm *, int []);
(declaim (inline PMPI_Comm_spawn_multiple))
(define-alien-routine ("PMPI_Comm_spawn_multiple" PMPI_Comm_spawn_multiple)
    int
  (count int)
  (array_of_commands (array c-string))
  (array_of_argv (* (array c-string)))
  (array_of_maxprocs (* int))
  (array_of_info (* MPI_Info))
  (root int)
  (comm MPI_Comm)
  (intercomm MPI_Comm :out)
  (array_of_errcodes (* int)))

;; int PMPI_Lookup_name(char *, MPI_Info, char *);
(declaim (inline PMPI_Lookup_name))
(define-alien-routine ("PMPI_Lookup_name" PMPI_Lookup_name)
    int
  (service_name c-string)
  (info MPI_Info)
  (port_name (* char)))

;; int PMPI_Open_port(MPI_Info, char *);
(declaim (inline PMPI_Open_port))
(define-alien-routine ("PMPI_Open_port" PMPI_Open_port)
    int
  (info MPI_Info)
  (port_name (* char)))

;; int PMPI_Publish_name(char *, MPI_Info, char *);
(declaim (inline PMPI_Publish_name))
(define-alien-routine ("PMPI_Publish_name" PMPI_Publish_name)
    int
  (service_name c-string)
  (info MPI_Info)
  (port_name (* char)))

;; int PMPI_Unpublish_name(char *, MPI_Info, char *);
(declaim (inline PMPI_Unpublish_name))
(define-alien-routine ("PMPI_Unpublish_name" PMPI_Unpublish_name)
    int
  (service_name c-string)
  (info MPI_Info)
  (port_name (* char)))

;; /* One-Sided Communications */
;; int PMPI_Accumulate(void *, int, MPI_Datatype, int, MPI_Aint, int, 
;; 		      MPI_Datatype,  MPI_Op, MPI_Win);
(declaim (inline PMPI_Accumulate))
(define-alien-routine ("PMPI_Accumulate" PMPI_Accumulate)
    int
  (origin_addr (* t))
  (origin_count int)
  (origin_datatype MPI_Datatype)
  (target_rank int)
  (target_disp MPI_Aint)
  (target_count int)
  (target_datatype MPI_Datatype)
  (op MPI_Op)
  (win MPI_Win))

;; int PMPI_Get(void *, int, MPI_Datatype, int, MPI_Aint, int, MPI_Datatype, 
;; 	       MPI_Win);
(declaim (inline PMPI_Get))
(define-alien-routine ("PMPI_Get" PMPI_Get)
    int
  (origin_addr (* t))
  (origin_count int)
  (origin_datatype MPI_Datatype)
  (target_rank int)
  (target_disp MPI_Aint)
  (target_count int)
  (target_datatype MPI_Datatype)
  (win MPI_Win))
  
;; int PMPI_Put(void *, int, MPI_Datatype, int, MPI_Aint, int, MPI_Datatype, 
;; 	       MPI_Win);
(declaim (inline PMPI_Put))
(define-alien-routine ("PMPI_Put" PMPI_Put)
    int
  (origin_addr (* t))
  (origin_count int)
  (origin_datatype MPI_Datatype)
  (target_rank int)
  (target_disp MPI_Aint)
  (target_count int)
  (target_datatype MPI_Datatype)
  (win MPI_Win))

;; int PMPI_Win_complete(MPI_Win);
(declaim (inline PMPI_Win_complete))
(define-alien-routine ("PMPI_Win_complete" PMPI_Win_complete)
    int
  (win MPI_Win))

;; int PMPI_Win_create(void *, MPI_Aint, int, MPI_Info, MPI_Comm, MPI_Win *);
(declaim (inline PMPI_Win_create))
(define-alien-routine ("PMPI_Win_create" PMPI_Win_create)
    int
  (base (* t))
  (size MPI_Aint)
  (disp_unit int)
  (info MPI_Info)
  (comm MPI_Comm)
  (win MPI_Win :out))

;; int PMPI_Win_fence(int, MPI_Win);
(declaim (inline PMPI_Win_fence))
(define-alien-routine ("PMPI_Win_fence" PMPI_Win_fence)
    int
  (_assert int)
  (win MPI_Win))

;; int PMPI_Win_free(MPI_Win *);
(declaim (inline PMPI_Win_free))
(define-alien-routine ("PMPI_Win_free" PMPI_Win_free)
    int
  (win (* MPI_Win)))

;; int PMPI_Win_get_group(MPI_Win, MPI_Group *);
(declaim (inline PMPI_Win_get_group))
(define-alien-routine ("PMPI_Win_get_group" PMPI_Win_get_group)
    int
  (win MPI_Win)
  (group MPI_Group :out))

;; int PMPI_Win_lock(int, int, int, MPI_Win);
(declaim (inline PMPI_Win_lock))
(define-alien-routine ("PMPI_Win_lock" PMPI_Win_lock)
    int
  (lock_type int)
  (rank int)
  (_assert int)
  (win MPI_Win))

;; int PMPI_Win_post(MPI_Group, int, MPI_Win);
(declaim (inline PMPI_Win_post))
(define-alien-routine ("PMPI_Win_post" PMPI_Win_post)
    int
  (group MPI_Group)
  (_assert int)
  (win MPI_Win))

;; int PMPI_Win_start(MPI_Group, int, MPI_Win);
(declaim (inline PMPI_Win_start))
(define-alien-routine ("PMPI_Win_start" PMPI_Win_start)
    int
  (group MPI_Group)
  (_assert int)
  (win MPI_Win))

;; int PMPI_Win_test(MPI_Win, int *);
(declaim (inline PMPI_Win_test))
(define-alien-routine ("PMPI_Win_test" PMPI_Win_test)
    int
  (win MPI_Win)
  (flag int :out))

;; int PMPI_Win_unlock(int, MPI_Win);
(declaim (inline PMPI_Win_unlock))
(define-alien-routine ("PMPI_Win_unlock" PMPI_Win_unlock)
    int
  (rank int)
  (win MPI_Win))

;; int PMPI_Win_wait(MPI_Win);
(declaim (inline PMPI_Win_wait))
(define-alien-routine ("PMPI_Win_wait" PMPI_Win_wait)
    int
  (win MPI_Win))
 
;; /* Extended Collective Operations */
;; int PMPI_Alltoallw(void *, int [], int [], MPI_Datatype [], void *, int [], 
;; 		     int [], MPI_Datatype [], MPI_Comm);
(declaim (inline PMPI_Alltoallw))
(define-alien-routine ("PMPI_Alltoallw" PMPI_Alltoallw)
    int
  (sendbuf (* t))
  (sendcnts (* int))
  (sdispls (* int))
  (sendtypes (* MPI_Datatype))
  (recvbuf (* t))
  (recvcnts (* int))
  (rdispls (* int))
  (recvtypes (* MPI_Datatype))
  (comm MPI_Comm))

;; int PMPI_Exscan(void *, void *, int, MPI_Datatype, MPI_Op, MPI_Comm) ;
(declaim (inline PMPI_Exscan))
(define-alien-routine ("PMPI_Exscan" PMPI_Exscan)
    int
  (sendbuf (* t))
  (recvbuf (* t))
  (count int)
  (datatype MPI_Datatype)
  (op MPI_Op)
  (comm MPI_Comm))
 
;; /* External Interfaces */
;; int PMPI_Add_error_class(int *);
(declaim (inline PMPI_Add_error_class))
(define-alien-routine ("PMPI_Add_error_class" PMPI_Add_error_class)
    int
  (erroclass int :out))

;; int PMPI_Add_error_code(int, int *);
(declaim (inline PMPI_Add_error_code))
(define-alien-routine ("PMPI_Add_error_code" PMPI_Add_error_code)
    int
  (errorclass int)
  (errorcode int :out))

;; int PMPI_Add_error_string(int, char *);
(declaim (inline PMPI_Add_error_string))
(define-alien-routine ("PMPI_Add_error_string" PMPI_Add_error_string)
    int
  (errcode int)
  (string c-string))

;; int PMPI_Comm_call_errhandler(MPI_Comm, int);
(declaim (inline PMPI_Comm_call_errhandler))
(define-alien-routine ("PMPI_Comm_call_errhandler" PMPI_Comm_call_errhandler)
    int
  (comm MPI_Comm)
  (errcode int))

;; int PMPI_Comm_create_keyval(MPI_Comm_copy_attr_function *, 
;;                            MPI_Comm_delete_attr_function *, int *, void *);
(declaim (inline PMPI_Comm_create_keyval))
(define-alien-routine ("PMPI_Comm_create_keyval" PMPI_Comm_create_keyval)
    int
  (comm_copy_attr_fn MPI_Comm_copy_attr_function)
  (comm_delete_attr_fn MPI_Comm_delete_attr_function)
  (comm_keyval int :out)
  (extra_state (* t)))

;; int PMPI_Comm_delete_attr(MPI_Comm, int);
(declaim (inline PMPI_Comm_delete_attr))
(define-alien-routine ("PMPI_Comm_delete_attr" PMPI_Comm_delete_attr)
    int
  (comm MPI_Comm)
  (comm_keyval int))

;; int PMPI_Comm_free_keyval(int *);
(declaim (inline PMPI_Comm_free_keyval))
(define-alien-routine ("PMPI_Comm_free_keyval" PMPI_Comm_free_keyval)
    int
  (comm_keyval (* int)))

;; int PMPI_Comm_get_attr(MPI_Comm, int, void *, int *);
(declaim (inline PMPI_Comm_get_attr))
(define-alien-routine ("PMPI_Comm_get_attr" PMPI_Comm_get_attr)
    int
  (comm MPI_Comm)
  (comm_keyval int)
  (attribute_val (* t))
  (flag int :out))

;; int PMPI_Comm_get_name(MPI_Comm, char *, int *);
(declaim (inline PMPI_Comm_get_name))
(define-alien-routine ("PMPI_Comm_get_name" PMPI_Comm_get_name)
    int
  (comm MPI_Comm)
  (comm_name (* char))
  (resultlen int :out))

;; int PMPI_Comm_set_attr(MPI_Comm, int, void *);
(declaim (inline PMPI_Comm_Set_attr))
(define-alien-routine ("PMPI_Comm_Set_attr" PMPI_Comm_Set_attr)
    int
  (comm MPI_Comm)
  (comm_keyval int)
  (attribute_val (* t)))

;; int PMPI_Comm_set_name(MPI_Comm, char *);
(declaim (inline PMPI_Comm_set_name))
(define-alien-routine ("PMPI_Comm_set_name" PMPI_Comm_set_name)
    int
  (comm MPI_Comm)
  (comm_name c-string))

;; int PMPI_File_call_errhandler(MPI_File, int);
(declaim (inline PMPI_File_call_errhandler))
(define-alien-routine ("PMPI_File_call_errhandler" PMPI_File_call_errhandler)
    int
  (fh MPI_File)
  (errcode int))

;; int PMPI_Grequest_complete(MPI_Request);
(declaim (inline PMPI_Grequest_complete))
(define-alien-routine ("PMPI_Grequest_complete" PMPI_Grequest_complete)
    int
  (request MPI_Request))

;; int PMPI_Grequest_start(MPI_Grequest_query_function *, 
;;                        MPI_Grequest_free_function *, 
;;                        MPI_Grequest_cancel_function *, void *,
;;                        MPI_Request *);
(declaim (inline PMPI_Grequest_start))
(define-alien-routine ("PMPI_Grequest_start" PMPI_Grequest_start)
    int
  (query_fn MPI_Grequest_query_function)
  (free_fn MPI_Grequest_free_function)
  (cancel_fn MPI_Grequest_cancel_function)
  (extra_state (* t))
  (request MPI_Request :out))

;; int PMPI_Init_thread(int *, char ***, int, int *);
(declaim (inline PMPI_Init_thread))
(define-alien-routine ("PMPI_Init_thread" PMPI_Init_thread)
    int
  (argc (* int))
  (argv (* (array c-string)))
  (required int)
  (provided int :out))

;; int PMPI_Is_thread_main(int *);
(declaim (inline PMPI_Is_thread_main))
(define-alien-routine ("PMPI_Is_thread_main" PMPI_Is_thread_main)
    int
  (flag int :out))

;; int PMPI_Query_thread(int *);
(declaim (inline PMPI_Query_thread))
(define-alien-routine ("PMPI_Query_thread" PMPI_Query_thread)
    int
  (provided int :out))

;; int PMPI_Status_set_cancelled(MPI_Status *, int);
(declaim (inline PMPI_Status_set_cancelled))
(define-alien-routine ("PMPI_Status_set_cancelled" PMPI_Status_set_cancelled)
    int
  (status (* MPI_Status))
  (flag int))

;; int PMPI_Status_set_elements(MPI_Status *, MPI_Datatype, int);
(declaim (inline PMPI_Status_set_elements))
(define-alien-routine ("PMPI_Status_set_elements" PMPI_Status_set_elements)
    int
  (status (* MPI_Status))
  (datatype MPI_Datatype)
  (count int))

;; int PMPI_Type_create_keyval(MPI_Type_copy_attr_function *, 
;;                            MPI_Type_delete_attr_function *, int *, void *);
(declaim (inline PMPI_Type_create_keyval))
(define-alien-routine ("PMPI_Type_create_keyval" PMPI_Type_create_keyval)
    int
  (type_copy_attr_fn MPI_Type_copy_attr_function)
  (type_delete_attr_fn MPI_Type_delete_attr_function)
  (type_keyval int :out)
  (extra_state (* t)))

;; int PMPI_Type_delete_attr(MPI_Datatype, int);
(declaim (inline PMPI_Type_delete_attr))
(define-alien-routine ("PMPI_Type_delete_attr" PMPI_Type_delete_attr)
    int
  (type MPI_Datatype)
  (type_keyval int))

;; int PMPI_Type_dup(MPI_Datatype, MPI_Datatype *);
(declaim (inline PMPI_Type_dup))
(define-alien-routine ("PMPI_Type_dup" PMPI_Type_dup)
    int
  (datatype MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int PMPI_Type_free_keyval(int *);
(declaim (inline PMPI_Type_free_keyval))
(define-alien-routine ("PMPI_Type_free_keyval" PMPI_Type_free_keyval)
    int
  (type_keyval (* int)))

;; int PMPI_Type_get_attr(MPI_Datatype, int, void *, int *);
(declaim (inline PMPI_Type_get_attr))
(define-alien-routine ("PMPI_Type_get_attr" PMPI_Type_get_attr)
    int
  (type MPI_Datatype)
  (type_keyval int)
  (attribute_val (* t))
  (flag int :out))

;; int PMPI_Type_get_contents(MPI_Datatype, int, int, int, int [], MPI_Aint [], 
;;                           MPI_Datatype []);
(declaim (inline PMPI_Type_get_contents))
(define-alien-routine ("PMPI_Type_get_contents" PMPI_Type_get_contents)
    int
  (datatype MPI_Datatype)
  (max_integers int)
  (max_addresses int)
  (max_datatypes int)
  (array_of_integers (* int))
  (array_of_addresses (* MPI_Aint))
  (array_of_datatypes (* MPI_Datatype)))

;; int PMPI_Type_get_envelope(MPI_Datatype, int *, int *, int *, int *);
(declaim (inline PMPI_Type_get_envelope))
(define-alien-routine ("PMPI_Type_get_envelope" PMPI_Type_get_envelope)
    int
  (datatype MPI_Datatype)
  (num_integers int :out)
  (num_addresses int :out)
  (num_datatypes int :out)
  (combiner int :out))

;; int PMPI_Type_get_name(MPI_Datatype, char *, int *);
(declaim (inline PMPI_Type_get_name))
(define-alien-routine ("PMPI_Type_get_name" PMPI_Type_get_name)
    int
  (datatype MPI_Datatype)
  (type_name (* char))
  (resultlen int :out))

;; int PMPI_Type_set_attr(MPI_Datatype, int, void *);
(declaim (inline PMPI_Type_set_attr))
(define-alien-routine ("PMPI_Type_set_attr" PMPI_Type_set_attr)
    int
  (type MPI_Datatype)
  (type_keyval int)
  (attribute_val (* t)))

;; int PMPI_Type_set_name(MPI_Datatype, char *);
(declaim (inline PMPI_Type_set_name))
(define-alien-routine ("PMPI_Type_set_name" PMPI_Type_set_name)
    int
  (type MPI_Datatype)
  (type_name c-string))

;; int PMPI_Type_match_size( int, int, MPI_Datatype *);
(declaim (inline PMPI_Type_match_size))
(define-alien-routine ("PMPI_Type_match_size" PMPI_Type_match_size)
    int
  (typeclass int)
  (size int)
  (datatype MPI_Datatype :out))

;; int PMPI_Win_call_errhandler(MPI_Win, int);
(declaim (inline PMPI_Win_call_errhandler))
(define-alien-routine ("PMPI_Win_call_errhandler" PMPI_Win_call_errhandler)
    int
  (win MPI_Win)
  (errorcode int))

;; int PMPI_Win_create_keyval(MPI_Win_copy_attr_function *, 
;;                          MPI_Win_delete_attr_function *, int *, void *);
(declaim (inline PMPI_Win_create_keyval))
(define-alien-routine ("PMPI_Win_create_keyval" PMPI_Win_create_keyval)
    int
  (win_copy_attr_fn MPI_Win_copy_attr_function)
  (win_delete_attr_fn MPI_Win_delete_attr_function)
  (win_keyval int :out)
  (extra_state (* t)))

;; int PMPI_Win_delete_attr(MPI_Win, int);
(declaim (inline PMPI_Win_delete_attr))
(define-alien-routine ("PMPI_Win_delete_attr" PMPI_Win_delete_attr)
    int
  (win MPI_Win)
  (win_keyval int))

;; int PMPI_Win_free_keyval(int *);
(declaim (inline PMPI_Win_free_keyval))
(define-alien-routine ("PMPI_Win_free_keyval" PMPI_Win_free_keyval)
    int
  (win_keyval (* int)))

;; int PMPI_Win_get_attr(MPI_Win, int, void *, int *);
(declaim (inline PMPI_Win_get_attr))
(define-alien-routine ("PMPI_Win_get_attr" PMPI_Win_get_attr)
    int
  (win MPI_Win)
  (win_keyval int)
  (attribute_val (* t))
  (flag int :out))

;; int PMPI_Win_get_name(MPI_Win, char *, int *);
(declaim (inline PMPI_Win_get_name))
(define-alien-routine ("PMPI_Win_get_name" PMPI_Win_get_name)
    int
  (win MPI_Win)
  (win_name (* char))
  (resultlen int :out))

;; int PMPI_Win_set_attr(MPI_Win, int, void *);
(declaim (inline PMPI_Win_set_attr))
(define-alien-routine ("PMPI_Win_set_attr" PMPI_Win_set_attr)
    int
  (win MPI_Win)
  (win_keyval int)
  (attribute_val (* t)))

;; int PMPI_Win_set_name(MPI_Win, char *);
(declaim (inline PMPI_Win_set_name))
(define-alien-routine ("PMPI_Win_set_name" PMPI_Win_set_name)
    int
  (win MPI_Win)
  (win_name c-string))

;; /* Miscellany */

;; int PMPI_Alloc_mem(MPI_Aint, MPI_Info info, void *baseptr);
(declaim (inline PMPI_Alloc_mem))
(define-alien-routine ("PMPI_Alloc_mem" PMPI_Alloc_mem)
    int
  (size MPI_Aint)
  (info MPI_Info)
  (baseptr (* (* t))))

;; int MPI_Comm_create_errhandler(MPI_Comm_errhandler_fn *, MPI_Errhandler *);
(declaim (inline PMPI_Comm_create_errhandler))
(define-alien-routine ("PMPI_Comm_create_errhandler"
		       PMPI_Comm_create_errhandler)
    int
  (function MPI_Comm_errhandler_fn)
  (errhandler MPI_Errhandler :out))

;; int PMPI_Comm_get_errhandler(MPI_Comm, MPI_Errhandler *);
(declaim (inline PMPI_Comm_get_errhandler))
(define-alien-routine ("PMPI_Comm_get_errhandler" PMPI_Comm_get_errhandler)
    int
  (comm MPI_Comm)
  (errhandler MPI_Errhandler :out))

;; int PMPI_Comm_set_errhandler(MPI_Comm, MPI_Errhandler);
(declaim (inline PMPI_Comm_set_errhandler))
(define-alien-routine ("PMPI_Comm_set_errhandler" PMPI_Comm_set_errhandler)
    int
  (comm MPI_Comm)
  (errhandler MPI_Errhandler))

;; int PMPI_File_create_errhandler(MPI_File_errhandler_fn *, MPI_Errhandler *);
(declaim (inline PMPI_File_create_errhandler))
(define-alien-routine ("PMPI_File_create_errhandler"
		       PMPI_File_create_errhandler)
    int
  (function MPI_File_errhandler_fn)
  (errhandler MPI_Errhandler :out))

;; int PMPI_File_get_errhandler(MPI_File, MPI_Errhandler *);
(declaim (inline PMPI_File_get_errhandler))
(define-alien-routine ("PMPI_File_get_errhandler" PMPI_File_get_errhandler)
    int
  (file MPI_File)
  (errhandler MPI_Errhandler :out))

;; int PMPI_File_set_errhandler(MPI_File, MPI_Errhandler);
(declaim (inline PMPI_File_set_errhandler))
(define-alien-routine ("PMPI_File_set_errhandler" PMPI_File_set_errhandler)
    int
  (file MPI_File)
  (errhandler MPI_Errhandler))

;; int PMPI_Finalized(int *);
(declaim (inline PMPI_Finalized))
(define-alien-routine ("PMPI_Finalized" PMPI_Finalized)
    int
  (flag int :out))

;; int PMPI_Free_mem(void *);
(declaim (inline PMPI_Free_mem))
(define-alien-routine ("PMPI_Free_mem" PMPI_Free_mem)
    int
  (base (* t)))

;; int PMPI_Get_address(void *, MPI_Aint *);
(declaim (inline PMPI_Get_address))
(define-alien-routine ("PMPI_Get_address" PMPI_Get_address)
    int
  (location (* t))
  (address MPI_Aint :out))

;; int PMPI_Info_create(MPI_Info *);
(declaim (inline PMPI_Info_create))
(define-alien-routine ("PMPI_Info_create" PMPI_Info_create)
    int
  (info MPI_Info :out))

;; int PMPI_Info_delete(MPI_Info, char *);
(declaim (inline PMPI_Info_delete))
(define-alien-routine ("PMPI_Info_delete" PMPI_Info_delete)
    int
  (info MPI_Info)
  (key c-string))

;; int PMPI_Info_dup(MPI_Info, MPI_Info *);
(declaim (inline PMPI_Info_dup))
(define-alien-routine ("PMPI_Info_dup" PMPI_Info_dup)
    int
  (info MPI_Info)
  (newinfo MPI_Info :out))

;; int PMPI_Info_free(MPI_Info *info);
(declaim (inline PMPI_Info_free))
(define-alien-routine ("PMPI_Info_free" PMPI_Info_free)
    int
  (info (* MPI_Info)))

;; int PMPI_Info_get(MPI_Info, char *, int, char *, int *);
(declaim (inline PMPI_Info_get))
(define-alien-routine ("PMPI_Info_get" PMPI_Info_get)
    int
  (info MPI_Info)
  (key c-string)
  (valuelen int)
  (value (* char))
  (flag int :out))

;; int PMPI_Info_get_nkeys(MPI_Info, int *);
(declaim (inline PMPI_Info_get_nkeys))
(define-alien-routine ("PMPI_Info_get_nkeys" PMPI_Info_get_nkeys)
    int
  (info MPI_Info)
  (nkeys int :out))

;; int PMPI_Info_get_nthkey(MPI_Info, int, char *);
(declaim (inline PMPI_Info_get_nthkey))
(define-alien-routine ("PMPI_Info_get_nthkey" PMPI_Info_get_nthkey)
    int
  (info MPI_Info)
  (n int)
  (key (* char)))

;; int PMPI_Info_get_valuelen(MPI_Info, char *, int *, int *);
(declaim (inline PMPI_Info_get_valuelen))
(define-alien-routine ("PMPI_Info_get_valuelen" PMPI_Info_get_valuelen)
    int
  (info MPI_Info)
  (key c-string)
  (valuelen int :out)
  (flag int :out))

;; int PMPI_Info_set(MPI_Info, char *, char *);
(declaim (inline PMPI_Info_set))
(define-alien-routine ("PMPI_Info_set" PMPI_Info_set)
    int
  (info MPI_Info)
  (key c-string)
  (value c-string))

;; int PMPI_Pack_external(char *, void *, int, MPI_Datatype, void *, MPI_Aint, 
;;                       MPI_Aint *); 
(declaim (inline PMPI_Pack_external))
(define-alien-routine ("PMPI_Pack_external" PMPI_Pack_external)
    int
  (datarep c-string)
  (inbuf (* t))
  (incount int)
  (datatype MPI_Datatype)
  (outbuf (* t))
  (outcount MPI_Aint)
  (position MPI_Aint :in-out))

;; int PMPI_Pack_external_size(char *, int, MPI_Datatype, MPI_Aint *); 
(declaim (inline PMPI_Pack_external_size))
(define-alien-routine ("PMPI_Pack_external_size" PMPI_Pack_external_size)
    int
  (datarep c-string)
  (incount int)
  (datatype MPI_Datatype)
  (size MPI_Aint :out))

;; int PMPI_Request_get_status(MPI_Request, int *, MPI_Status *);
(declaim (inline PMPI_Request_get_status))
(define-alien-routine ("PMPI_Request_get_status" PMPI_Request_get_status)
    int
  (request MPI_Request)
  (flag int :out)
  (status MPI_Status :out))

;; int PMPI_Status_c2f(MPI_Status *, MPI_Fint *);
(declaim (inline PMPI_Status_c2f))
(define-alien-routine ("PMPI_Status_c2f" PMPI_Status_c2f)
    int
  (c_status (* MPI_Status))
  (f_status MPI_Fint :out))

;; int PMPI_Status_f2c(MPI_Fint *, MPI_Status *);
(declaim (inline PMPI_Status_f2c))
(define-alien-routine ("PMPI_Status_f2c" PMPI_Status_f2c)
    int
  (f_status (* MPI_Fint))
  (c_status MPI_Status :out))

;; int PMPI_Type_create_darray(int, int, int, int [], int [], int [], int [],
;;                            int, MPI_Datatype, MPI_Datatype *);
(declaim (inline PMPI_Type_create_darray))
(define-alien-routine ("PMPI_Type_create_darray" PMPI_Type_create_darray)
    int
  (size int)
  (rank int)
  (ndims int)
  (array_of_gsizes (* int))
  (array_of_distribs (* int))
  (array_of_dargs (* int))
  (array_of_psizes (* int))
  (order int)
  (oldtype MPI_Datatype)
  (newtype MPI_Datatype :out))
		   
;; int PMPI_Type_create_hindexed(int, int [], MPI_Aint [], MPI_Datatype, 
;;                              MPI_Datatype *);
(declaim (inline PMPI_Type_create_hindexed))
(define-alien-routine ("PMPI_Type_create_hindexed" PMPI_Type_create_hindexed)
    int
  (count int)
  (blocklengths (* int))
  (displacements (* int))
  (oldtype MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int PMPI_Type_create_hvector(int, int, MPI_Aint, MPI_Datatype,
;;                             MPI_Datatype *);
(declaim (inline PMPI_Type_create_hvector))
(define-alien-routine ("PMPI_Type_create_hvector" PMPI_Type_create_hvector)
    int
  (count int)
  (blocklength int)
  (stride MPI_Aint)
  (oldtype MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int PMPI_Type_create_indexed_block(int, int, int [], MPI_Datatype, 
;;                                   MPI_Datatype *);
(declaim (inline PMPI_Type_create_indexed_block))
(define-alien-routine ("PMPI_Type_create_indexed_block"
		       PMPI_Type_create_indexed_block)
    int
  (count int)
  (blocklength int)
  (array_of_displacements (* int))
  (oldtype MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int PMPI_Type_create_resized(MPI_Datatype, MPI_Aint,
;;                             MPI_Aint, MPI_Datatype *);
(declaim (inline PMPI_Type_create_resized))
(define-alien-routine ("PMPI_Type_create_resized" PMPI_Type_create_resized)
    int
  (oldtype MPI_Datatype)
  (lb MPI_Aint)
  (extent MPI_Aint)
  (newtype MPI_Datatype :out))

;; int PMPI_Type_create_struct(int, int [], MPI_Aint [], MPI_Datatype [], 
;;                            MPI_Datatype *);
(declaim (inline PMPI_Type_create_struct))
(define-alien-routine ("PMPI_Type_create_struct" PMPI_Type_create_struct)
    int
  (count int)
  (array_of_blocklengths (* int))
  (array_of_displacements (* MPI_Aint))
  (array_of_types (* MPI_Datatype))
  (newtype MPI_Datatype :out))

;; int PMPI_Type_create_subarray(int, int [], int [], int [], int, MPI_Datatype, 
;;                              MPI_Datatype *);
(declaim (inline PMPI_Type_create_subarray))
(define-alien-routine ("PMPI_Type_create_subarray" PMPI_Type_create_subarray)
    int
  (ndims int)
  (array_of_sizes (* int))
  (array_of_subsizes (* int))
  (array_of_starts (* int))
  (order int)
  (oldtype MPI_Datatype)
  (newtype MPI_Datatype :out))

;; int PMPI_Type_get_extent(MPI_Datatype, MPI_Aint *, MPI_Aint *);
(declaim (inline PMPI_Type_get_extent))
(define-alien-routine ("PMPI_Type_get_extent" PMPI_Type_get_extent)
    int
  (datatype MPI_Datatype)
  (lb MPI_Aint :out)
  (extent MPI_Aint :out))

;; int PMPI_Type_get_true_extent(MPI_Datatype, MPI_Aint *, MPI_Aint *);
(declaim (inline PMPI_Type_get_true_extent))
(define-alien-routine ("PMPI_Type_get_true_extent" PMPI_Type_get_true_extent)
    int
  (datatype MPI_Datatype)
  (true_lb MPI_Aint :out)
  (true_extent MPI_Aint :out))

;; int PMPI_Unpack_external(char *, void *, MPI_Aint, MPI_Aint *, void *, int, 
;;                         MPI_Datatype); 
(declaim (inline PMPI_Unpack_external))
(define-alien-routine ("PMPI_Unpack_external" PMPI_Unpack_external)
    int
  (datarep c-string)
  (inbuf (* t))
  (insize MPI_Aint)
  (position MPI_Aint :in-out)
  (outbuf (* t))
  (outcount int)
  (datatype MPI_Datatype))

;; int PMPI_Win_create_errhandler(MPI_Win_errhandler_fn *, MPI_Errhandler *);
(declaim (inline PMPI_Win_create_errhandler))
(define-alien-routine ("PMPI_Win_create_errhandler" PMPI_Win_create_errhandler)
    int
  (function MPI_Win_errhandler_fn)
  (errhandler MPI_Errhandler :out))

;; int PMPI_Win_get_errhandler(MPI_Win, MPI_Errhandler *);
(declaim (inline PMPI_Win_get_errhandler))
(define-alien-routine ("PMPI_Win_get_errhandler" PMPI_Win_get_errhandler)
    int
  (win MPI_Win)
  (errhandler MPI_Errhandler :out))

;; int PMPI_Win_set_errhandler(MPI_Win, MPI_Errhandler);
(declaim (inline PMPI_Win_set_errhandler))
(define-alien-routine ("PMPI_Win_set_errhandler" PMPI_Win_set_errhandler)
    int
  (win MPI_Win)
  (errhandler MPI_Errhandler))

;; /* Fortran 90-related functions.  These routines are available only if
;;    Fortran 90 support is enabled 
;; */
;; int PMPI_Type_create_f90_integer( int, MPI_Datatype * );
(declaim (inline PMPI_Type_create_f90_integer))
(define-alien-routine ("PMPI_Type_create_f90_integer"
		       PMPI_Type_create_f90_integer)
    int
  (r int)
  (newtype MPI_Datatype :out))

;; int PMPI_Type_create_f90_real( int, int, MPI_Datatype * );
(declaim (inline PMPI_Type_create_f90_real))
(define-alien-routine ("PMPI_Type_create_f90_real" PMPI_Type_create_f90_real)
    int
  (r int)
  (p int)
  (newtype MPI_Datatype :out))

;; int PMPI_Type_create_f90_complex( int, int, MPI_Datatype * );
(declaim (inline PMPI_Type_create_f90_complex))
(define-alien-routine ("PMPI_Type_create_f90_complex" PMPI_Type_create_f90_complex)
    int
  (r int)
  (p int)
  (newtype MPI_Datatype :out))

;; #endif  /* MPI_BUILD_PROFILING */
;; /* End of MPI bindings */

;; /* feature advertisement */
(defconstant MPIIMPL_ADVERTISES_FEATURES 1)
(defconstant MPIIMPL_HAVE_MPI_INFO 1)
(defconstant MPIIMPL_HAVE_MPI_COMBINER_DARRAY 1)
(defconstant MPIIMPL_HAVE_MPI_TYPE_CREATE_DARRAY 1)
(defconstant MPIIMPL_HAVE_MPI_COMBINER_SUBARRAY 1)
(defconstant MPIIMPL_HAVE_MPI_TYPE_CREATE_DARRAY 1)
(defconstant MPIIMPL_HAVE_MPI_COMBINER_DUP 1)
(defconstant MPIIMPL_HAVE_MPI_GREQUEST 1)
(defconstant MPIIMPL_HAVE_STATUS_SET_BYTES 1)
(defconstant MPIIMPL_HAVE_STATUS_SET_INFO 1)

;; [VNP] this is included through the system definition
;; #include "mpio.h"

;; [VNP] We don't need C++ bindings in Lisp
;; #if defined(__cplusplus)
;; }
;; /* Add the C++ bindings */
;; /* 
;;    If MPICH_SKIP_MPICXX is defined, the mpicxx.h file will *not* be included.
;;    This is necessary, for example, when building the C++ interfaces.  It
;;    can also be used when you want to use a C++ compiler to compile C code,
;;    and do not want to load the C++ bindings.  These definitions can
;;    be made by the C++ compilation script
;;  */
;; #if !defined(MPICH_SKIP_MPICXX)
;; /* mpicxx.h contains the MPI C++ binding.  In the mpi.h.in file, this 
;;    include is in an autoconf variable in case the compiler is a C++ 
;;    compiler but MPI was built without the C++ bindings */
;; #include "mpicxx.h"
;; #endif 
;; #endif

;; /* BEGIN: non-standard but public extensions to MPI */

;; /* Generalized requests extensions as proposed in "Extending the MPI-2
;;  * Generalized Request Interface" */
(define-alien-type MPIX_Grequest_poll_function
    (* (function int (* t) (* MPI_Status))))

(define-alien-type MPIX_Grequest_wait_function
    (* (function int int (* (* t)) double (* MPI_Status))))

(define-alien-type MPIX_Grequest_class int)

;; int MPIX_Grequest_class_create(MPI_Grequest_query_function *, 
;;                        MPI_Grequest_free_function *, 
;;                        MPI_Grequest_cancel_function *, 
;; 		       MPIX_Grequest_poll_function *,
;; 		       MPIX_Grequest_wait_function *, 
;; 		       MPIX_Grequest_class *);
(declaim (inline MPIX_Grequest_class_create))
(define-alien-routine ("MPIX_Grequest_class_create" MPIX_Grequest_class_create)
    int
  (query_fn MPI_Grequest_query_function)
  (free_fn MPI_Grequest_free_function)
  (cancel_fn MPI_Grequest_cancel_function)
  (poll_fn MPIX_Grequest_poll_function)
  (wait_fn MPIX_Grequest_wait_function)
  (greq_class MPIX_Grequest_class :out))

;; int MPIX_Grequest_class_allocate(MPIX_Grequest_class,
;; 		       void *,
;; 		       MPI_Request *);
(declaim (inline MPIX_Grequest_class_allocate))
(define-alien-routine ("MPIX_Grequest_class_allocate"
		       MPIX_Grequest_class_allocate)
    int
  (greq_class MPIX_Grequest_class)
  (extra_state (* t))
  (request MPI_Request :out))

;; int MPIX_Grequest_start(MPI_Grequest_query_function *, 
;;                        MPI_Grequest_free_function *, 
;;                        MPI_Grequest_cancel_function *, 
;; 		       MPIX_Grequest_poll_function *,
;; 		       MPIX_Grequest_wait_function *, void *, MPI_Request *);
(declaim (inline MPIX_Grequest_start))
(define-alien-routine ("MPIX_Grequest_start" MPIX_Grequest_start)
    int
  (query_fn MPI_Grequest_query_function)
  (free_fn MPI_Grequest_free_function)
  (cancel_fn MPI_Grequest_cancel_function)
  (poll_fn MPIX_Grequest_poll_function)
  (wait_fn MPIX_Grequest_wait_function)
  (extra_state (* t))
  (request MPI_Request :out))

;; #if !defined(MPI_BUILD_PROFILING)
;; int PMPIX_Grequest_class_create(MPI_Grequest_query_function *, 
;;                        MPI_Grequest_free_function *, 
;;                        MPI_Grequest_cancel_function *, 
;; 		       MPIX_Grequest_poll_function *,
;; 		       MPIX_Grequest_wait_function *, 
;; 		       MPIX_Grequest_class *);
(declaim (inline PMPIX_Grequest_class_create))
(define-alien-routine ("PMPIX_Grequest_class_create"
		       PMPIX_Grequest_class_create)
    int
  (query_fn MPI_Grequest_query_function)
  (free_fn MPI_Grequest_free_function)
  (cancel_fn MPI_Grequest_cancel_function)
  (poll_fn MPIX_Grequest_poll_function)
  (wait_fn MPIX_Grequest_wait_function)
  (greq_class MPIX_Grequest_class :out))

;; int PMPIX_Grequest_class_allocate(MPIX_Grequest_class,
;; 		       void *,
;; 		       MPI_Request *);
(declaim (inline PMPIX_Grequest_class_allocate))
(define-alien-routine ("PMPIX_Grequest_class_allocate"
		       PMPIX_Grequest_class_allocate)
    int
  (greq_class MPIX_Grequest_class)
  (extra_state (* t))
  (request MPI_Request :out))

;; int PMPIX_Grequest_start(MPI_Grequest_query_function *, 
;;                        MPI_Grequest_free_function *, 
;;                        MPI_Grequest_cancel_function *, 
;; 		       MPIX_Grequest_poll_function *,
;; 		       MPIX_Grequest_wait_function *, void *, MPI_Request *);
(declaim (inline PMPIX_Grequest_start))
(define-alien-routine ("PMPIX_Grequest_start" PMPIX_Grequest_start)
    int
  (query_fn MPI_Grequest_query_function)
  (free_fn MPI_Grequest_free_function)
  (cancel_fn MPI_Grequest_cancel_function)
  (poll_fn MPIX_Grequest_poll_function)
  (wait_fn MPIX_Grequest_wait_function)
  (extra_state (* t))
  (request MPI_Request :out))

;; #endif
;; /* END: non-standard but public extensions to MPI */

;; #endif
