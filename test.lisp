(require :asdf)
(require :sb-mpich2)  ;; calls MPI_Init implicitly
(use-package :sb-mpich2)

(format t "Hello from node ~a~%" *mpi-world-rank*)
(with-mpi-call ((MPI_Finalize)))
