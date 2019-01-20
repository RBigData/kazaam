#ifndef KAZAAM_MPIUTILS_
#define KAZAAM_MPIUTILS_


static inline MPI_Comm* get_mpi_comm_from_Robj(SEXP comm_)
{
  MPI_Comm *comm = (MPI_Comm*) R_ExternalPtrAddr(comm_);
  return comm;
}

static inline void R_mpi_throw_err(int check, MPI_Comm *comm)
{
  int rank;
  MPI_Comm_rank(*comm, &rank);
  if (rank == 0)
    error("MPI_Allreduce returned error code %d\n", check);
  else
    error(""); // FIXME
}


#endif
