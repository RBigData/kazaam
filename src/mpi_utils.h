#ifndef KAZAAM_MPIUTILS_
#define KAZAAM_MPIUTILS_


static inline void R_mpi_throw_err(int check)
{
  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  if (rank == 0)
    error("MPI_Allreduce returned error code %d\n", check);
  else
    error(""); // FIXME
}


#endif
