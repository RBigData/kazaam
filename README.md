# pbdSHAQ

* **Version:** 0.1-0
* **URL**: https://github.com/RBigData/pbdSHAQ
* **License:** [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause)
* **Author:** Drew Schmidt, Wei-Chen Chen, Mike Matheson, and George Ostrouchov.


Basic matrix operations for very tall, narrow distributed matrices.  For a more general distributed matrix framework, see [pbdDMAT](https://cran.r-project.org/package=pbdDMAT)


Our tall/skinny/distributed matrices are called `shaq`'s, after Shaquille O'Neal (who is very tall).  Throughout the package, we make a few key assumptions:
* The number of rows `m` should be **very large**.  If you only have a few hundred thousand rows (and few columns), you're probably better off with base R matrices.
* The number of columns `n` should be **very small**.  I have not benchmarked this yet, but I suspect that 1000 would seriously be pushing it.
* The local problem size should be **as big as possible** so that the local BLAS/LAPACK operations can dominate.  This also keeps the total number of MPI ranks minimal, which cuts down on communication.
* Codes should be **run in batch**.  Communication is handled by [pbdMPI](https://cran.r-project.org/package=pbdMPI), which (as the name suggests) uses MPI.

At this time, the package is very young and barely documented.  See `inst/batchtests` for some example codes.
