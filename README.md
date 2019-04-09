# kazaam

* **Version:** 0.2-0
* **URL**: https://github.com/RBigData/kazaam
* **Status:** [![Build Status](https://travis-ci.org/RBigData/kazaam.png)](https://travis-ci.org/RBigData/kazaam)
* **License:** [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause)
* **Author:** Drew Schmidt, Wei-Chen Chen, Mike Matheson, and George Ostrouchov.

Basic matrix and statistics operations for very tall, narrow distributed matrices.  For a more general distributed matrix framework, see [pbdDMAT](https://cran.r-project.org/package=pbdDMAT).



## Installation

You can install the stable version from CRAN using the usual `install.packages()`:

```r
install.packages("kazaam")
```

The development version is maintained on GitHub, and can easily be installed by any of the packages that offer installations from GitHub:

```r
remotes::install_github("RBigData/kazaam")
```

To simplify installation on cloud systems, we also have a
[Docker container](https://github.com/rbigdata/docker) available.



## Background

Our tall/skinny/distributed matrices are called `shaq`'s which stands for Super Huge Analytics done Quickly.  This of course has nothing at all to do with esteemed actor Shaquille O'Neal, who is very tall.  And since the package is so easy to use, it sometimes looks like a magic trick.  And "kazaam!" is something a magician might say.  It is by mere coincidence that Shaquille O'Neal starred in a movie titled Kazaam.

Throughout the package, we make a few key assumptions:
* The data local to each process has **the same number of columns**.  The number of rows can vary freely, or be identical across ranks.
* Codes should be **run in batch**.  Communication is handled by [pbdMPI](https://cran.r-project.org/package=pbdMPI), which (as the name suggests) uses MPI.
* Finally, **adjacent ranks in the MPI communicator** as reported by `comm.rank()` (e.g., ranks 2 and 3, 20 and 21, 1000 and 1001, ...) should store **adjacent pieces of the matrix**.

In order to get good performance, there are several other considerations:

* The number of rows `m` should be **very large**.  If you only have a few hundred thousand rows (and few columns), you're probably better off with base R matrices.
* The number of columns `n` should be **very small**.  A shaq with 10,000 colums is pushing it.
* For most operations, the local problem size should be **as big as possible** so that the local BLAS/LAPACK operations can dominate over communication.  This also keeps the total number of MPI ranks minimal, which cuts down on communication.

Because of these assumptions, we get a few distinct advantages over other, similar frameworks:
* Communication is very minimal.  Generally it amounts to a single `allreduce()` of an `n*n` matrix.  With even a few hundred MPI ranks, this is basically instantaneous.  And since most of the work is local, operations should complete very quickly.
* The total number of rows can be as large as you like, even if that's more than can fit in a signed 32-bit integer, or `2^31-1`.



## Examples and Documentation

Individual package methods are well-documented, both with example code and discussions of the total amount of communication required.

For complete example codes, see `inst/batchtests`.  These are tests that are meant to be run in batch, generally with 2 or 4 MPI ranks.  You can launch any one of them via:

```bash
mpirun -np 2 Rscript test_script.r
```

Finally, there is a comprehensive package vignette.  If you installed the package from CRAN, you can view the vignette by entering:

```r
vignette("kazaam", package="kazaam")
```

into your R session.
