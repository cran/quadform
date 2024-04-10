\name{quad.form}
\alias{quad.form}
\alias{quad.form.inv}
\alias{quad.form.chol}
\alias{quad.3form}
\alias{quad.3form.inv}
\alias{quad.tform}
\alias{quad.3tform}
\alias{quad.tform.inv}
\alias{quad.diag}
\alias{quad.tdiag}
\alias{quad.3diag}
\alias{quad.3tdiag}
\alias{cprod}
\alias{tcprod}
\alias{ht}
\alias{cp}
\alias{tcp}
\alias{qf}
\alias{qfi}
\alias{q3}
\alias{q3i}
\alias{q3t}
\alias{qt}
\alias{qti}
\alias{qd}
\alias{qtd}
\alias{q3d}
\alias{q3td}

\title{Evaluate a quadratic form efficiently}
\description{\loadmathjax
  
  Given a square matrix \eqn{M} of size \eqn{n\times n}{n*n}, and a
  matrix \eqn{x} of size \eqn{n\times p}{n*p} (or a vector of length
  \eqn{n}), evaluate various quadratic forms.

  (in the following, \eqn{x^T}{t(x)} denotes the complex conjugate of
  the transpose, also known as the Hermitian transpose.  This only
  matters when considering complex numbers).
  
}
\usage{
quad.form(M, x)
quad.form.inv(M, x)
quad.form.chol(chol,x)
quad.tform(M, x)
quad.3form(M,left,right)
quad.3tform(M,left,right)
quad.tform.inv(M,x)
quad.diag(M,x)
quad.tdiag(M,x)
quad.3diag(M,left,right)
quad.3tdiag(M,left,right)
cprod(x,y)
tcprod(x,y)
ht(x)
}
\arguments{
  \item{M}{Square matrix of size \eqn{n\times n}{n*n}}
  \item{x,y}{Matrix of size \eqn{n\times p}{n*p}, or vector of length \eqn{n}}
  \item{chol}{Lower triangular Cholesky decomposition
    of the quadratic form, see details}
  \item{left,right}{In function \code{quad.3form()}, matrices with
    \eqn{n} rows and arbitrary number of columns}
}
\details{

  \itemize{

  \item Function \code{quad.form(M,x)} evaluates \eqn{x^TMx}{t(x) \%*\%
    M \%*\% x} in an efficient manner [terse form \code{qf()}]
    
    \item Function \code{quad.form.inv(M,x)} returns
      \eqn{x^TM^{-1}x}{t(x) \%*\% solve(M) \%*\% x} using an efficient
      method that avoids inverting \eqn{M} [terse form \code{qfi()}]
    
    \item Function \code{quad.tform(M,x)} returns \eqn{xMx^T}{x \%*\% A
      \%*\% t(x)} using \code{tcrossprod()} without taking
      a transpose [\code{qt()}]

    \item Function \code{quad.tform.inv(M,x)} returns \eqn{xM^{-1}x^T}{x
      \%*\% solve(M) \%*\% t(x)}, although a single transpose is needed
      [\code{qti()}]

    \item Function \code{quad.3form(M,l,r)} returns \eqn{l^TMr}{t(l)
      \%*\% M \%*\% r} using nested calls to \code{crossprod()}.  It's
    no faster than calling \code{crossprod()} directly, but makes code
    neater and less error-prone (IMHO) [\code{q3()}]

    \item Function \code{quad.3form.inv(M,l,r)} returns
      \eqn{l^TM^{-1}r}{t(l) \%*\% solve(M) \%*\% r} [\code{q3i()}]

    \item Function \code{quad.3tform(M,l,r)} returns \eqn{lMr^T}{l \%*\%
      M \%*\% t(r)} using nested calls to \code{tcrossprod()}.  Again,
    this is to make for neater code [\code{q3t()}]
    
    \item Function \code{quad.diag(M,x)} returns the \emph{diagonal} of
    the (potentially very large) square matrix \code{quad.form(M,x)}
    without calculating the off diagonal elements [\code{qd()}]
    
    \item Function \code{quad.tdiag(M,x)} similarly returns the diagonal of
    \code{quad.tform(M,x)} [\code{qtd()}]

    \item Function \code{quad.3diag(M,l,r)} returns the diagonal of
    \code{quad.3form(M,l,r)} [\code{q3d()}]

    \item Function \code{quad.3tdiag(M,l,r)} returns the diagonal of
    \code{quad.3tform(M,l,r)} [\code{q3td()}]

    \item Function \code{quad.form.chol()} interprets argument
    \code{chol} as the lower triangular Cholesky decomposition of the
    quadratic form.  Remember that \code{M.lower \%*\% M.upper == M},
    and \code{chol()} returns the upper triangular matrix, so one needs
    to use the transpose \code{t(chol(M))} in calls.
    }
    
  These functions invoke the following lower-level calls:
  
  \itemize{
    \item Function \code{ht(x)} returns the Hermitian transpose, that is,
    the complex conjugate of the transpose, sometimes written \eqn{x^*}

    \item Function \code{cprod(x,y)} returns \eqn{x^T y}{t(x) \%*\% y},
    equivalent to \code{crossprod(Conj(x),y)} [\code{cp()}]
    
    \item Function \code{tcprod(x,y)} returns \eqn{x y^T}{x \%*\% t(y)},
    equivalent to \code{crossprod(x,Conj(y))} [\code{tcp()}]
  }
   
  Note again that in the calls above, \dQuote{transpose} [that is,
  \eqn{x^T}{t(x)}] means \dQuote{Conjugate transpose}, or the Hermitian
  transpose.
  



  These various functions generally avoid taking needless expensive
  transposes in favour of using nested \code{crossprod()} and
  \code{tcrossprod()} calls.  For example, the \dQuote{meat} of
  \code{quad.form()} is just \code{crossprod(crossprod(M,Conj(x)),x)}

  Functions such as \code{quad.form.inv()} avoid taking a matrix
  inverse.  The meat of \code{quad.form.inv()}, for example, is
  \code{cprod(x, solve(M, x))}.  Many people have stated things like
  \dQuote{Never invert a matrix unless absolutely necessary}.  But I
  have \emph{never} seen a case where \code{quad.form.inv(M,x)} is
  faster than \code{quad.form(solve(M),x)}.
  
  If the Cholesky decomposition of \code{M} is available, then using
  \code{quad.form.chol()} and supplying \code{chol((M)} should generally
  be faster (for large matrices) than calling \code{quad.form()} and
  using \code{M} directly.  The time saving is negligible for matrices
  smaller than about \eqn{50\times 50}{50*50}, even if the overhead of
  computing the decomposition is ignored.

  Terse forms [\code{qf()} for \code{quad.form()}, \code{qti()} for
  \code{quad.tform.inv()}, etc] are provided for the perl golfers among
  us.

}

\value{
  Generally, return a (dropped) matrix, real or complex as appropriate
}

\author{Robin K. S. Hankin}

\note{
 These functions are used extensively in the \CRANpkg{emulator} and
  \CRANpkg{calibrator} packages, primarily in the interests of elegant
  code, but also speed.  For the problems I usually consider, the
  speedup (of \code{quad.form(M,x)} over \code{t(x) \%*\% M \%*\% x},
  say) is marginal at best.
}

\seealso{\code{\link{optimize}}}
\examples{
jj <- matrix(rnorm(80),20,4)
M <- crossprod(jj,jj)
M.lower <- t(chol(M))
x <- matrix(rnorm(8),4,2)

jj.1 <- t(x) \%*\% M \%*\% x
jj.2 <- quad.form(M,x)
jj.3 <- quad.form.chol(M.lower, x)
print(jj.1)
print(jj.2)
print(jj.3)


## Make two Hermitian positive-definite matrices:
L <- matrix(c(1,0.1i,-0.1i,1),2,2)
LL <- diag(11)
LL[2,1] <- -(LL[1,2] <- 0.1i)

z <- matrix(rnorm(22) + 1i*rnorm(22),2,11)

quad.diag(L,z)     # elements real because L is HPD
quad.tdiag(LL,z)   # ditto


## Now consider accuracy:
quad.form(solve(M),x) - quad.form.inv(M,x)  # should be zero
quad.form(M,x) - quad.tform(M,t(x))         # should be zero
quad.diag(M,x) - diag(quad.form(M,x))       # should be zero
diag(quad.form(L,z))   - quad.diag(L,z)     # should be zero
diag(quad.tform(LL,z)) - quad.tdiag(LL,z)   # should be zero
}
\keyword{array}
