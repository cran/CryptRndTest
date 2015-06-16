\name{Strlng2}
\alias{Strlng2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stirling Number of The Second Kind
}
\description{
Asymptotically computes natural logarithm of Stirling numbers of the second kind for large values of inputs by the approach of Bleick and Wang (1954) and Temme (1993). For small or moderate values of inputs, this function is not as precise as avaliable functions.
}
\usage{
Strlng2(n, k, log = TRUE)
}
\arguments{
  \item{n}{
    positive integer greater than zero.
}
  \item{k}{
   positive integer between 1 and \code{n}.
}
  \item{log}{
   if \code{TRUE}, natural logarithm of the Stirling numbers of the second kind is returned.
}
}
\details{
Due to the overflows in the calculation of large factorials, an asymptotic calculation of the Stirling numbers of the second kind is required. This function makes use of Lambert W function to calculate the Stirling numbers of the second kind with large values of \code{n} and \code{k}.
}
\value{
\item{Stirling.num }{
     the corresponding Stirling number of the second kind to the pair \code{(n,k)}.
}
}
\references{
Bleick, W.W., Wang, P.C.C., Asymptotics of Stirling Numbers of the Second Kind. Proceedings of the American Mathematical Society (1974), 42(2), 575--580.

Temme, N.M., Asymptotic estimates of Stirling numbers. Studies in Applied Mathematics (1993), 89, 233--243.
}
\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydarde@hacettepe.edu.tr>
}

\seealso{
See also \code{Stirling} function from the package \pkg{copula}.
}
\examples{
# When n = 10 and k = 4, exact value is 34105
copula::Stirling2(10,4)           # Works well.
Strlng2(10,4,log=FALSE)
# ---- Moderate values of n and k ----
# When n = 30 and k = 20, exact value is 581535955088511150
log(581535955088511150)-log(copula::Stirling2(30,20))
log(581535955088511150)-Strlng2(30,20,log=TRUE)  # Works well.
# ---- Large values of n and k ----
copula::Stirling2(500,40)         
Strlng2(500,40,log=TRUE)  # Works better.
}

\keyword{ ~approximation }
\keyword{ ~large factorials }
