\name{birthday.spacings}
\alias{birthday.spacings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Birthday Spacings Test
}
\description{
Performs Birthday Spacings test of Marsaglia and Tsang (2002) to evaluate the randomness of an RNG. The Kolmogorov-Smirnov, Anderson-Darling, and Chi-Square tests are applied as goodness-of-fit tests. 
}
\usage{
birthday.spacings(x, m = 128, n = 2^16, alpha = 0.05, lambda, num.class = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
     a vector that includes random integers. 
}
  \item{m}{
     the number of birthdays.
}
  \item{n}{
     the length of year.
}
  \item{alpha}{
     a predetermined value of significance level with the default value of 0.05.
}
  \item{lambda}{
     mean of Poisson distribution that constitutes theoretical cumulative distribution function in goodness-of-fit tests. See Details section.
}
  \item{num.class}{
     number of classes in the constructed frequency table for goodness-of-fit testing. 
}
}
\details{
This is one of the "difficult to pass tests" that RNG's that are able to pass this set of tests possibly pass most of the tests included in the Diehard Battery of Tests.

To conduct the test, \eqn{m} birthdays are randomly chosen from a year composed of \eqn{n} days. When the birthdays are sorted, asymptotic distribution of the number of duplicated values among the spacings between birthdays is Poisson with mean \eqn{\lambda=m^3/(4n)}. For most of the cases, this formula for \code{lambda} is useful. However, user should check suitability of the value entered for \code{lambda}. Note that some suitable values for \eqn{m} and \eqn{n} are given by Marsaglia and Tsang (2002).

The argument \code{num.class} should be increased along with increasing bit-length. It can be set to 5 for testing with 8-bit and to 10 for testing with 16-bit and higher.
}
\value{
\item{AD.statistic}{
     calculated value of the test statistic of Anderson-Darling goodness-of-fit test.
}
\item{AD.pvalue}{
     p-value of the test of Anderson-Darling goodness-of-fit test.
}
\item{AD.result}{
    returns 0 if H0 is rejected and 1 otherwise in Anderson-Darling goodness-of-fit test.
}
\item{KS.statistic}{
     calculated value of the test statistic of Kolmogorov-Smirnov goodness-of-fit test.
}
\item{KS.pvalue}{
     p-value of the test of Kolmogorov-Smirnov goodness-of-fit test.
}
\item{KS.result}{
    returns 0 if H0 is rejected and 1 otherwise in Kolmogorov-Smirnov goodness-of-fit test.
}
\item{CS.statistic}{
     calculated value of the test statistic of Chi-Square goodness-of-fit test.
}
\item{CS.pvalue}{
     p-value of the test of Chi-Square goodness-of-fit test.
}
\item{CS.result}{
    returns 0 if H0 is rejected and 1 otherwise in Chi-Square goodness-of-fit test.
}
}
\references{
Marsaglia, G., Tsang, W.W., Some Difficult-to-pass tests of randomness. Journal of Statistical Software (2002), 7(3).
}
\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydarde@hacettepe.edu.tr>
}

\examples{

RNGkind(kind = "L'Ecuyer-CMRG")
B=16                  # Bit length is 16.
m=32                  # Number of birthdays is 64.
n=2^B                 # Length of year is 65536.
lambda=(m^3)/(4*n)     
k=5000                # Generate 5000 integers.
x=round(runif(k,0,(2^B-1)))
alpha = 0.05
test=birthday.spacings(x, m, n, alpha, lambda, num.class=10)
print(test)

}

\keyword{Anderson-Darling }
\keyword{Kolmogorov-Smirnov }
\keyword{Chi-Square }
\keyword{nonparametric }
\keyword{goodness-of-fit test }
\keyword{randomness test }