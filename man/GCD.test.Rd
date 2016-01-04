\name{GCD.test}
\alias{GCD.test}

\title{
GCD Test
}
\description{
Performs Greatest Common Divisor (GCD) test of Marsaglia and Tsang (2002) to evaluate the randomness of an RNG. Randomness tests are conducted over two outputs of greatest common divisor operation, namely the number of required iterations and the value of greatest common divisor. The Kolmogorov-Smirnov, Anderson-Darling, Jarque-Bera, and Chi-Square tests are applied as goodness-of-fit tests when the test is conducted over the number of required iterations. The Kolmogorov-Smirnov and Chi-Square tests are applied as goodness-of-fit tests when the test is conducted over the value of greatest common divisor.
}
\usage{
GCD.test(x, B = 32, KS = TRUE, CSQ = TRUE, AD = TRUE, JB = TRUE, 
         test.k = TRUE, test.g = TRUE, mu, sd, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    an \eqn{N*2} matrix of integers that includes random data. See details for further information.
}
  \item{B}{
    the length of words (B-bit).
}
  \item{KS}{
     if \code{TRUE}, Kolmogorov-Smirnov goodness-of-fit test is applied.
}
  \item{CSQ}{
     if \code{TRUE}, Chi-Square goodness-of-fit test is applied.
}
  \item{AD}{
      if \code{TRUE}, Anderson-Darling goodness-of-fit test is applied.
}
  \item{JB}{
if \code{TRUE}, Jarque-Bera goodness-of-fit test is applied.
}
  \item{test.k}{
if \code{TRUE}, randomness test is applied over the number of required iterations of the GCD operation.
}
  \item{test.g}{
if \code{TRUE}, randomness test is applied over the value of greatest common divisor.
}
  \item{mu}{
     the mean of theoretical normal distribution that the number of required iterations follows.
}
  \item{sd}{
     the standard deviation of theoretical normal distribution that the number of required iterations follows.
}
  \item{alpha}{
     a predetermined value of significance level with the default value of 0.05.
}
}
\details{

Total number of integers to be tested is divided into two sets and entered as \code{x}. The GCD operation is applied to each row of \code{x}.

  The number of required iterations follows a normal distribution with parameters \code{mu} and \code{sd}. Values of \code{mu} and \code{sd} are obtained by Monte Carlo simulation and given by Marsaglia and Tsang (2002) for 32-bit setting. We obtained values of \code{mu} and \code{sd} for other bit settings as \code{mu=4.2503, sd=1.650673} for 8-bits, \code{mu=8.8772, sd=2.38282} for 16-bits, ...for 24-bits,...


}
\value{

\item{sig.value.k}{
   a \eqn{4 x 1} vector of p-values. Elements of \code{sig.value.k} include p-value of Kolmogorov-Smirnov and Chi-Square tests, respectively.
}
\item{sig.value.g}{
   a \eqn{2 x 1} vector of p-values. Elements of \code{sig.value.g} include p-value of Kolmogorov-Smirnov, Chi-Square, Jarque-Bera, and Anderson-Darling tests, respectively.
}
\item{KS.result.k}{
    returns 0 if H0 is rejected and 1 otherwise in Kolmogorov-Smirnov goodness-of-fit test conducted over the number of required iterations.
}
\item{CSQ.result.k}{
    returns 0 if H0 is rejected and 1 otherwise in Chi-Square goodness-of-fit test conducted over the number of required iterations.
}
\item{JB.result.k}{
    returns 0 if H0 is rejected and 1 otherwise in Jarque-Bera goodness-of-fit test conducted over the number of required iterations.
}
\item{AD.result.k}{
    returns 0 if H0 is rejected and 1 otherwise in Anderson-Darling goodness-of-fit test conducted over the number of required iterations.
}
\item{KS.result.g}{
    returns 0 if H0 is rejected and 1 otherwise in Kolmogorov-Smirnov goodness-of-fit test conducted over the value of greatest common divisor.
}
\item{CSQ.result.g}{
    returns 0 if H0 is rejected and 1 otherwise in Chi-Square goodness-of-fit test conducted over the value of greatest common divisor.
}
}
\references{
Marsaglia, G., Tsang, W.W., Some Difficult-to-pass tests of randomness. Journal of Statistical Software (2002), 7(3).
}
\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydarde@hacettepe.edu.tr>
}

\seealso{
See the function \code{\link{GCD}} that provides detailed results for the greatest common divisor operation.
}
\examples{

RNGkind(kind = "L'Ecuyer-CMRG")
B=16                 # Bit length is 16. 
k=250                # Generate 250 integers.
x=array(0,dim=c(k,2))
x[,1]=round(runif(k,0,(2^B-1)))
x[,2]=round(runif(k,0,(2^B-1)))
mu=8.8772
sd=2.38282
alpha = 0.05
test=GCD.test(x,B=B,KS=TRUE,CSQ=TRUE,AD=TRUE,JB=TRUE,
              test.k=TRUE,test.g=TRUE,mu=mu,sd=sd,alpha=alpha)
print(test)

}
\keyword{Anderson-Darling }
\keyword{Jarque-Bera }
\keyword{Kolmogorov-Smirnov }
\keyword{Chi-Square }
\keyword{nonparametric }
\keyword{goodness-of-fit test }
\keyword{randomness test }