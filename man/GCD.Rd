\name{GCD}
\alias{GCD}

\title{
Greatest Common Divisor
}
\description{
   Finds the greatest common divisor (GCD) of two integers. In addition to the value of GCD, it generates the number of required iterations to find GCD and the sequence of partial quotients. 
}
\usage{
GCD(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   the first integer greater than zero.
}
  \item{y}{
   the second integer greater than zero.
}
}
\value{

\item{k}{
   the number of required iterations to find GCD.
}
\item{q}{
    the sequence of partial quotients.
}
\item{g}{
    the value of greatest common divisor.
}
}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydarde@hacettepe.edu.tr>
}

\examples{
result=GCD(4535,2451)
print(result)

result=GCD(35,2)
print(result)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
