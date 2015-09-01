\name{GCD.q}
\alias{GCD.q}

\title{
Greatest Common Divisor
}
\description{
   Finds the greatest common divisor (GCD) of two integers using the Euclidean algorithm. In addition to the value of GCD, it generates the number of required iterations to find GCD and the sequence of partial quotients. 
}
\usage{
GCD.q(x, y)
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
result=GCD.q(4535,2451)
print(result)

result=GCD.q(35,2)
print(result)
}


