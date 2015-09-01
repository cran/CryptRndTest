\name{GCD.big}
\alias{GCD.big}

\title{
Greatest Common Divisor for Large Integers
}
\description{
   Finds the greatest common divisor (GCD) of two large integers. It utilizes multiple precision floating point numbers along with the package Rmpfr. In addition to the value of GCD, it generates the number of required iterations to find GCD and the sequence of partial quotients. 
}
\usage{
GCD.big(x, y, B)
}
\arguments{
  \item{x}{
   the first integer greater than zero.
}
  \item{y}{
   the second integer greater than zero.
}
\item{B}{
   default precision in bits.
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
result=GCD.big(14532710900972355716,4463510164971546043,64)
print(result)
}


