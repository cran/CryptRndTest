\name{toBaseTwo}
\alias{toBaseTwo}

\title{
Convert form Base 10 to 2
}
\description{
   Converts large integers form base 10 to base 2 using mpfr numbers by Pmpfr package.
}
\usage{
toBaseTwo(x, m = 128, prec = 512, num.CPU = 4)
}

\arguments{
  \item{x}{
   an mpfr vector including the data in base 10.
}
  \item{m}{
   desired bit length in the output.
}
  \item{prec}{
   precision of the calculations.
}
  \item{num.CPU}{
   the number of CPUs that will be used in parallel computing.
}
}
\details{
  The function toBaseTwo utilizes the package parallel to make calculation utilizing parallel computing.
}
\value{
\item{r.bit}{
   a list of mpfr numbers that contains the input data in base 2 format.
}
}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydarde@hacettepe.edu.tr>
}