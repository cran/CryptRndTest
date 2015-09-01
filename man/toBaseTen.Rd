\name{toBaseTen}
\alias{toBaseTen}

\title{
Convert form Base 2 to 10
}
\description{
   Converts large integers form base 2 to base 10 using mpfr numbers by Pmpfr package.
}
\usage{
toBaseTen(x, m = 128, prec = 256, toFile = FALSE, file)
}

\arguments{
  \item{x}{
   an m-by-k binary matrix including the data in base 2.
}
  \item{m}{
   desired bit length in the output.
}
  \item{prec}{
   precision of the calculations.
}
  \item{toFile}{
   if \code{TRUE}, the resulting numbers are written on a file.
}
  \item{file}{
   the path of the file to which the output is written. 
}
}
\value{
\item{dat}{
   an m-by-k matrix that contains the input data in base 10 format.
}
}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydarde@hacettepe.edu.tr>
}



