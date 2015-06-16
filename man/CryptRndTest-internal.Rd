\name{CryptRndTest-internal}

\alias{KSADdga}
\alias{KSADCHRY}
\alias{Random.walk.Y}
\alias{Random.walk.D}
\alias{Random.walk.G}
\alias{dogumGunuAraliklari}

\title{
Functions for internal use only
}
\description{
Contains functions desinged for internal use only. Functions including "default" in name makes basic checks using \code{check} function, and then call main function. Those including "main" in name carries on main processes to generate random tables. \code{rtableICC.RxC.engine} is the function multiply called by \code{rTable.RxC.main}.
}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydarde@hacettepe.edu.tr>
}

\seealso{
\code{\link[CryptRndTest]{adaptive.chi.square}}, \code{\link[CryptRndTest]{birthday.spacings}}, \code{\link[CryptRndTest]{book.stack}}, \code{\link[CryptRndTest]{GCD.test}}, , \code{\link[CryptRndTest]{GCD}}, \code{\link[CryptRndTest]{random.walk.tests}}, , \code{\link[CryptRndTest]{topological.binary}}, \code{\link[CryptRndTest]{Strlng2}}, \code{\link[copula]{Stirling2}}
}
