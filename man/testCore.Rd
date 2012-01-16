\name{testCore}
\alias{testCore}
\alias{testCoreClass}
\alias{testCoreAttrEval}
\alias{testCoreReg}
\alias{testCoreOrdEval}
\alias{testCoreNA}
\alias{testCoreRPORT}
\alias{testCoreRand}
\alias{allTests}
\title{Verification of the CORElearn installation}
\description{
Performs a partial check of the classification part of CORElearn.
}
\usage{
testCoreClass(verbose=1)
testCoreAttrEval(continue=TRUE)
testCoreReg(verbose=1)
testCoreOrdEval(verbose=1)
testCoreNA(verbose=1)
testCoreRPORT(verbose=1)
testCoreRand(verbose=1)
allTests(verbose=1, timed=FALSE)
}

\arguments{
\item{verbose}{An integer determining the level of verbosity. If 0, an error
  is generated, if any of the tests fails. Otherwise, only the result of the
  test is printed.}
\item{continue}{ Logical. Whether the test should continue or \code{stop()}
  should be called after a difference is found.}
\item{timed}{Logical. Whether the time usage should be printed.}
}

\details{
Functions \code{testCoreClass()}, \code{testCoreAttrEval()}, \code{testCoreReg()} run
functions \code{CoreModel()}, \code{predict.CoreModel()}, \code{modelEval()}, and
\code{attrEval()} and perform a partial check of the obtained results.

Function \code{testNA()} performs a test of consistency NA and NaN between R and CORElearn.

Functions \code{testCoreRPORT()} and \code{testCoreRand()} test, whether the
\code{R_PORT} directive is defined in C code and whether R random number
generator is used. These tests are mostly for debugging.

Function \code{allTests()} calls all the above functions with \code{verbose=0}
and prints a table of the results.
}
\value{
The functions have no output value. The result OK or FAILED is printed.
}

\author{ Marko Robnik-Sikonja, Petr Savicky }

\keyword{ classif }

\seealso{
\code{\link{CORElearn}}.
}
\examples{
allTests(verbose=0) # generate an error, if any of the tests fails
}

