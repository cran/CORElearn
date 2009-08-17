\name{testCore}
\alias{testCore}
\alias{testNA}
\title{Verification of the CORElearn installation}
\description{
Performs a partial check of the classification part of CORElearn.
}
\usage{
testCore()
testNA()
}

\arguments{None.}

\details{
Function \code{testCore()} runs functions CoreModel, predict.CoreModel, modelEval, and attrEval and
performs a partial check of the obtained results.

Function \code{testNA()} performs a test of consistency NA and NaN between R and CORElearn.
}
\value{
Function \code{testCore()} has no output value. The result OK or FAILED is printed.

Function \code{testNA()} returns a logical value indicating, whether the result of the
test is correct.
}

\author{ Marko Robnik-Sikonja, Petr Savicky }

\keyword{ classif }

\seealso{
\code{\link{CORElearn}}.
}
\examples{
testCore()
}

