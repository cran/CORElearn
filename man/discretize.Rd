\name{discretize}
\alias{discretize}
\alias{applyDiscretization}

\title{ Discretization of numeric attributes }
\description{
  The method returns discretization bounds for numeric attributes, obtained with greedy search using given feature evaluation heuristics.
  The attributes and target variable are specified using formula interface, target variable name or index.
   Feature evaluation algorithms available for classification problems 
   are various variants of Relief and ReliefF algorithms, gain ratio, gini-index, MDL, DKM, information gain, etc.
   For regression problems there are RREliefF, MSEofMean, MSEofModel, MAEofMode, etc.
}
\usage{
 discretize(formula, data, estimator, discretizationLookahead=3,
            discretizationSample=0, maxBins=0, ...)
 
 applyDiscretization(data, boundsList)
            
}
\arguments{
  \item{formula}{ Either a formula specifying the attributes to be evaluated and the target variable, or a name of target variable, or an index of target variable. }
  \item{data}{ Data frame with data. }
  \item{estimator}{ The name of the evaluation method.}
  \item{discretizationLookahead}{Discretization is performed with a greedy algorithm which adds a new boundary, until there is no
     improvement in evaluation function for \code{discretizationLookahead} number of times
     (0=try all possibilities). Candidate boundaries are chosen from a random sample of boundaries,
     whose size is \code{discretizationSample}.}
  \item{discretizationSample}{Maximal number of points to try discretization (0=all sensible). Binarization of multivalued discrete features with
     \eqn{k} values is performed exhaustively, if \eqn{2^k - 1} is at most \code{discretizationSample}. Otherwise binarization
     is done greedily starting from the best separation of a single value.
     For ReliefF-type measures, binarization of numeric features is performed with \code{discretizationSample} randomly
     chosen splits. For other measures, the split is searched exhaustively among all possible splits.}
  \item{maxBins}{The maximal number of discrete bins for numeric attributes (0=don't care). This shall be an integer vector of length 
    equal to the number of numeric attributes or an integer which applies to all numeric attributes. The default value of
    0 means that the number of bins will be determined greedily taking into account \code{discretizationLookahead}.}.
  \item{\dots }{ Additional options used by specific evaluation methods as described in \code{\link{helpCore}}.}
  \item{boundsList}{A list of numeric bounds which is applied to numeric attributes in \code{data} to produce
    discrete attributes of type \code{factor}. Numeric bounds can be obtained by calling \code{discretize} function.}.
}
\details{
  The parameter \code{formula} can be interpreted in three ways, where the formula interface is the most elegant one, 
  but inefficient and inappropriate for large data sets. See \code{\link{CoreModel}} for details.
  
  The \bold{estimator} parameter selects the evaluation heuristics.  For classification problem it 
  must be one of the names returned by \code{infoCore(what="attrEval")} and for 
  regression problem it must be one of the names returned by \code{infoCore(what="attrEvalReg")}.
  For details see their description in \code{\link{attrEval}}.
  
  If the number of supplied vector in \code{maxBins} is shorter than the number of numeric attributes, the
  vector is coerced to the required length.
 
  There are some additional parameters \bold{\dots } available which are used by specific evaluation heuristics.
  Their list and short description is available by calling \code{\link{helpCore}}. See Section on attribute evaluation.
}
\value{
  The method returns a list of discretization bounds for numeric attributes. One component of a list contains bounds for one attribute.
  If an attribute has all values equal, value NA is returned. If an attribute has all values equal to NA, it is skipped in the returned list. 
  }
\examples{
# use iris data

# run method using estimator ReliefF with exponential rank distance  
discBounds <- discretize(Species ~ ., iris, estimator="ReliefFexpRank")
print(discBounds)
discreteIris <- applyDiscretization(iris, discBounds)

regData <- regDataGen(200)
discretize(response ~ ., regData, estimator="RReliefFequalK", maxBins=2)
 
# print all available estimators
#infoCore(what="attrEval")
#infoCore(what="attrEvalReg")
}
\references{ 
Marko Robnik-Sikonja, Igor Kononenko: Theoretical and Empirical Analysis of ReliefF and RReliefF.
    \emph{Machine Learning Journal}, 53:23-69, 2003

Marko Robnik-Sikonja, Igor Kononenko: Discretization of continuous attributes using ReliefF. \emph{Proceedings of ERK'95} , Portoroz, Slovenia, 1995.
 
Some of these references are available also from \url{http://lkm.fri.uni-lj.si/rmarko/papers/}
 }
 
\author{ Marko Robnik-Sikonja }


\keyword{ regression }
\keyword{ nonlinear }
\keyword{ classif }

\seealso{
\code{\link{CORElearn}},
\code{\link{CoreModel}},
\code{\link{attrEval}},
\code{\link{helpCore}}, 
\code{\link{infoCore}}.
}

