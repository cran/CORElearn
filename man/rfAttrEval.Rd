\name{rfAttrEval}
\alias{rfAttrEval}
\title{ Attribute evaluation with random forest }
\description{
  The method evaluates the quality of the features/attributes/dependent variables
  used in the given random forest \code{model}.
  
}
\usage{
rfAttrEval(model) 
 }

\arguments{
  \item{model}{ The model of type \code{rf} or \code{rfNear} as returned by \code{\link{CoreModel}}. }
  }
\details{
  The attributes are evaluated via provided random forest's out-of-bag sets. Values for each attribute in turn
  are randomly shuffled and classified with random forest. The difference between average margin of
  non-shuffled and shuffled instances serves as a quality estimate of the attribute.   
}
\value{
  Vector of evaluations for the features in the order specified by the formula used to generate provided \code{model}.
}

\author{ Marko Robnik-Sikonja }

\keyword{ models }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ classif }

\seealso{
\code{\link{CORElearn}},
\code{\link{CoreModel}}.
\code{\link{attrEval}}.
}

\examples{
# use iris data set

# build random forests model with certain parameters
modelRF <- CoreModel(Species ~ ., iris, model="rf", 
              selectionEstimator="MDL",minNodeWeight=5,rfNoTrees=100)

estRF <- rfAttrEval(modelRF) 

print(estRF)

}
\references{ 
Marko Robnik-Sikonja: Improving Random Forests. In J.-F. Boulicaut et al.(Eds): ECML 2004, 
LNAI 3210, Springer, Berlin, 2004, pp. 359-370
Available also from \url{http://lkm.fri.uni-lj.si/rmarko/papers/}
   
Leo Breiman: Random Forests. Machine Learning Journal, 2001, 45, 5-32

 }
