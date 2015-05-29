\name{calibrate}
\alias{calibrate}

\title{ Calibration of  probabilities according to the given prior.}
\description{
  Given probability scores \code{predictedProb} as provided for example by a call to \code{\link{predict.CoreModel}} 
  and using one of available methods given by \code{methods} the function calibrates predicted probabilities  so that they 
  match the actual probabilities of a binary class 1 provided by \code{correctClass}. 
  }
\usage{

calibrate(correctClass, predictedProb, class1=1, 
          method = c("isoReg","binIsoReg","binning","mdlMerge"), 
          weight=NULL, noBins=10, assumeProbabilities=FALSE)
}

\arguments{
  \item{correctClass}{ A vector of correct class labels for a binary classification problem. }
  \item{predictedProb}{ A vector of predicted class 1 (probability) scores of the same length as \code{correctClass}. }
  \item{class1}{A class value (factor) or an index of the class value to be taken as a class to be calibrated.}
  \item{method}{ One of \code{isoReg}, \code{binIsoReg}, \code{binning}, or \code{mdlMerge}. See details below.}
  \item{weight}{ If specified, should be of the same length as \code{correctClass} and gives the weights for all the instances, 
                 otherwise a default weight of 1 for each instance is assumed. }
  \item{noBins}{The value of parameter depends on the parameter \code{method} and specifies desired or initial number of bins. See details below.}
  \item{assumeProbabilities}{ If \code{assumeProbabilities=TRUE} the values in \code{predictedProb} are expected to be in [0,1] range i.e., probability estimates. 
                           \code{assumeProbabilities=FALSE} the algorithm can be used as ordinary (isotonic) regression    }  
  }
\details{
   Depending on the specified \code{method} one of the following calibration methods is executed.
   \itemize{
\item \code{"isoReg"} isotonic regression calibration based on pair-adjacent violators (PAV) algorithm.
\item \code{"binning"} calibration into a pre-specified number of bands given by \code{noBins} parameter, trying to make bins of equal weight.
\item \code{"binIsoReg"} first binning method is executed, following by a isotonic regression calibration.
\item \code{"mdlMerge"} first intervals are merged by a MDL gain criterion into a prespecified number of intervals, following by the isotonic regression calibration.
}  
If \code{model="binning"} the parameter \code{noBins} specifies the desired number of bins i.e., calibration bands;
      if \code{model="binIsoReg"} the parameter \code{noBins} specifies the number of initial bins that are formed by binning before isotonic regression is applied;
      if \code{model="mdlMerge"}  the parameter \code{noBins} specifies the number of bins formed after first applying isotonic regression. The most similar bins are merged using MDL criterion.
}
\value{
  A function returns a list with two vector components of the same length:
  \item{interval}{The boundaries of the intervals. Lower boundary 0 is not explicitly included but should be taken into account.}
  \item{calProb}{The calibrated probabilities for each corresponding interval.}
  }
  

\author{ Marko Robnik-Sikonja}

\seealso{
\code{\link{CORElearn}},
\code{\link{predict.CoreModel}}.
}
\references{ 
I. Kononenko, M. Kukar: \emph{Machine Learning and Data Mining: Introduction to Principles and Algorithms. } Horwood, 2007

A. Niculescu-Mizil, R. Caruana: Predicting Good Probabilities With Supervised Learning. \emph{Proceedings of the 22nd International Conference on Machine Learning (ICML'05)}, 2005

} 
\examples{
# generate data
train <-classDataGen(noInst=200)
cal <-classDataGen(noInst=200)

# build random forests model with certain parameters
modelRF <- CoreModel(class~., train, model="rf", selectionEstimator="MDL",
                     minNodeWeightRF=5, rfNoTrees=100, maxThreads=1)
# prediction 
pred <- predict(modelRF, cal, rfPredictClass=FALSE)
destroyModels(modelRF) # clean up

# calibrate for a chosen class1 and method
class1<-1
calibrate(cal$class, pred$prob[,class1], class1=1, method="binning", noBins=5)
}
\keyword{ models }
\keyword{ classif }
