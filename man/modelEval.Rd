\name{modelEval}
\alias{modelEval}

\title{ Statistical evaluation of predictions }
\description{
  Using predictions of given model produced by \code{\link{predict.CoreModel}} and correct labels,
  computes  some statistics evaluating the quality of the model.
}
\usage{
modelEval(model, correctClass, predictedClass, predictedProb=NULL, costMatrix=NULL, priorClProb = NULL, avgTrainPrediction = NULL, beta = 1) 
}

\arguments{
  \item{model}{ The model structure as returned by \code{\link{CoreModel}}, or NULL if some other predictions are evaluated. }
  \item{correctClass}{ A vector of correct class labels for classification problem and function values for regression problem. }
  \item{predictedClass}{ A vector of predicted class labels for classification problem and function values for regression problem. }
  \item{predictedProb}{ An optional matrix of predicted class probabilities for classification. }
  \item{costMatrix}{ Optional cost matrix can provide nonuniform costs for classification problems. }
  \item{priorClProb}{ If \code{model=NULL} a vector of prior class probabilities shell be provided in case of classification. }
  \item{avgTrainPrediction}{ If \code{model=NULL} mean of prediction values on training set shell be provided in case of regression. }
  \item{beta}{For two class problems \code{beta} controls the relative importance of precision and recall in F-measure.}
  }
\details{
   The function uses the \code{model} structure as returned by \code{\link{CoreModel}},
   \code{predictedClass} and (optionally) \code{predictedProb} returned by 
   \code{\link{predict.CoreModel}}. Predicted values are compared with true values 
   and some statistics are computed  measuring the quality of predictions.
   
  Optional cost matrix can provide nonuniform costs for classification problems. For regression
  problem this parameter is ignored. The costs can be different from the ones used for building the model 
   in \code{\link{CoreModel}} and predicting with the model in \code{\link{predict.CoreModel}}.
   If no costs are supplied uniform costs are assumed where necessary. 
   
   If a non CORElearn model is evaluated, one should set \code{model=NULL}, and  a vector of prior of class 
   probabilities \code{priorClProb} shell be provided in case of classification,
   and in case of regression \code{avgTrainPrediction} shell be mean of prediction values on training set.
   
}
\value{
  For classification problem function returns list with the components
  \item{accuracy}{classification accuracy}
  \item{averageCost}{average classification cost}
  \item{informationScore}{statistics measuring information in the predicted probabilities}
  \item{AUC}{Area under the ROC curve}
  \item{predictionMatrix}{matrix of miss-classifications}
  \item{sensitivity}{sensitivity for two class problems}
  \item{specificity}{specificity for two class problems}
  \item{brierScore}{Brier score of predicted probabilities}
  \item{kappa}{Kappa statistics measuring randomness of the predictions}
  \item{precision}{precision for two class problems}
  \item{recall}{recall for two class problems (the same as sensitivity) }
  \item{F-measure}{F-measure giving a weighted score of precision and recall for two class problems}
  \item{G-mean}{geometric mean of positive and negative accuracy}
 For regression problem the returned list has components
 \item{MSE}{Mean Squared Error}
 \item{RMSE}{Relative Mean Squared Error} 
 \item{MAE}{Mean Absolute Error}
 \item{RMAE}{Relative Mean Absolute Error} 
 }
  

\author{ Marko Robnik-Sikonja, Petr Savicky }

\seealso{
\code{\link{CORElearn}},
\code{\link{CoreModel}},
\code{\link{predict.CoreModel}}.
}
\references{ 
Igor Kononenko, Matjaz Kukar: \emph{Machine Learning and Data Mining: Introduction to Principles and Algorithms. }
Horwood, 2007
} 
\examples{
# use iris data

# build random forests model with certain parameters
model <- CoreModel(Species ~ ., iris, model="rf", 
              selectionEstimator="MDL",minNodeWeight=5,rfNoTrees=100)

# prediction with node distribution
pred <- predict.CoreModel(model, iris, rfPredictClass=FALSE)

# Model evaluation
mEval <- modelEval(model, iris[["Species"]], pred$class, pred$prob)
print(mEval)
}
\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ multivariate }
\keyword{ loess }
\keyword{ classif }
