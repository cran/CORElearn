\name{CoreModel}
\alias{CoreModel}
\title{Build a classification or regression model }
\description{
  Builds a classification or regression model from the \code{data} and \code{formula} with given parameters.
  Classification models available are
\itemize{
\item random forests, possibly with local weighing of basic models,
\item decision tree with constructive induction in the inner nodes and/or models in the leaves,
\item kNN and weighted kNN with Gaussian kernel,
\item naive Bayesian classifier.
}
Regression models:
\itemize{
\item regression trees with constructive induction in the inner nodes and/or models in the leaves,
\item linear models with pruning techniques,
\item locally weighted regression,
\item kNN and weighted kNN with Gaussian kernel.
}
}
\usage{
  CoreModel(formula, data,
       model=c("rf","rfNear","tree","knn","knnKernel","bayes","regTree"),
       ..., costMatrix=NULL, dataFromFiles=FALSE)
}
\arguments{
  \item{formula}{ Formula specifying the response and attribute variables. }
  \item{data}{ Data frame with training data. }
  \item{model}{ The type of model to be learned. }
  \item{... }{ Options for building the model. See \code{\link{optionCore}}. }
  \item{costMatrix}{ Optional cost matrix. }
  \item{dataFromFiles}{ Logical value controlling whether the training data will be read directly from files.}
}
\details{
  Parameter \bold{formula} is used as a mechanism to select features (attributes)
  and the prediction (response) variable (class). Only simple terms can be used.
  Interaction terms are not supported. The simplest way is
  to specify just the response variable using e.g. "class ~ .". See examples below.

  Parameter \bold{model} controls the type of the constructed model. There are several possibilities:
  \describe{
     \item{\code{"rf"}}{ random forests classifier as defined by (Breiman, 2001) with some extensions, }
     \item{\code{"rfNear"}}{ random forests classifier with basic models weighted locally (Robnik-Sikonja, 2005), }
     \item{\code{"tree"}}{ decision tree with constructive induction in the inner nodes and/or models in the leaves, }
     \item{\code{"knn"}}{ k nearest neighbors classifier, }
     \item{\code{"knnKernel"}}{ weighted k nearest neighbors classifier with distance taken into account through Gaussian kernel, }
     \item{\code{"bayes"}}{ naive Bayesian classifier, }
     \item{\code{"regTree"}}{ regression trees with constructive induction in inner nodes and/or models in leaves
     controlled by modelTypeReg parameter.
     Models used in leaves of the regression tree can also be used as stand-alone regression models 
     using option minNodeWeight=Inf (see examples below):
     \itemize{
        \item linear models with pruning techniques
        \item locally weighted regression
        \item kNN and kNN with Gaussian kernel.
      }
      }
   }

  There are many additional parameters \bold{\dots } available which are used by different models.
  Their list and description is available by calling \code{\link{optionCore}}. Evaluation of attributes is covered
  in function \code{\link{attrEval}}.

  The optional parameter \bold{ costMatrix } can provide nonuniform cost matrix for classification problems. For regression
  problem this parameter is ignored. The format of the matrix is costMatrix(true class, predicted class).
  By default uniform costs are assumed, i.e.,  costMatrix(i, i) = 0, and costMatrix(i, j) = 1, for i not equal to j.

   The optional logical parameter \bold{ dataFromFiles } can be set to \code{TRUE} in case of very large
   data sets not fitting into \R. In this case training data is read directly from files, ignoring \code{data} parameter.
   Supported data formats are C4.5 (M5) and native format of CORElearn. Its description is available from
   \url{http://lkm.fri.uni-lj.si/rmarko/software/}.
}
\value{
  The created model is not returned as a structure. It is stored internally
  in the package memory space and only its pointer (index) is returned.
  The maximum  number of models that can be stored simultaneously
  is a parameter of the initialization function \code{initCore} and
  defaults to 100. Models, which are not needed may be deleted in order
  to free the memory using function \code{destroyModels}.
  By referencing the returned model, any of the stored models may be
  used for prediction with \code{\link{predict.CoreModel}}.
  What the function actually returns is a list with components:
    \item{modelID}{ index of internally stored model, }
    \item{terms}{ description of prediction variables and response, }
    \item{class.lev}{ class values for classification problem, null for regression problem, }
    \item{model}{ the type of model used, see parameter \code{model}, }
    \item{dataFromFiles}{ was data read directly from file, see parameter \code{dataFromFiles}, }
    \item{formula}{ the \code{formula} parameter passed. }
 }
\references{
Marko Robnik-Sikonja, Igor Kononenko: Theoretical and Empirical Analysis of ReliefF and RReliefF.
    \emph{Machine Learning Journal}, 53:23-69, 2003

Leo Breiman: Random Forests. \emph{Machine Learning Journal}, 45:5-32, 2001

Marko Robnik-Sikonja: Improving Random Forests.
      In J.-F. Boulicaut et al.(Eds): \emph{ECML 2004, LNAI 3210}, Springer, Berlin, 2004, pp. 359-370

Marko Robnik-Sikonja: CORE - a system that predicts continuous variables.
    \emph{Proceedings of ERK'97} , Portoroz, Slovenia, 1997

Marko Robnik-Sikonja, Igor Kononenko: Discretization of continuous attributes using ReliefF.
 \emph{Proceedings of ERK'95}, B149-152, Ljubljana, 1995

Majority of these references are available from \url{http://lkm.fri.uni-lj.si/rmarko/papers/}
}

\author{ Marko Robnik-Sikonja, Petr Savicky }

\seealso{
\code{\link{CORElearn}},
\code{\link{predict.CoreModel}},
\code{\link{modelEval}},
\code{\link{attrEval}},
\code{\link{optionCore}},
\code{\link{paramCoreIO}}.
}

\examples{
# use iris data set

# build random forests model with certain parameters
modelRF <- CoreModel(Species ~ ., iris, model="rf",
              selectionEstimator="MDL",minNodeWeight=5,rfNoTrees=100)
print(modelRF)

# build decision tree with naive Bayes in the leaves
modelDT <- CoreModel(Species ~ ., iris, model="tree", modelType=4)
print(modelDT)

# build regression tree similar to CART
instReg <- regDataGen(200)
modelRT <- CoreModel(response~., instReg, model="regTree", modelTypeReg=1)
print(modelRT)

# build kNN kernel regressor by preventing tree splitting
modelKernel <- CoreModel(response~., instReg, model="regTree",
                    modelTypeReg=7, minNodeWeight=Inf)
print(modelKernel)

# A more complex example demonstrating also destroyModels() follows.
# Test accuracy of random forest predictor with 20 trees on iris data
# using 10-fold cross-validation.
ncases <- nrow(iris)
ind <- ceiling(10*(1:ncases)/ncases)
ind <- sample(ind,length(ind))
pred <- rep(NA,ncases)
fit <- NULL
for (i in unique(ind)) {
    # Delete the previous model, if there is one.
    if (!is.null(fit)) destroyModels(fit)
    fit <- CoreModel(Species ~ ., iris[ind!=i,], model="rf", rfNoTrees=20)
    pred[ind==i] <- predict.CoreModel(fit, iris[ind==i,], type="class")
}
table(pred,iris$Species)
}

\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ multivariate }
\keyword{ loess }
\keyword{ classif }
