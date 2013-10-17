\name{saveRF}
\alias{saveRF}
\alias{loadRF}
\title{ Saves/loads random forests model to/from file }
\description{
  \code{saveRF}: the internal structure of given random forests model is saved to file.
  \code{loadRF}: the internal structure of random forests model is loaded from given file and a model is created and returned.
  }
\usage{
saveRF(model, fileName) 
loadRF(formula, data, fileName) 
 }

\arguments{
  \item{model}{ The model structure as returned by \code{\link{CoreModel}}. }
  \item{fileName}{ Name of the file to save/load the model to/from. }
  \item{formula}{ Formula shell match the model loaded from file. }
  \item{data}{ Data shell match the formula and the model loaded from file. }
  }
\details{
   The function \code{saveRF} saves the internal structure of given random forests model to specified file.
   The \code{model} must be a valid structure returned by \code{\link{CoreModel}}.
   The function \code{loadRF} loads the internal structure of random forests saved in a specified file and
   returns access to it in the \code{model}. The parameters \code{formula} and \code{data} have to match
   the loaded model, and are needed for subsequent predictions with the loaded model.   
}
\value{
  \code{saveRF} does not return any value, while \code{loadRF}
     returns a loaded model as a list, similarly to \code{\link{CoreModel}}.
}

\author{ Marko Robnik-Sikonja, Petr Savicky }

\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ classif }

\seealso{
\code{\link{CORElearn}},
\code{\link{CoreModel}}.
}

\examples{
# use iris data set

# build random forests model with certain parameters
modelRF <- CoreModel(Species ~ ., iris, model="rf", 
              selectionEstimator="MDL",minNodeWeightRF=5,rfNoTrees=100)
print(modelRF)

# prediction with node distribution
pred <- predict(modelRF, iris, rfPredictClass=FALSE, type="both")
print(pred)


# saves the random forests model to file
saveRF(modelRF, "tempRF.txt")

# restore the model to another model
loadedRF = loadRF(Species ~ ., iris, "tempRF.txt")

# prediction should be the same
predLoaded <- predict(loadedRF, iris, rfPredictClass=FALSE, type="both")
print(predLoaded)



}

