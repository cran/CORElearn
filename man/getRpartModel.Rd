\name{getRpartModel}
\alias{getRpartModel}
\title{ Conversion of a CoreModel tree into an rpart.object }
\description{ The function converts given CoreModel model (decision or regression tree) 
into an rpart.object prepared for visualization with  \code{"\link[=plot.rpart]{plot}"}
and \code{\link{text.rpart}} functions. }
\usage{
  getRpartModel(model, dataset)
}
\arguments{
  \item{model}{ A tree model produced by \code{\link{CoreModel}} }
  \item{dataset}{ A data set which was used in learning of the \code{model}.}
}
\value{ Function returns a \code{\link{rpart.object}}.}
\author{ John Adeyanju Alao (as a part of his BSc thesis) and Marko Robnik-Sikonja (thesis supervisor)}

\examples{
# plotting a decision tree directly
dataset <- CO2
md<-CoreModel(Plant ~ ., dataset, model="tree")
plot(md, dataset)

# or indirectly 
rpm <- getRpartModel(md, dataset)
# set angle to tan(0.5)=45 (degrees) and length of branches at least 5 
plot(rpm, branch=0.5, minbranch=5, compress=TRUE)
#(pretty=0) full names of attributes, numbers to 3 decimals, try to make a dendrogram more compact
text(rpm, pretty=0, digits=3)
}

\seealso{\code{\link{CoreModel}}, \code{\link{plot.CoreModel}},\code{\link{rpart}}, \code{\link{rpart.object}},
\code{\link{plot.rpart}}
}

\keyword{tree}
