\name{preparePlot}
\alias{preparePlot}
\alias{preparePlot.Core}
\title{ Prepare graphics device }
\description{
 Based on provided \code{fileName} opens and sets appropriate graphical device: pdf, postscript, 
 interactive graphical window, or (only on windows) windows metafile,.
 } 
\usage{
preparePlot(fileName="Rplot",...)
 }

\arguments{
  \item{fileName}{ Name of the file to store the output to. }
  \item{...}{ Further parameters passed to device. }
  }
\details{
   The function opens the graphical output device based on \code{fileName} extension. 
   The extensions .pdf, .ps,  or none select  \code{\link{pdf}}, \code{\link{postscript}},
    or a default (interactive) graphical device.
   On Windows also .emf extension is supported which opens \code{win.metafile}    
    and creates vector graphics in windows enhanced metafile format.      
   #endif
    Some sensible default values are passed to these devices, but further options can be
   passed via \code{...}.  
}
\value{
A plot device is opened and nothing is returned to the R interpreter.
}

\author{ Marko Robnik-Sikonja }


\seealso{
\code{\link{CORElearn}},
\code{\link{plot.ordEval}},
\code{\link{pdf}}, 
\code{\link{postscript}},
\code{\link{Devices}}    
}

\examples{
   # prepare a data set
    dat <- ordDataGen(200)
    # evaluate ordered features with ordEval
    oe <- ordEval(class ~ ., dat, ordEvalNoRandomNormalizers=200)
    # creates a separate postscript file for each attribute with given name     
    preparePlot("myGraph\%03d.ps") 
    plot(oe)
    dev.off()
}

