\name{Report-class}
\docType{class}
\alias{Report-class}
\alias{show,Report-method}
\alias{pmid,Report-method}
\alias{experiments,Report-method}
\alias{supportedBy,Report-method}
\alias{pmid}
\alias{experiments}
\alias{supportedBy}
\title{Class \dQuote{Report}, report and evidence for a miRNA target gene interaction }
\description{
  The class \dQuote{Report} provides the evidence for a miRNA target gene interaction as
  defined by the miRTarBase.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Report", ...)} or
using the \code{ \link{newReport} } function (the preferred way).
}
\section{Slots}{
  \describe{
    \item{\code{pmid}}{The PubMed identifier of the publication in which
      the miRNA target gene interaction has been described.}
    \item{\code{experiments}}{A character vector of experiments that
      have been performed in the publication to verify the MTI.}
    \item{\code{support_type}}{The evidence grade of the interaction.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{show( object )}}{Print the content of the Report class.}
    \item{\code{pmid( object, ... )}}{Returns the PubMed identifier(s) of the
      publications providing evidence of the interaction.}
    \item{\code{experiments( object, ... )}}{Returns the experiments
      that were performed to proof the interaction.}
    \item{\code{supportedBy( object, ... )}}{Returns the support type
      of the evidence/experiments.}
  }
}
%\references{ ~put references to the literature/web site here ~ }
\author{Johannes Rainer}
\note{
  Instead of directly accessing the slots of the class, the getter methods listed above should be used instead.
}
\seealso{
	\code{\linkS4class{MTI}}, \code{\link{getMti}}
}
\examples{

## get a MTI from the database.
MTI <- getMti( "MIRT006481", type="mirtarbase_id" )[[1]]

## get the Reports
Reps <- reports( MTI )
Reps

}
\keyword{classes}

