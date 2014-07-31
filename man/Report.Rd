\name{Report}
\alias{Report}
\title{
  Create a Report object.
}
\description{
  This function creates a new \code{\link{Report-class}} object.
}
\usage{
Report( pmid=0, support_type="", experiments="" )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pmid}{the PubMed identifier of the report/paper.}
  \item{support_type}{the support type.}
  \item{experiments}{a character vector of experiments.}
}
\value{
  A Report object.
}
\seealso{
  \code{\link{MTI-class}}, \code{\link{Report-class}}
}
\author{
Johannes Rainer
}
\keyword{ data }
