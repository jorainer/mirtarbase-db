\name{getMirbaseFamForMature}
\alias{getMirbaseFamForMature}
\alias{mgetMirbaseFamForMature}
\title{
  Retrieves some more information for a mature miRNA ID from the mirbase database.
}
\description{
  This functions returns a \code{data.frame} with various informations
  for a given mature miRNA ID from the mirbase database (provided by the
  mirbase.db Bioconductor package).

  \code{getMirbaseFamForMature}: search for a single miRNA.

  \code{mgetMirbaseFamForMature}: search for multiple miRNAs.
}
\usage{
getMirbaseFamForMature( x, operator="=" )
mgetMirbaseFamForMature( x, operator="=", ifnotfound=NA )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    The mature miRNA ID(s).
  }
  \item{operator}{
    The operator to search for the miRNA IDs. One of \dQuote{=},
    \dQuote{!=}, \dQuote{like} and \dQuote{not like}.
  }
  \item{ifnotfound}{
    Will be returned if a miRNA is not found.
  }
}
\value{
  A \code{data.frame} or a list of \code{data.frame}s (in the case of
  \code{mgetMirbaseForMature}).
}
\details{
  The function returns the contents of the mirbase database tables
  \dQuote{mirna\_mature} and \dQuote{mirna}.
}
\note{
  These functions are only wrapper functions for
  \code{getMirbaseForMature( x, tables=c( "mirna", "mirna_prefam" ) )}.
}
\author{
Johannes Rainer
}
\seealso{
  \code{\link{getMirbaseForMature}}, \code{\link{mgetMirbaseForMature}}
}
\examples{

Res <- getMirbaseFamForMature( x="hsa-miR-221-3p" )
colnames( Res )

Res <- mgetMirbaseFamForMature( c( "hsa-miR-221-3p", "hsa-miR-221-5p" ) )
class( Res )
length( Res )

## join to a data.frame
Res <- do.call( "rbind", Res )

}
\keyword{ data }
