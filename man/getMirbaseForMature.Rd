\name{getMirbaseForMature}
\alias{getMirbaseForMature}
\alias{mgetMirbaseForMature}
\title{
  Retrieves various information for a mature miRNA ID from the mirbase database.
}
\description{
  This functions returns a \code{data.frame} with various informations
  for a given mature miRNA ID from the mirbase database (provided by the
  mirbase.db Bioconductor package).

  \code{getMirbaseForMature}: search for a single miRNA.

  \code{mgetMirbaseForMature}: search for multiple miRNAs.
}
\usage{
getMirbaseForMature( x, operator="=", tables=c( "mirna" ) )
mgetMirbaseForMature( x, operator="=", tables=c( "mirna" ), ifnotfound=NA )
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
  \item{tables}{
    Character vector of the tables from which information should be
    returned. Currently \dQuote{mirna} (informations about the
    pre-miRNA), \dQuote{mirna\_prefam} (miRNA family information),
    \dQuote{mirna\_cluster} (miRNA cluster) and
    \dQuote{mirna_chromosome_build} (chromosomal alignment of the
    pre-miRNA) are supported.
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
\author{
Johannes Rainer
}
\examples{

Res <- getMirbaseForMature( x="hsa-miR-221-3p" )
colnames( Res )

Res <- mgetMirbaseForMature( c( "hsa-miR-221-3p", "hsa-miR-221-5p" ) )
class( Res )
length( Res )

## join to a data.frame
Res <- do.call( "rbind", Res )

## the call below is identical to getMirbaseFamForMature
Res <- getMirbaseForMature( "hsa-miR-221-3p", tables=c( "mirna", "mirna_prefam" ) )

}
\keyword{ data }
