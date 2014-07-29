\name{getMtiForGene}
\alias{getMtiForGene}
\title{
  Retrieve miRNA-target gene interactions (MTI) for specified gene(s)
}
\description{
  Retrieve all miRNA-target gene interactions (MTI) from the miRTarBase
  database for one or more genes.
}
\usage{
getMtiForGene( x, type="symbol", operator="like", filter.mirna.species=getAvailableSpecies( "mirna" ), filter.gene.species=getAvailableSpecies( "target_gene" ), filter.support.type=getSupportTypes(), cleanup=FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    one or more gene ids (official gene symbols or NCBI Entrezgene identifiers).
  }
  \item{ type }{
    the type of identifier that was submitted with the argument
    \code{x}. Can be either \dQuote{symbol} (official gene symbol;
    e.g. \dQuote{BCL2L11}) or \dQuote{entrezid} (NCBI Entrezgene ID;
    e.g. 1848).
  }
  \item{ operator }{
    the operator to be used in the select SQL call (either \dQuote{like}
    or \dQuote{=}).
  }
  \item{ filter.mirna.species }{
    restrict the results to MTI for miRNAs from the provided species
    (defaults to all species in the database).
  }
  \item{ filter.gene.species }{
    restrict the results to MTI for target genes from the provided
    species (defaults to all species in the database).
  }
  \item{ filter.support.type }{
    restrict the results to MTI with evidence from the provided support
    types (defaults to all available support types in the database).
  }
  \item{ cleanup }{
    whether \code{NA} rows should be removed from the results table. By
    default at least one row is returned for each mature miRNA.
  }
}
\details{
  This function retrieves MTIs from the database for one or more
  genes. Note that the operator will be changed to \code{=} for
  \code{type="entrezid"}, otherwise, with the default \code{like}
  operator a case insensitive search is performed, thus returning
  results for \dQuote{BCL2} and \dQuote{Bcl2}.
  The default operator \dQuote{like} allows also to search for MTIs
  using a pattern (\code{\%} matching zero, one or multiple characters,
  \code{_} matching one character).

}
\value{
  A data.frame with the results. The data.frame contains a row with NAs
  is no MTI is available for a miRNA (unless \code{cleanup=TRUE}).
}
\seealso{
  \code{\link{getMtiForMiRNA}}
}
\author{
Johannes Rainer
}
\examples{

## just get everything for BCL2 and BCL2L11 (from all species etc)
BCL2 <- getMtiForGene( x=c( "BCL2", "BCL2L11" ), type="symbol" )
nrow( BCL2 )
table( BCL2$support_type )
table( BCL2$species_target_gene )

## now restrict to human genes.
BCL2 <- getMtiForGene( x=c( "BCL2", "BCL2L11" ), type="symbol", filter.gene.species = "Homo sapiens" )
nrow( BCL2 )
table( BCL2$support_type )

## search using entrezid
Test <- getMtiForGene( x=1234, type="entrezid" )
nrow( Test )
Test
## ok, so we don't have that gene... if we don't want any results in such a case:
Test <- getMtiForGene( x=1234, type="entrezid", cleanup=TRUE )
nrow( Test )

## get something for real:
Test <- getMtiForGene( x=1848, type="entrezid" )
Test

}
\keyword{ data }

