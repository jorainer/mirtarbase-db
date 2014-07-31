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
getMtiForGene( x, type="symbol", operator="like", filter.mirna.species=getAvailableSpecies( "mirna" ), filter.gene.species=getAvailableSpecies( "target_gene" ), filter.support.type=getSupportTypes(), return.data.frame=FALSE )
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
  \item{ return.data.frame }{
    If \code{TRUE} returns a data.frame instead of a list of MTI
    objects. The default should be used (\code{return.data.frame=FALSE})
    whenever possible (see notes below).
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
  A list of \code{\linkS4class{MTI}} objects (containing also the
  reports where the interaction was described).
}
\note{
  Using \code{return.data.frame=TRUE} it is possible to return the
  results as a data.frame. This data.frame will contain at least one row
  per query gene, with \code{NA} entries if nothing was found for a
  gene. Also, the data.frame will contain the same interaction between a
  miRNA and a target gene multiple times, if the interaction was
  described in more than one publication. Thus it is advised to use the
  default (\code{return.data.frame=FALSE}) which ensures that each MTI
  is returned only once (with eventually more than one reports providing
  evidence).

  In the default case, however, the function will not return anything
  (i.e. \code{NULL}) if no interaction was found.
}
\seealso{
  \code{\link{getMtiForMiRNA}}, \code{\linkS4class{MTI}}
}
\author{
Johannes Rainer
}
\examples{

## just get everything for BCL2 and BCL2L11 (from all species etc)
BCL2 <- getMtiForGene( x=c( "BCL2", "BCL2L11" ), type="symbol" )
length( BCL2 )

BCL2[1:2]

## now restrict to human genes.
BCL2 <- getMtiForGene( x=c( "BCL2", "BCL2L11" ), type="symbol", filter.gene.species = "Homo sapiens" )
length( BCL2 )

## search using entrezid
Test <- getMtiForGene( x=1234, type="entrezid" )
length( Test )
Test

## get something for real:
Test <- getMtiForGene( x=1848, type="entrezid" )
Test

}
\keyword{ data }

