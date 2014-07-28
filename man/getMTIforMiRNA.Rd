\name{getMTIforMiRNA}
\alias{getMTIforMiRNA}
\title{
  Retrieve miRNA-target gene interactions (MTI) for specified miRNA(s)
}
\description{
  Retrieve all miRNA-target gene interactions (MTI) from the miRTarBase
  database for one or more miRNAs.
}
\usage{
getMTIforMiRNA( x, type="mature_mirna", operator="like", filter.mirna.species=getAvailableSpecies( "mirna" ), filter.gene.species=getAvailableSpecies( "target_gene" ), filter.support.type=getSupportTypes(), cleanup=FALSE )

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    one or more mature miRNA ids, miRNA ids or miRNA family ids.
  }
  \item{ type }{
    the type of identifier that was submitted with the argument
    \code{x}. Can be either \dQuote{mature_mirna}, \dQuote{mirna_id} or
    \dQuote{mirna_family}. See details section for (well...) details.
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
  This function retrieves MTIs from the database for one or more mature
  miRNAs. By default (\code{type="mature_mirna"}) the function expects the user to submit the mature
  miRNA IDs (e.g. hsa-mir-29b-5p). Alternatively, it is possible to
  submit pre-miRNA IDs (e.g. hsa-mir-29b-1; \code{type="mirna_id"}) or
  miRNA family IDs (e.g. mir-29; \code{type="mirna_family"}). In these
  cases, the function internally retrieves all mature miRNAs for the
  submitted IDs from the mirbase database.
}
\value{
  A data.frame with the results. The data.frame contains a row with NAs
  is no MTI is available for a miRNA (unless \code{cleanup=TRUE}).
}
\author{
Johannes Rainer
}
\keyword{ data }

