\name{getMtiForMiRNA}
\alias{getMtiForMiRNA}
\title{
  Retrieve miRNA-target gene interactions (MTI) for specified miRNA(s)
}
\description{
  Retrieve all miRNA-target gene interactions (MTI) from the miRTarBase
  database for one or more miRNAs.
}
\usage{
getMtiForMiRNA( x, type="mature_mirna", operator="like", filter.mirna.species=getAvailableSpecies( "mirna" ), filter.gene.species=getAvailableSpecies( "target_gene" ), filter.support.type=getSupportTypes(), return.data.frame=FALSE )

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
  \item{ return.data.frame }{
    If \code{TRUE} returns a data.frame instead of a list of MTI
    objects. The default should be used (\code{return.data.frame=FALSE})
    whenever possible (see notes below).
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
  The default operator \dQuote{like} allows also to search for MTIs
  using a pattern (\code{\%} matching zero, one or multiple characters,
  \code{_} matching one character). This works however only for
  \code{type="mature_mirna"}!

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
  \code{\link{getMtiForGene}}, \code{\linkS4class{MTI}}
}
\author{
  Johannes Rainer
}
\examples{

## get MTIs for the (mature miRNA) hsa-mir-29c-5p and restrict to human target genes.
MTIs <- getMtiForMiRNA( "hsa-mir-29c-5p", filter.gene.species="Homo sapiens" )
length( MTIs )
MTIs

## get MTIs for the (pre) miRNA hsa-mir-29b-2 (i.e. for both 3p and 5p sequences)
MTIs <- getMtiForMiRNA( "hsa-mir-29b-2", type="mirna_id", filter.gene.species="Homo sapiens" )
length( MTIs )
unique( lapply( MTIs, matureMirna ) )

## get MTIs for all miRNAs of the miRNA family mir-29 (we're also restricting to human miRNAs)
MTIs <- getMtiForMiRNA( "mir-29", type="mirna_family", filter.gene.species="Homo sapiens", filter.mirna.species="Homo sapiens" )
length( MTIs )
unique( lapply( MTIs, matureMirna ) )

}
\keyword{ data }

