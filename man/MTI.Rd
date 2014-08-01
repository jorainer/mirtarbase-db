\name{newMTI}
\alias{newMTI}
\title{
  Create a MTI object.
}
\description{
  This function creates a new \code{\linkS4class{MTI}} object.
}
\usage{
newMTI( id="", mature_mirna="", species_mirna="", query="", target_gene="", target_gene_entrezid=0, species_target_gene="", report=list( ) )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{the identifier for the interaction.}
  \item{mature_mirna}{the identifier of the mature miRNA.}
  \item{species_mirna}{the species of the miRNA.}
  \item{query}{optional; not used.}
  \item{target_gene}{the official gene symbol of the target gene.}
  \item{target_gene_entrezid}{the NCBI Entrezgene identifier of the
    target gene.}
  \item{species_target_gene}{the species of the target gene.}
  \item{report}{a list of \code{\linkS4class{Report}} objects providing
    the evicence for the interaction.}
}
\value{
  A MTI object.
}
\note{
  The functions will return \code{NA} if the \code{id} is \code{NA} or
  the \code{pmid} is \code{NA}.
}
\seealso{
  \code{\linkS4class{MTI}}, \code{\linkS4class{Report}}
}
\author{
Johannes Rainer
}
\keyword{ data }
