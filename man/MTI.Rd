\name{MTI}
\alias{MTI}
\title{
  Create a MTI object.
}
\description{
  This function creates a new \code{\link{MTI-class}} object.
}
\usage{
MTI( id="", mature_mirna="", species_mirna="", query="", target_gene="", target_gene_entrezid=0, species_target_gene="", report=list( ) )
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
  \item{report}{a list of \code{\link{Report-class}} objects providing
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
  \code{\link{MTI-class}}, \code{\link{Report-class}}
}
\author{
Johannes Rainer
}
\keyword{ data }
