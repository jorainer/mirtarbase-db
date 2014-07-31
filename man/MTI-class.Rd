\name{MTI-class}
\docType{class}
\alias{MTI-class}

\title{Class \dQuote{MTI}, miRNA target gene interaction }
\description{
  The class \dQuote{MTI} describes a miRNA target gene interaction as
  provided by the miRTarBase.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MTI", ...)} or
using the \code{ \link{MTI} } function (the preferred way).
}
\section{Slots}{
  \describe{
    \item{\code{id}}{The miRTarBase identifier of the interaction.}
    \item{\code{mature_mirna}}{The (mirbase conform) mature miRNA
      identifier (e.g. \dQuote{hsa-mir-24-3p}).}
    \item{\code{species_mirna}}{The species of the miRNA.}
    \item{\code{query}}{Not used yet.}
    \item{\code{target_gene}}{The official gene symbol of the target gene.}
    \item{\code{target_gene_entrezid}}{The NCBI Entrezgene identifier of
    the target gene.}
    \item{\code{species_target_gene}}{The species of the target gene.}
    \item{\code{report}}{A list of \code{\linkS4class{Report}}} objects
    providing the evidence for the miRNA target gene interaction.
  }
}
\section{Methods}{
  \describe{
    \item{\code{show( object )}}{Print the content of the MTI class.}
    \item{\code{id( object, ... )}}{Returns the identifier.}
    \item{\code{matureMirna( object, ... )}}{Returns the name of the mature miRNA.}
    \item{\code{mirnaSpecies( object, ... )}}{Returns the species of the
    miRNA.}
    \item{\code{targetGene( object, ... )}}{Returns the official symbol
      of the target gene.}
    \item{\code{targetGeneEntrezid( object, ... )}}{Returns the NCBI
      Entrezgene identifier of the target gene.}
    \item{\code{targetGeneSpecies( object, ... )}}{Returns the species
      of the target gene.}
    \item{\code{reports( x, ... )}}{Returns the list of
      \code{\linkS4class{Report}} objects.}
    \item{\code{pmid( object, ... )}}{Returns the PubMed identifier(s) of the
      publications providing evidence of the interaction.}
    \item{\code{experiments( object, ... )}}{Returns the experiments
      that were performed to proof the interaction.}
    \item{\code{supportedBy( object, ... )}}{Returns the support type
      of the evidence/experiments.}
    \item{\code{reportCount( object, ... )}}{Returns the number of
      publications/reports providing evidence for the interaction.}
  }
}
%\references{ ~put references to the literature/web site here ~ }
\author{Johannes Rainer}
\note{
  Instead of directly accessing the slots of the class, the getter methods listed above should be used instead.
}
\seealso{
	\code{\linkS4class{Report}}, \code{\link{getMtiForGene}}, \code{\link{getMtiForMiRNA}}
}
\examples{



}
\keyword{classes}

