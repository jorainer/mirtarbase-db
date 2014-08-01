\name{getAvailableSpecies}
\alias{getAvailableSpecies}
\alias{getSupportTypes}
\alias{getAvailableExperiments}
\alias{getPmids}
\title{
  Retrieve various content from the miRTarBase.
}
\description{
  These functions return all various informations from the miRTarBase.
  \code{getAvailableSpecies}: Returns the species of miRNAs or
  genes from the database.

  \code{getSupportTypes}: Returns a unique list of support types
  (and evidences) for a miRNA target interaction.

  \code{getAvailableExperiments}: Returns a unique list of
  experiments that have been performed.

  \code{getPmids}: Returns the PubMed identifiers of all
  publications in the database.

}
\usage{
getAvailableSpecies( x="target_gene" )
getSupportTypes()
getAvailableExperiments( split="//" )
getPmids()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    For which the species should be returned. Either
    \dQuote{target_gene} or \dQuote{mirna}.
  }
  \item{split}{By default experiments providing evidence for a miRNA
    target gene interaction in a publication are separated by
    \dQuote{//} in the miRTarBase. The function
    \code{getAvailableExperiments} thus splits the entry in the database
    using the provided character to return a unique list of experiments.}
}
\value{
  A character vector of species names, support types, experiments or a
  numerical vector of PubMed identifiers.
}
\author{
Johannes Rainer
}
\examples{

## get available species for miRNAs
getAvailableSpecies( "mirna" )

## get available species for target genes
getAvailableSpecies( "target_gene" )

## get all support types from the database
getSupportTypes()

## get a list of experiments
Exps <- getAvailableExperiments()

## get a list of PubMed IDs
getPmids()

}
\keyword{ data }
