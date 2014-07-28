\name{getAvailableSpecies}
\alias{getAvailableSpecies}
\alias{getSupportTypes}
\title{
  List all available species or MTI support types from the database
}
\description{
These functions return all available species for which a miRNA-target
gene interaction is stored in the database or all support types for such MTIs.
}
\usage{
getAvailableSpecies( x="target_gene" )
getSupportTypes()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    For which the species should be returned. Either
    \dQuote{target_gene} or \dQuote{mirna}.
  }
}
\value{
  A character vector of species names or support types.
}
\author{
Johannes Rainer
}
\keyword{ data }
