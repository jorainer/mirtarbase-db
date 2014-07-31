\name{data.frame2mti}
\alias{data.frame2mti}
\alias{mti2data.frame}
\title{
  Convert data.frames to MTI classes and \emph{vice versa}
}
\description{
  These function allow to create MTI (miRNA target gene interaction)
  objects from data.frames) or to export the contents of a MTI object
  (along with the stored Report object(s)) as a (single or multi-row) data.frame.
}
\usage{
data.frame2mti( x, colname.id="mirtarbase_id", colname.mirna="mirna", colname.mirna.species="species_mirna", colname.target="target_gene", colname.target.entrezid="target_gene_entrez_gene_id", colname.target.species="species_target_gene", do.unique=TRUE )

mti2data.frame( x, colname.id="mirtarbase_id", colname.mirna="mirna", colname.mirna.species="species_mirna", colname.target="target_gene", colname.target.entrezid="target_gene_entrez_gene_id", colname.target.species="species_target_gene", collapse.reports=NULL, stringsAsFactors=FALSE, row.names=NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  }
  \item{colname.id}{the column name in the data.frame containing the
    miRTarBase identifiers.}
  \item{colname.mirna}{the column name in the data.frame containing the
    mature miRNA name.}
  \item{colname.mirna.species}{the column name in the data.frame
    containing the species name of the miRNA.}
  \item{colname.target}{the column name in the data.frame containing the
  target gene's official symbol.}
  \item{colname.target.entrezid}{the column name in the data.frame
    containing the target gene's NCBI Entrezgene identifier.}
  \item{colname.target.species}{the column name in the data.frame
    containing the species of the target gene.}
  \item{do.unique}{whether a unique list of MTI objects should be
    generated from the data.frame.}
  \item{collapse.reports}{if not NULL multiple reports providing evidence
    for the MTI are joined into a single row for the MTI, with the
    character provided by \code{collapse.reports} being used as a
    separator. In that case the resulting data.frame contains only
    unique MTIs (one row for each MTI).}
  \item{stringsAsFactors}{whether character strings should be
    automatically converted to factors.}
  \item{row.names}{would allow to set the row.names of the data.frame.}
}
\details{
  This are helper functions that allow to transform data.frames into
  objects and \emph{vice versa}.
}
\value{
  A data.frame or a list of MTI objects.
}
\seealso{
  \code{\link{data.frame2report}}, \code{\link{report2data.frame}}
}
\author{
Johannes Rainer
}
\examples{

## get all miRNA target gene interactions (MTI) for BCL2L11 using a SQL
## query.
## Note, that we get the same MTI in multiple rows, if this MTI has been
## described in more than one publication:
con <- getMtiCon()
Res <- dbGetQuery( con, "select * from mirtarbase where target_gene='BCL2L11'" )

## the same MTI, but more than one publication.
Res[ Res$mirtarbase_id=="MIRT001206", c( "mirna", "target_gene", "references_pmid" ) ]

## create a list of MTI objects.
## Note that the reports are NOT added to the report slot of the MTI objects.
MTIs <- data.frame2mti( Res )
nrow( Res )
length( MTIs )
## MTIs contains only unique list of MTIs

## convert it again to a data.frame
DF <- do.call( "rbind", lapply( MTIs, mti2data.frame ) )
nrow( DF )

}
\keyword{ data }

