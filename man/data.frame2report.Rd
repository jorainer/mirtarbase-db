\name{data.frame2report}
\alias{data.frame2report}
\alias{report2data.frame}
\title{
  Convert data.frames to Report classes and \emph{vice versa}
}
\description{
  These function allow to create Report objects from data.frames (one
  Report object per line) or to export the contents of a Report object
  as a (single row) data.frame.
}
\usage{
data.frame2report( x, colname.pmid="references_pmid", colname.support="support_type", colname.experiments="experiments", split="//", do.unique=TRUE )
report2data.frame( x, colname.pmid="references_pmid",
colname.support="support_type", colname.experiments="experiments",
split="//", stringsAsFactors=FALSE, row.names=NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    the data.frame or Report object.
  }
  \item{colname.pmid}{the column name in the data.frame providing the
    pubmed identifiers of the publication.}
  \item{colname.support}{the column name in the data.frame that provides
  the support type information from the miRTarBase.}
  \item{colname.experiments}{the column name in the data.frame
    containing the types of experiments that have been performed.}
  \item{split}{character that allow to split experiments in the
    experiments column. By default all experiments in a publication that
    provide evidence for the MTI are separated by a \dQuote{//}.}
  \item{do.unique}{by default, the function returns a unique list of
    Report objects, i.e. if the submitted data.frame contained several
    rows with the same entries for pmid, support and evidence, it will
    return only one Report object for these.}
  \item{stringsAsFactors}{whether character strings should be
    automatically converted to factors.}
  \item{row.names}{would allow to set the row.names of the data.frame.}
}
\details{
  This are helper functions that allow to transform data.frames into
  objects and \emph{vice versa}.
}
\value{
  A data.frame or a list of Report objects.
}
\seealso{
  \code{\link{data.frame2mti}}, \code{\link{mti2data.frame}}
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

## create a list of Report objects.
Reps <- data.frame2report( Res )

## convert it again to a data.frame
DF <- do.call( "rbind", lapply( Reps, report2data.frame ) )
nrow( DF )

}
\keyword{ data }

