\name{mirtarbase.db-package}
\alias{mirtarbase.db-package}
\alias{mirtarbase.db}
\docType{package}
\title{
  The miRTarBase database for validated miRNA target gene interactions
}
\description{
}
\details{

  The functions provided by this package are:

  Data analysis:

  \itemize{

    \item getAnnotation: retrieves an annotation data.frame for a given
    (Affymetrix based) \dQuote{ExpressionSet}. For more informations see
    \code{\link{getAnnotation}}.

    \item getAnnotationForIDs: retrieves a data.frame with annotations
    for the provided feature (probe set) ids. For more informations see \code{\link{getAnnotationForIDs}}.

    \item getRepPS: returns a representative probe set for a gene based
    on some criteria. For more informations see \code{\link{getRepPS}}.

    \item hyperGGeneric: performs a category analysis on any user
    provided definition of categories. For more informations see \code{\link{hyperGGeneric}}.

  }

  Utilities:

  \itemize{

    \item convertTxt2Xls: converts a text file with tabular content to a
    Microsoft Excel spreadsheet. For more informations see \code{\link{convertTxt2Xls}}.

    \item load.from.http: loads a RData file from a remote server
    (e.g. http or ftp server). For more informations see \code{\link{load.from.http}}.

    \item pastedmatch: value matching allowing e.g. \dQuote{;} separated
    values. For more informations see \code{\link{pastedmatch}}.

    \item read.table.from.http: loads and reads a table from a remote
  (e.g. http or ftp) server. For more information see
  \code{\link{read.table.from.http}}.
  }

}
\author{
Johannes Rainer

Maintainer: Johannes Rainer <johannes.rainer@eurac.edu>
}
\references{
http://bioinfo.i-med.ac.at
}
\examples{

## connect to the database:
library( mirtarbase.db )
source( "../R/MTI-class.R" )
con <- getMtiCon()
dbGetQuery( con, "select count( * ) from mirtarbase;" )

## get a MTI for the gene BCL2L11, using an SQL query
Res <- getQuery( con, "select * from mirtarbase where target_gene='BCL2L11'" )

## Report stuff
## generate Report class instances from that data.frame:
Reports <- data.frame2report( Res )
## get a data.frame listing all reports for that MTI.
Reports.df <- do.call( "rbind", lapply( Reports, report2data.frame ) )

## creating MTI objects from the data.frame. Note that the report slot
## of the MTI objects will be empty.
MTIs <- data.frame2mti( Res, do.unique=FALSE )
length( MTIs )
MTIs <- data.frame2mti( Res )
length( MTIs )
## note, we do have empty report slots in these MTIs, thus the corresponding data.frame columns will also be empty.
do.call( "rbind", lapply( MTIs, mti2data.frame ) )

}
\keyword{ package }
