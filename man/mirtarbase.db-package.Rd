\name{mirtarbase.db-package}
\alias{mirtarbase.db-package}
\alias{mirtarbase.db}
\docType{package}
\title{
  The miRTarBase database for validated miRNA target gene interactions
}
\description{
  This package provides the miRNA target gene interactions (MTI) from
  the miRTarBase database.
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
con <- getMtiCon()
dbGetQuery( con, "select count( * ) from mirtarbase;" )

## get a MTI for the gene BCL2L11, using an SQL query
Res <- dbGetQuery( con, "select * from mirtarbase where target_gene='BCL2L11'" )

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

## the "right" way to get MTIs:
MTIs <- getMti( "BCL2L11" )
length( MTIs )

## get only MTIs for human BCL2L11
MTIs <- getMti( "BCL2L11", filter.gene.species="Homo sapiens" )
length( MTIs )

## get the miRNAs that have been reported:
lapply( MTIs, matureMirna )
## the publications
lapply( MTIs, pmid )

## The reports for a selected MTI:
reports( MTIs[[ "MIRT001206" ]] )

## transform to a data.frame. Note that the a MTI can be present in more
## than one row, depending on the number of publications.
do.call( "rbind", lapply( MTIs, mti2data.frame ) )

## with collapse.report="," mutliple evidences per MTI will be pasted,
## so each line represents a unique MTI
do.call( "rbind", lapply( MTIs, mti2data.frame, collapse.reports=", " ) )

}
\keyword{ package }
