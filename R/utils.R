## list all species available.
# x either for mirna or for target_gene
getAvailableSpecies <- function( x="target_gene" ){
    x <- match.arg( x, choices = c( "mirna", "target_gene" ) )
    con <- getMtiCon( v=FALSE )
    Res <- dbGetQuery( con, paste0( "select distinct species_", x, " from mirtarbase;" ) )
    return( Res[ ,1 ] )
}

getSupportTypes <- function( ){
    con <- getMtiCon( v=FALSE )
    Res <- dbGetQuery( con, paste0( "select distinct support_type from mirtarbase;" ) )
    return( Res[ ,1 ] )
}

getAvailableExperiments <- function( split="//" ){
    con <- getMtiCon( v=FALSE )
    Res <- dbGetQuery( con, paste0( "select distinct experiments from mirtarbase;" ) )
    Exps <- unique( unlist( strsplit( Res[ , 1 ], split=split ) ) )
    return( Exps )
}

getPmids <- function(  ){
    con <- getMtiCon( v=FALSE )
    return( dbGetQuery( con, paste0( "select distinct references_pmid from mirtarbase;" ) )[ , 1 ] )
}


## print some information...
mirtarbase <- function( prefix="" ){
    con <-getMtiCon()
    x <- system.file( "extdata/txt/INFO", package="mirtarbase.db" )
    #x <- "inst/extdata/txt/INFO"
    Lines <- readLines( x )
    for( i in 1:length( Lines ) ){
        cat( paste0( prefix, " ", Lines[ i ], "\n" ) )
    }
    ## number of MTIs:
    miRNAs <- dbGetQuery( con, "select count(distinct mirna) from mirtarbase;" )[ 1, 1 ]
    cat( paste0( prefix, " number of miRNAs: ", miRNAs, "\n" ) )
    ## number of genes:
    genes <- dbGetQuery( con, "select count( distinct target_gene ) from mirtarbase;" )[ 1, 1 ]
    cat( paste0( prefix, " number of target genes: ", genes, "\n" ) )
    ## MTI evidences:
    Tab <- dbGetQuery( con, "select support_type, count(*) as number_MTI from mirtarbase group by support_type" )
    for( i in 1:nrow( Tab ) ){
        cat( paste0( prefix, " ", Tab[ i, 1 ], ": ", Tab[ i, 2 ],"\n" ) )
    }
}
