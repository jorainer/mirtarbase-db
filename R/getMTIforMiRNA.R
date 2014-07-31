### this will go in it's own thing...
## returns miRNA target interactions.
## x: miRNA id.
## type: mature_mirna: the ID of the mature miRNA (e.g. cel-let-7-5p)
##       mirna_id: the miRNA ID (e.g. hsa-mir-29b-1). Will return MTIs for both (all) mature miRNAs of the (double stranded) miRNA.
##       mirna_family: the ID of the miRNA family. Will return MTIs for all mature miRNAs of the miRNA family.
## operator: the operator to be used in the select sql call (either like or =).
## filter.mirna.species: restrict the results to miRNAs from the provided species (by default it matches all species in the database).
## filter.gene.species: restrict the results to target genes from the provided species (by default it matches all species in the database).
## filter.support.type: restrict results to MTI from provided evidence support types (by default it matches all support types).
## return.data.frame: returns a data.frame (like in the old version) instead of a list of MTIs.
## DEPRECATED: cleanup: whether NA rows should be removed from the results. By default the function returns at least one row for each mature miRNA.
getMtiForMiRNA <- function( x, type="mature_mirna", operator="like", filter.mirna.species=getAvailableSpecies( "mirna" ), filter.gene.species=getAvailableSpecies( "target_gene" ), filter.support.type=getSupportTypes(), return.data.frame=FALSE ){
    type <- match.arg( type, c( "mature_mirna", "mirna_id", "mirna_family" ) )
    ## have the mature miRNA IDs...
    if( type=="mature_mirna" ){
        xmat <- x
    }
    ## get all mature miRNA IDs for the miRNA family
    if( type=="mirna_family" ){
        cat( "retrieving all miRNA IDs for the submitted miRNA family ID(s).\n" )
        ## it it absolutely crazy. the mirbase.db package is so ..
        FAM <- toTable( mirbaseFAMILY )
        FAM <- FAM[ tolower( FAM[ , "id" ] ) %in% tolower( x ), , drop=FALSE ]
        notfound <- !( tolower( x ) %in% tolower( FAM[ , "id" ] ) )
        if( any( notfound ) ){
            warning( paste0( "No mature miRNA IDs for miRNA families ", paste( x[ notfound ], collapse = "," ), " found!" ) )
        }
        if( nrow( FAM )==0 ){
            stop( "No mature miRNA IDs found!" )
        }
        ## OK, I've got mirna ids, now I need the mature ones...
        x <- unique( FAM[ , "mirna_id" ] )
        type <- "mirna_id"
    }
    ## have to get the mature miRNA IDs for the miRNA.
    if( type=="mirna_id" ){
        ## map the mirna_id to the mature miRNA using mirbase.db package.
        cat( "Mapping mirna id(s) to mature miRNA ids.\n" )
        Mat <- mget( tolower( x ), envir=mirbaseMATURE, ifnotfound=NA )
        NAs <- unlist( lapply( Mat, class ) )=="logical"
        if( any( NAs ) ){
            notfound <- names( Mat )[ NAs ]
            warning( paste0( "No mature miRNA ID for miRNA(s) ", paste( notfound, collapes="," ), " found!" ) )
        }
        Mat <- Mat[ !NAs ]
        if( length( Mat )==0 ){
            stop( "No mature miRNA IDs found!" )
        }
        xmat <- unlist( lapply( Mat, matureName ) )
    }
    xmat <- sQuote( xmat )
    con <- getMtiCon( v=FALSE )
    ## building the filter rules...
    filter.mirna.string = paste0( " and species_mirna in (", paste( sQuote( filter.mirna.species ), collapse="," ), ")" )
    filter.gene.string = paste0( " and species_target_gene in (", paste( sQuote( filter.gene.species ), collapse="," ), ")" )
    filter.support.string = paste0( " and support_type in (", paste( sQuote( filter.support.type ), collapse = "," ), ")" )

    ## return the result as a data.frame:
    if( return.data.frame ){
        ## search for the MTIs
        Res <- sapply( xmat, FUN=dogetMtiForMiRNA, con=con, operator=operator, simplify=FALSE, USE.NAMES=FALSE, filterstring=paste0( filter.mirna.string, filter.gene.string, filter.support.string ) )
        Res <- do.call( what=rbind, Res )
        return( Res )
    }
    ## that's now what we want: a list of MTIs (with Reports inside).
    Res <- unlist( sapply( xmat, FUN=dogetMtiListForMiRNA, con=con, operator=operator, simplify=FALSE, USE.NAMES=FALSE, filterstring=paste0( filter.mirna.string, filter.gene.string, filter.support.string ) ) )
    return( Res )
}

## well, that's just the function that does the stuff...
dogetMtiForMiRNA <- function( x, con, operator="like", filterstring="" ){
    operator = match.arg( operator, c( "like", "=") )
    Res <- dbGetQuery( con, paste0( "select * from mirtarbase where mirna ", operator, " ", x, filterstring,  ";" ) )
    if( nrow( Res )==0 ){
        Res[ 1,  ] <- NA
    }
    Res <- cbind( query=gsub( x, pattern="'", replacement="" ), Res, stringsAsFactors=FALSE )
    return( Res )
}

## this function performs the query, but returns a list of MTI objects!
dogetMtiListForMiRNA <- function( x, con, operator="like", filterstring="" ){
    TheDataFrame <- dogetMtiForMiRNA( x=x, con=con, operator=operator, filterstring=filterstring )
    TheDataFrame <- split( TheDataFrame, f=TheDataFrame$mirtarbase_id ) ## this way we loose any NA columns!
    MTIs <- lapply( TheDataFrame, data.frame2mtiNreport )
    return( MTIs )
}



