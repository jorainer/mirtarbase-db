## that's the main function. get an MTI for a miRNA or a gene, or any other stuff.
getMti <- function( x, type="symbol", operator="like", filter.mirna.species=getAvailableSpecies( "mirna" ), filter.gene.species=getAvailableSpecies( "target_gene" ), filter.support.type=getSupportTypes(), return.data.frame=FALSE ){
    ## look up miRNAs
    mirna.types <- c( "mature_mirna", "mirna_id", "mirna_family" )
    gene.types <- c( "symbol", "entrezid" )
    other.types <- c( "mirtarbase_id", "experiments", "support_type", "references_pmid" )
    type <- match.arg( type, c( mirna.types, gene.types, other.types ) )
    ## miRNAs are a little tricky... will try to get the mature miRNA in any case.
    if( type=="mature_mirna" ){
        ## smooth sailing.
        query.column <- "mirna"
        query.x <- sQuote( x )
    }
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
    ## have to get the mature miRNA IDs for the (pre) miRNA.
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
        query.column <- "mirna"
        query.x <- sQuote( unique( unlist( lapply( Mat, matureName ) ) ) )
    }
    ## check the type, and select column to query.
    if( type=="symbol" ){
        query.column <- "target_gene"
        query.x <- sQuote( x )
    }
    if( type=="entrezid" ){
        query.column <- "target_gene_entrez_gene_id"
        operator <- "="
        query.x <- x
    }
    if( type=="mirtarbase_id" ){
        query.column <- "mirtarbase_id"
        query.x <- sQuote( x )
    }
    if( type=="experiments" ){
        query.column <- "experiments"
        query.x <- sQuote( x )
    }
    if( type=="support_type" ){
        query.column <- "support_type"
        query.x <- sQuote( x )
    }
    if( type=="references_pmid" ){
        query.column <- "references_pmid"
        operator <- "="
        query.x <- x
    }
    ## build filters, build query and run.
    filter.mirna.string = paste0( " and species_mirna in (", paste( sQuote( filter.mirna.species ), collapse="," ), ")" )
    filter.gene.string = paste0( " and species_target_gene in (", paste( sQuote( filter.gene.species ), collapse="," ), ")" )
    filter.support.string = paste0( " and support_type in (", paste( sQuote( filter.support.type ), collapse = "," ), ")" )
    ## connecting to the database
    con <- getMtiCon()
    ## just return a data.frame
    if( return.data.frame ){
        Res <- sapply( query.x, FUN=dogetMtiForWhatever, con=con, operator=operator, query.column=query.column, simplify=FALSE, USE.NAMES=FALSE, filterstring=paste0( filter.mirna.string, filter.gene.string, filter.support.string ) )
        Res <- do.call( what=rbind, Res )
        return( Res )
    }
    ## do the query:
    if( length( query.x ) > 20 ){
        Res <- unlist( mclapply( query.x, FUN=dogetMtiListForWhatever, con=mtiDB( v=FALSE ), query.column=query.column, operator=operator, filterstring=paste0( filter.mirna.string, filter.gene.string, filter.support.string ) ) )
    }else{
        Res <- unlist( sapply( query.x, FUN=dogetMtiListForWhatever, con=con, query.column=query.column, operator=operator, filterstring=paste0( filter.mirna.string, filter.gene.string, filter.support.string ), simplify=FALSE, USE.NAMES=FALSE ) )
    }
    return( Res )
}


dogetMtiForWhatever <- function( x, con, query.column="target_gene",  operator="like", filterstring="" ){
    operator <- match.arg( operator, c( "like", "=", "!=", "not like" ) )
    Res <- dbGetQuery( con, paste0( "select * from mirtarbase where ", query.column, " ", operator, " ", x, filterstring, ";" ) )
    ## should I add NA?
    return( Res )
}


dogetMtiListForWhatever <- function( x, con, query.column="target_gene", operator="like", filterstring="" ){
    TheDF <- dogetMtiForWhatever( x, con, query.column, operator, filterstring )
    ## we like to get a data.frame with all columns required to build an MTI and Report.
    TheDF <- split( TheDF, f=TheDF$mirtarbase_id )  ## we would also loose any NA rows.
    MTIs <- lapply( TheDF, data.frame2mtiNreport )
    return( MTIs )
}

### this generates a MTI with Report objects from a data.frame CONTAINING ONLY VALUES FOR ONE MTI!
data.frame2mtiNreport <- function( x, colname.pmid="references_pmid", colname.support="support_type", colname.experiments="experiments", split="//", colname.id="mirtarbase_id", colname.mirna="mirna", colname.mirna.species="species_mirna", colname.target="target_gene", colname.target.entrezid="target_gene_entrez_gene_id", colname.target.species="species_target_gene" ){
    ## first do the Reports.
    Reps <- data.frame2report( x, colname.pmid=colname.pmid, colname.support=colname.support, colname.experiments=colname.experiments, split=split, do.unique=TRUE )
    ## then the MTI
    if( length( unique( x[ , colname.id ] ) ) > 1 ){
        stop( "This function should not be called on a data.frame with more than one MTI!" )
    }
    MTI <- data.frame2mti( x, colname.id=colname.id, colname.mirna=colname.mirna, colname.mirna.species=colname.mirna.species, colname.target=colname.target, colname.target.entrezid=colname.target.entrezid, colname.target.species=colname.target.species, do.unique=TRUE )[[ 1 ]]
    ## fill the reports into MTI
    MTI@report <- Reps
    return( MTI )
}

