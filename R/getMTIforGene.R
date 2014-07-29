## that's the function to retrieve MTIs for a given gene (or more than one gene)
## Note: if type is entrezid, we're changing the operator to =
getMtiForGene <- function( x, type="symbol", operator="like", filter.mirna.species=getAvailableSpecies( "mirna" ), filter.gene.species=getAvailableSpecies( "target_gene" ), filter.support.type=getSupportTypes(), cleanup=FALSE ){
    type <- match.arg( type, c( "symbol", "entrezid" ) )
    gene_column <- "target_gene"
    if( type=="entrezid" ){
        gene_column <- "target_gene_entrez_gene_id"
        ## setting the operator to =
        operator <- "="
    }
    con <- getMtiCon( v=FALSE )
    ## building the filter rules...
    filter.mirna.string = paste0( " and species_mirna in (", paste( sQuote( filter.mirna.species ), collapse="," ), ")" )
    filter.gene.string = paste0( " and species_target_gene in (", paste( sQuote( filter.gene.species ), collapse="," ), ")" )
    filter.support.string = paste0( " and support_type in (", paste( sQuote( filter.support.type ), collapse = "," ), ")" )
    ## search for the MTIs
    Res <- sapply( x, FUN=dogetMtiForGene, con=con, operator=operator, simplify=FALSE, USE.NAMES=FALSE, filterstring=paste0( filter.mirna.string, filter.gene.string, filter.support.string ), type=type )
    Res <- do.call( what=rbind, Res )
    if( cleanup ){
        Res <- Res[ !is.na( Res$mirna ), ]
    }
    return( Res )
}

## well, that's just the function that does the stuff...
dogetMtiForGene <- function( x, con, operator="like", filterstring="", type="symbol" ){
    operator <- match.arg( operator, c( "like", "=") )
    type <- match.arg( type, c( "symbol", "entrezid" ) )
    what.col <- "target_gene"
    if( type=="entrezid" ){
        what.col <- "target_gene_entrez_gene_id"
    }else{
        x <- sQuote( x )
    }
    Query <- paste0( "select * from mirtarbase where ", what.col, " ", operator, " ", x, filterstring,  ";" )
    cat(Query)
    Res <- dbGetQuery( con, Query )
    ## that's just to ensure that we return something (actually NAs) in case we did not find something.
    if( nrow( Res )==0 ){
        Res[ 1,  ] <- NA
    }
    Res <- cbind( query=gsub( x, pattern="'", replacement="" ), Res, stringsAsFactors=FALSE )
    return( Res )
}


