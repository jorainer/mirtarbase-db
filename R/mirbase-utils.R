## these are functions to enable some advanced use of the mirbase.db package.
getMirbaseCon <- function( ){
    Vars <- ls( pos=1, all.names=TRUE )
    if( any( Vars ==".mirbase.con" ) ){
        return( get( ".mirbase.con" ) )
    }else{
        #cat( "Connecting to database.\n" )
        con <- mirbase_dbconn( )
        assign( ".mirbase.con", con, pos=1 )
        return( con )
    }
}

getMirbaseForMature <- function( x, operator="=", tables=c( "mirna" ) ){
    operator <- match.arg( operator, c( "=", "!=", "like", "not like" ) )
    tables <- unique( c( "mirna", tables ) )  ## always need that...
    notSupported <- !(tables %in% c( "mirna", "mirna_prefam" ) )
    if( sum( notSupported ) > 0 ){
        warning( "Tables ", paste( notSupported, collapse=", " ), "not supported; have been removed." )
        tables <- tables[ !notSupported ]
    }
    if( length( x ) > 1 ){
        stop( "x should only be a single mature miRNA ID! Use mgetMirbaseForMature for more than one mature miRNA ID." )
        ## using in...
        ## return( dbGetQuery( getMirbaseCon(), paste0( "select * from ( select * from mirna_mature where mature_name in (", paste( sQuote( x ), collapse="," ), ") ) as tmp join mirna_pre_mature on tmp.auto_mature=mirna_pre_mature.auto_mature join mirna on mirna_pre_mature._id=mirna._id;" ) ) )
    }
    Query <- paste0( "select * from ( select * from mirna_mature where mature_name ", operator, " ", sQuote( x ), " ) as tmp join mirna_pre_mature on tmp.auto_mature=mirna_pre_mature.auto_mature join mirna on mirna_pre_mature._id=mirna._id" )
    if( any( tables=="mirna_prefam" ) ){
        Query <- paste0( Query, " join mirna_2_prefam on mirna._id=mirna_2_prefam._id join mirna_prefam on mirna_2_prefam.auto_prefam=mirna_prefam.auto_prefam" )
    }
    return( dbGetQuery( getMirbaseCon(), paste0( Query, ";" ) ) )
}

mgetMirbaseForMature <- function( x, operator="=", tables=c( "mirna" ), ifnotfound=NA ){
    return( sapply( x, simplify=FALSE, function( z ){
        tmp <- getMirbaseForMature( z, operator=operator, tables=tables )
        if( nrow( tmp )==0 ){
            return( ifnotfound )
        }
        return( tmp )
    } ) )
}

getMirbaseFamForMature <- function( x, operator="=" ){
    if( length( x ) > 1 ){
        stop( "x should only be a single mature miRNA ID! Use mgetMirbaseFamForMature for more than one mature miRNA ID." )
    }
    ## one more join than in getMirbaseForMature...
    return( getMirbaseForMature( x, operator=operator, tables=c( "mirna", "mirna_prefam" ) ) )
}

mgetMirbaseFamForMature <- function( x, operator="=", ifnotfound=NA ){
    return( sapply( x, simplify=FALSE, function( z ){
        tmp <- getMirbaseFamForMature( z, operator=operator )
        if( nrow( tmp )==0 ){
            return( ifnotfound )
        }
        return( tmp )
    } ) )
}

